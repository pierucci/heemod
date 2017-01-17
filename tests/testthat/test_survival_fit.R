context("fitting survival models")

data <- data.frame(time = rexp(100, rate = 0.01),
                   status = rep(1, 100),
                   group = rep(1, 100))

test_that("input errors are caught", {
  expect_error(
  f_fit_survival_models(
    data,
    dist,
    time_col_name = "time2",
    censor_col_name = "status",
    treatment_col_name = "group",
    fit_indiv_groups = F
  ),
  "time_col_name"
)
expect_error(
  f_fit_survival_models(
    data,
    dist,
    time_col_name = "time",
    censor_col_name = "status2",
    treatment_col_name = "group",
    fit_indiv_groups = F
  ),
  "censor_col_name"
)
expect_error(
  f_fit_survival_models(
    data,
    dist,
    time_col_name = "time",
    censor_col_name = "status",
    treatment_col_name = "group2",
    fit_indiv_groups = F
  ),
  "treatment_col_name"
)

expect_error(
  f_fit_survival_models(
    as.list(data),
    dist,
    time_col_name = "time",
    censor_col_name = "status",
    treatment_col_name = "group2",
    fit_indiv_groups = F
  ),
  "is.data.frame"
)

expect_error(
  f_fit_survival_models(
    data[0, ],
    dist,
    time_col_name = "time",
    censor_col_name = "status",
    treatment_col_name = "group2",
    fit_indiv_groups = F
  ),
  "nrow(survdata)", fixed = TRUE
)



})

test_that("getting survival inputs works",
          {
            ref_1 <- heemod:::read_file(system.file(
              "tabular/test",
              "survival_spec_1.csv",
              package = "heemod"
            ))
            mixed_order <- heemod:::read_file(system.file(
              "tabular/test",
              "survival_spec_2.csv",
              package = "heemod"
            ))
            ref_3 <- ref_1
            ref_3$data <- gsub("_pfs", "_pfs2", ref_1$data)
            input <- 
              list(
                surv_data_dir = "survival_data",
                fit_files = c("OS.surv.fit", "PFS.surv.fit"),
                fit_names = c("OS.fit", "PFS.fit"),
                surv_data_files = c("OS.data.csv",
                                    "PFS.data.csv"),
                fit_metric = "AIC",
                time_col_name = "time",
                censor_col_name = "status",
                treatment_col_name = "treatment",
                dists = c("exp", "weibull", "lnorm", "gamma", 
                          "gompertz", "gengamma")
              )
            
            expect_identical(get_survival_input(ref_1), input)
            expect_identical(get_survival_input(mixed_order), input)
            expect_identical(get_survival_input(ref_3), input)
            
            ref_error_1 <- read_file(system.file(
              "tabular/test",
              "survival_spec_error_1.csv",
              package = "heemod"
            ))
            
            
            expect_error(get_survival_input(ref_error_1),
                         "suffixes do not match")
            
            ref_error_2 <- read_file(system.file(
              "tabular/test",
              "survival_spec_error_2.csv",
              package = "heemod"
            ))
            expect_error(get_survival_input(ref_error_2),
                         "same number of elements")

            ref_error_3 <- ref_3
            ref_error_3$data <- gsub("_pfs", "_os", ref_3$data)
            expect_error(get_survival_input(ref_error_3),
                         "must have each suffix OS and PFS once"
                         )
          }
)



#set seed
set.seed(5001)

#define distributions to test
dist_fit = c("exp", "weibull", "lnorm", "gamma", "gompertz", "llogis")
params <- list(
  exp = c(rate = 1 / 50),
  weibull = c(shape = 1, scale = 50),
  lnorm = c(meanlog = log(50), sdlog = 0.75),
  gamma = c(shape = 1, rate = 1 / 50),
  gompertz = c(shape = 1, rate = 1 / 50),
  llogis = c(shape = 3, scale = 50)
)

for (i in 1:length(dist_fit))
{
  dist = dist_fit[i]
  
  #testthat instance per distribution
  test_that(paste(
    "Survival fit matches well with Markov-based simulations for ",
    dist,
    sep = ""
  ),
  {
    #define number of patients
    for (n_patients in c(100, 10000)) {
      args_list <- as.list(c(n = n_patients, params[[dist]]))
      event.time <- do.call(paste0("r", dist),
                            args_list)
      
      #set-up censor time
      censor.time = rep(10000, n_patients)
      
      #get censor status
      obs.time = pmin(event.time, censor.time)
      status = as.numeric(obs.time == event.time)
      
      #set-up simulated data as belonging to one patient group
      group = rep(1, n_patients)
      data = data.frame(time = event.time,
                        status = status,
                        group = group)
      
      #get max.time
      max.time = floor(max(data$time))
      
      #fit survival distribution
      fit = f_fit_survival_models(
        data,
        dist,
        time_col_name = "time",
        censor_col_name = "status",
        treatment_col_name = "group",
        fit_indiv_groups = F
      )[[1]]
      
      # generate KM estimates
      mf <- stats::model.frame(fit)
      Xraw <- mf[, attr(mf, "covnames.orig"), drop = FALSE]
      dat <- fit$data
      isfac <- sapply(Xraw, is.factor)
      mm <-  as.data.frame(stats::model.matrix(fit))
      form <-
        "survival::Surv(dat$Y[,\"start\"],dat$Y[,\"stop\"],dat$Y[,\"status\"]) ~ "
      
      form <- paste(form,
                    if (fit$ncovs > 0 && all(isfac))
                      paste("mm[,", 1:fit$ncoveffs, "]", collapse = " + ")
                    else
                      1)
      form <- stats::as.formula(form)
      km <-
        summary(survival::survfit(form, data = mm), times = c(0:max.time))
      
      # generate fit prediction estimates
      fit_pred = summary(fit, t = c(0:max.time))[[1]]$est
      
      ## correctness of fitting of survival function
      ## check that true value is within 95% confidence interval
      ## note there's actually a potential problem with multiple
      ##   testing here, which we might need to deal with at some point
      for (this_param in names(params[[dist]])) {
        lower <- fit$res[this_param, 2]
        upper <- fit$res[this_param, 3]
        true_val <- params[[dist]][this_param]
        ## print(c(lower, true_val, upper))
        expect_gt(true_val - lower, 0)
        expect_gt(upper - true_val, 0)
      }  # end loop around different parameters
      
      ## simulate Markov model and compare against survival fit data
      #loop over km_pred definitions
      km_until_vals <- list(0, 5000)
      for (km_until in km_until_vals)
      {
        #define parameters for Markov model
        param <- 
          define_parameters(tr = get_probs_from_surv(fit, 
                                                     km_limit = km_until, 
                                                     cycle = markov_cycle))
        
        #define transition matrix for Markov model
        mat_trans <- define_transition(state_names = c("Alive", "Dead"),
                                       1 - tr, tr,
                                       0, 1)
        
        #define Markov model with transition matrix and state assignments
        mod <- define_strategy(
          transition = mat_trans,
          Alive = define_state(utility = 0, cost = 0),
          Dead = define_state(utility = 0, cost = 0)
        )
        
        #run model over the same time period a survival simulation data (365 days)
        res_mod <- run_model(
          mod = mod,
          parameters = param,
          cycles = max.time + 1,
          cost = cost,
          effect = utility,
          method = "end"
        )
        
        #get predicted results from either KM estimate or fitted distribution
        preds <- rep(NA, max.time + 1)
        if(km_until > 0)
          km_pred <-
          data.frame(time = c(0, km_until),
                     method = c("km", "pred"))
        else
          km_pred <- data.frame(time = 0, method = "pred")
        
        type <- look_up(km_pred,
                        time = c(1:(max.time + 1)),
                        bin = "time",
                        value = "method")
        use_km <- type %in% c("KM", "km")
        use_pred <- type == "pred"
        if (any(use_km)) {
          preds[use_km] = km$surv[use_km]
        }
        if (any(use_pred)) {
          preds[use_pred] = fit_pred[use_pred]
        }
        
        #get counts from Markov model simulation
        mm <-  subset(get_counts(res_mod), state_names == "Alive")$count / 1000
        
        #expect equal
        expect_equal(preds, mm)
      }
    }  # end loop around n_patients
  })
}  # end loop around distributions


