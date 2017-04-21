context("Survival analysis-to-Markov model tests")
library(flexsurv)

## simple data where half of participants
## die at time 10, rest are censored at time 20
test_dat <- data.frame(
  time = rep(c(10, 20), each = 50),
  status = rep(c(1, 0), each = 50),
  group = rep(1, 100)
)

test_fit <- flexsurv::flexsurvreg(Surv(time, status) ~ 1,
                                  data = test_dat,
                                  dist = "exp")
bad_test_fit <- test_fit
class(bad_test_fit) <- "random"

km_pred <- data.frame(time = 0, method = "km")

km_probs <- trans_probs_from_surv(test_fit,
                                  use_km_until = 100,
                                  markov_cycle = 1:20)

test_that("Kaplan-Meier probabilities are calculated correctly",
          {
            expect_equal(km_probs, rep(c(0, 0.5, 0),
                                       c(9, 1, 10)))
          })

test_that("input errors are caught", {
  expect_error(
    trans_probs_from_surv(list(),
                          use_km_until = 100,
                          markov_cycle = 1:20),
    '"dist_name" %in% names(dist_list)',
    fixed = TRUE
  )
  expect_error(
    trans_probs_from_surv(list(dist_name = "exp",
                               shape = 2, rate = 0.01),
                          use_km_until = 100,
                          markov_cycle = 1:20),
    "does not give the correct arguments for the distribution"
  )
  expect_error(
    trans_probs_from_surv(list(dist_name = "gamma",
                               shape = 2, random = 0.01),
                          use_km_until = 100,
                          markov_cycle = 1:20),
    "does not give the correct arguments for the distribution"
  )
  expect_error(
    trans_probs_from_surv(list(dist_name = "lnorm",
                               mean = 2, sdlog = 0.01),
                          use_km_until = 100,
                          markov_cycle = 1:20),
    "does not give the correct arguments for the distribution"
  )
  
  expect_error(
    trans_probs_from_surv(
      bad_test_fit,
      use_km_until = 10,
      markov_cycle = 1:20
    ),
    "no applicable method",
    fixed = TRUE
  )
  expect_error(
    trans_probs_from_surv(
      test_fit,
      use_km_until = 10,
      markov_cycle = 0:20
    ),
    "markov_cycle > 0"
  )
  expect_error(
    trans_probs_from_surv(
      test_fit,
      use_km_until = 10,
      markov_cycle = 1:20,
      markov_cycle_length = -1
    ),
    "markov_cycle_length > 0"
  )
  expect_error(
    trans_probs_from_surv(
      test_fit,
      use_km_until = -1,
      markov_cycle = 1:20
    ),
    "use_km_until",
    fixed = TRUE
  )

  data <- data.frame(time = rexp(100, rate = 0.01),
                     status = rep(1, 100),
                     group = rep(1, 100))
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
        param <- define_parameters(tr = trans_probs_from_surv(fit, km_until, markov_cycle))
        
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


