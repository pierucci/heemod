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

test_that("reading set definitions works",
          {
            example_1 <- 
              get_set_definitions(system.file("tabular/surv", package = "heemod"),
                                  "set_definitions_1.csv")
            expect_identical(example_1,
                             data.frame(treatment = rep(c("fake_treatment", "not_real"),2),
                                        set_name = rep(c("all", "time.gt.100"), each = 2),
                                        condition = rep(c("TRUE", "time > 100"), each = 2),
                                        subtract_time = rep(as.integer(c(NA, 100)), each = 2),
                                        stringsAsFactors = FALSE
                                        )
                             )
            example_2 <-
              get_set_definitions(system.file("tabular/surv", package = "heemod"),
                                  "set_definitions_2.csv")
            expect_identical(example_2,
                             data.frame(treatment = "fake_treatment",
                                        set_name = "all", 
                                        condition = "TRUE",
                                        stringsAsFactors = FALSE))
            expect_error(get_set_definitions(system.file("tabular/surv", package = "heemod"),
                                             "set_definitions_error_1.csv"),
                         "set_definitions file missing column(s): treatment, set_name",
                         fixed = TRUE)
          }
          )


test_that("getting survival inputs works",
          {
           ok_surv_info <- 
             read_file(system.file("tabular/surv/survival_info.csv", 
                                   package = "heemod"))
           check_survival_specs(ok_surv_info)
           for(i in 1:ncol(ok_surv_info)){
             expect_error(check_survival_specs(ok_surv_info[, -i]),
                          "missing names")
             }
           surv_info_extra <- ok_surv_info
           surv_info_extra$extra_col <- 5
           expect_error(check_survival_specs(surv_info_extra),
                        "extra names")
           expect_error(check_survival_specs(surv_info_extra),
                        "extra_col")
           surv_info_extra_row <- ok_surv_info[c(1, 1:nrow(ok_surv_info)),]
           expect_error(check_survival_specs(surv_info_extra_row),
                        "exactly one PFS and one OS entry")
           surv_info_dup <- ok_surv_info
           surv_info_dup[1, c("fit_directory", "fit_file", "time_col", "censor_col")] <- 
             surv_info_dup[2, c("fit_directory", "fit_file", "time_col", "censor_col")]
           expect_error(check_survival_specs(surv_info_dup),
                        "same time column and censoring column")
           surv_info_wrong_type <- ok_surv_info
           surv_info_wrong_type[1, "type"] <- "oops"
           expect_error(check_survival_specs(surv_info_wrong_type),
                        "exactly one PFS and one OS entry")
           surv_info_na <- ok_surv_info
           surv_info_na[1, "time_col"] <- NA
           expect_error(check_survival_specs(surv_info_na),
                        "all elements of surv_specs must be filled in")
           
           }
          )

test_that("fitting works (including with subsets)",
          {
            location <- system.file("tabular/surv", package = "heemod")
            ok_surv_info <- 
              heemod:::read_file(system.file("tabular/surv/survival_info.csv", 
                                    package = "heemod"))
            these_fits <- 
              heemod:::survival_from_data(location = location,
                                 survival_specs = ok_surv_info,
                                 dists = c("exp", "weibull"),
                                 save_fits = FALSE,
                                 use_envir = new.env())
            expect_identical(names(these_fits), c("", "env"))
            expect_identical(names(these_fits[[1]]),
                             c("type", "treatment", "set_name",
                               "dist", "fit", "set_def",
                               "time_subtract"))
            expect_identical(these_fits[[1]]$dist,
                             rep(c("exp", "weibull", "km"), 10))
            expect_identical(sapply(these_fits[[1]]$fit, class),
                             c(rep(c("flexsurvreg", "flexsurvreg", "survfit",
                                   "surv_shift", "surv_shift", "surv_shift"), 
                                 3),
                             c("flexsurvreg", "flexsurvreg", "survfit",
                               "flexsurvreg", "flexsurvreg", "survfit",
                               "surv_shift", "surv_shift", "surv_shift",
                               "flexsurvreg", "flexsurvreg", "survfit")))
            combos <- table(these_fits[[1]][, c("treatment", "set_def")])
            ## sorting to make sure things are in right order for tests -
            ##   otherwise sometimes had problems with different locales
            combos <- combos[c("A", "B"),]
            combos <- combos[, c("biomarker > 0.5","time > 50", "TRUE")]
            expect_identical(dimnames(combos),
                             list(treatment = c("A", "B"),
                                  set_def = c("biomarker > 0.5","time > 50", "TRUE")))
            expect_identical(as.numeric(combos),
                             c(0, 6, 6, 6, 6, 6))
            metrics <- extract_surv_fit_metrics(these_fits[[1]])
            expect_identical(names(metrics),
                             c("type", "treatment", "set_name", "dist", "fit",
                               "set_def", "time_subtract", "AIC", "BIC", "m2LL"))
            expect_equal(nrow(metrics), 20)
            expect_identical(round(metrics[1, c("AIC", "BIC", "m2LL")], 3),
                             tibble::tribble(~AIC, ~BIC, ~m2LL,
                                             494.77, 496.662, 492.77)
            
                             )
            ## make sure it works calling with just one row
            metrics <- extract_surv_fit_metrics(these_fits[[1]][1,])
            expect_identical(round(metrics[, c("AIC", "BIC", "m2LL")], 3),
                             tibble::tribble(~AIC, ~BIC, ~m2LL,
                                             494.77, 496.662, 492.77)
                             )
          }
          )

test_that("we handle fitting errors",
          {
            ## some data that causes an error when
            ##  fitting with generalized gamma
            this_dat <- 
              tibble::tribble(
              ~time, ~event, ~trt,
              251,0,"A",
              250,0,"A",
              91,1,"A",
              173,1,"A",
              467,0,"A",
              382,0,"A",
              247,1,"A",
              8,1,"A",
              291,1,"A",
              49,1,"A",
              1,1,"A",
              2,1,"A",
              259,0,"A",
              288,0,"A",
              212,0,"A",
              45,1,"A",
              9,1,"A",
              257,0,"A",
              63,1,"A",
              69,1,"A",
              338,0,"A",
              292,0,"A",
              248,1,"A",
              1,1,"A",
              1,1,"A",
              73,1,"A",
              223,0,"A",
              1,1,"A",
              2,1,"A",
              378,0,"A",
              464,0,"A",
              295,0,"A",
              40,1,"A",
              2,1,"A",
              44,1,"A"
            )
            suppressMessages(
            fit_tib <- f_fit_survival_models(this_dat, 
                                         dist = c("exp", "weibull", "gengamma"),
                                         time_col_name = "time", 
                                         censor_col_name = "event", 
                                         treatment_col_name = "trt",
                                         fit_indiv_groups = FALSE)
            )
            expect_equal(sapply(fit_tib$fit, class),
                         c("flexsurvreg", "flexsurvreg", 
                           "try-error", "survfit")
                         )
            }
          )


