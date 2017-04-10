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


