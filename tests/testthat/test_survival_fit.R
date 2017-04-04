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

# test_that("getting survival inputs works",
#           {
#             ref_1 <- heemod:::read_file(system.file(
#               "tabular/test",
#               "survival_spec_1.csv",
#               package = "heemod"
#             ))
#             mixed_order <- heemod:::read_file(system.file(
#               "tabular/test",
#               "survival_spec_2.csv",
#               package = "heemod"
#             ))
#             ref_3 <- ref_1
#             ref_3$data <- gsub("_pfs", "_pfs2", ref_1$data)
#             input <- 
#               list(
#                 surv_data_dir = "survival_data",
#                 fit_files = c("OS.surv.fit", "PFS.surv.fit"),
#                 fit_names = c("OS.fit", "PFS.fit"),
#                 surv_data_files = c("OS.data.csv",
#                                     "PFS.data.csv"),
#                 fit_metric = "AIC",
#                 time_col_name = "time",
#                 censor_col_name = "status",
#                 treatment_col_name = "treatment",
#                 dists = c("exp", "weibull", "lnorm", "gamma", 
#                           "gompertz", "gengamma"),
#                 set_definitions = NULL
#               )
#             
#             expect_identical(get_survival_input(ref_1), input)
#             expect_identical(get_survival_input(mixed_order), input)
#             expect_identical(get_survival_input(ref_3), input)
#             
#             ref_error_1 <- read_file(system.file(
#               "tabular/test",
#               "survival_spec_error_1.csv",
#               package = "heemod"
#             ))
#             
#             
#             expect_error(get_survival_input(ref_error_1),
#                          "suffixes do not match")
#             
#             ref_error_2 <- read_file(system.file(
#               "tabular/test",
#               "survival_spec_error_2.csv",
#               package = "heemod"
#             ))
#             expect_error(get_survival_input(ref_error_2),
#                          "same number of elements")
# 
#             ref_error_3 <- ref_3
#             ref_error_3$data <- gsub("_pfs", "_os", ref_3$data)
#             expect_error(get_survival_input(ref_error_3),
#                          "must have each suffix OS and PFS once"
#                          )
#           }
# )

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


 # test_that("subsetting fit objects works",
 #           {
 #           fit_matrix <- 
 #             partitioned_survival_from_tabular(base_dir = system.file("tabular\\surv", 
 #                                                           package = "heemod"), 
 #                                               ref_file = "example_oncSpecs.csv", 
 #                                               df_env = new.env(), 
 #                                               state_names = c("ProgressionFree", "Progressive", 
 #                                               "Terminal", "Death"), 
 #                                              save_fits = FALSE)
 #           ## not concerned about the particular test, just
 #           ##   that it finishes as opposed to giving an error
 #           expect_equal(names(combine_part_surv(fit_matrix, 
 #                               A = list(pfs = "exp", os = "weibull"),
 #                               B = list(pfs = "exp", os = "lnorm"),
 #                              subset = "all")),
 #                          c("A", "B")
 #                          )
 #           
 #           
 #           expect_error(combine_part_surv(fit_matrix, 
 #                                          A = list(pfs = "exp", os = "weibull"),
 #                                          Z = list(pfs = "exp", os = "lnorm"),
 #                                          subset = "all"),
 #                        "column names of fit_matrix")
 #           expect_error(combine_part_surv(fit_matrix, 
 #                                          A = list(pfs = "exp", os = "weibul"),
 #                                          B = list(pfs = "exp", os = "lnorm"),
 #                                          subset = "all"), 
 #                        "row names of fit_matrix")
 #           expect_error(combine_part_surv(fit_matrix, 
 #                                          A = list(PFS = "exp", os = "weibull"),
 #                                          B = list(pfs = "exp", os = "lnorm"),
 #                                          subset = "all"), 
 #                        "only names os and pfs")
 #           })
 # 


