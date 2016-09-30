context("running with multiple cores")
library(parallel)

test_that(
  "Same results using 1 core or 2.", {

    result_1core <- run_models_tabular(
      location = system.file("tabular/thr", package = "heemod"),
      save = FALSE, overwrite = FALSE, run_psa = FALSE 
    )
  
    result_2core <- run_models_tabular(
      location = system.file("tabular/thr", package = "heemod"),
      reference = "REFERENCE_2core.csv",
      save = FALSE, overwrite = FALSE, run_psa = FALSE
    )
    ## the objects contain environments, so we can't use identical;
    ##   instead, we'll check parts that use multi-core processing
    ## 
    ##  parameters from the discrete sensitivity analysis
    expect_identical(result_1core$dsa$.par_value,
                     result_2core$dsa$.par_value)
    ##  counts from all models from the discrete sensitivity analysis
    expect_identical(lapply(result_1core$dsa$.mod, get_counts),
                     lapply(result_2core$dsa$.mod, get_counts))
    ## demographic analysis
    expect_equal(result_1core$demographics,
                 result_2core$demographics)
    ## remove attributes (which contain environments)
    expect_identical(data.frame(result_1core$demographics),
                     data.frame(result_2core$demographics))
    
  }
)