context("Multiple cores")
library(parallel)

test_that(
  "Same results using 1 core or 2.", {

    result_1core <- run_model_tabular(
      location = system.file("tabular/thr", package = "heemod"),
      save = FALSE, overwrite = FALSE, run_psa = FALSE 
    )
  
    result_2core <- run_model_tabular(
      location = system.file("tabular/thr", package = "heemod"),
      reference = "REFERENCE_2core.csv",
      save = FALSE, overwrite = FALSE, run_psa = FALSE
    )
    ## the objects contain environments, so we can't use identical;
    ##   instead, we'll check parts that use multi-core processing
    ## 
    ##  parameters from the discrete sensitivity analysis
    expect_identical(result_1core$dsa$dsa$.par_value,
                     result_2core$dsa$dsa$.par_value)
    ##  counts from all models from the discrete sensitivity analysis
    expect_identical(get_counts(get_model(result_1core$dsa)),
                     get_counts(get_model(result_2core$dsa)))
    ## demographic analysis
    expect_equal(result_1core$demographics$updated_model,
                 result_2core$demographics$updated_model)
  }
)
