# heemod 0.3.1

## Changes

  * a single set of parameters is now used for a set of models.

# Bug fixes

  * correct error in probabilistic analysis vignette.
  * fix incompatibility with upcoming dplyr update.

# heemod 0.3.0

## Enhancements

  * `shiny` interface.
  * added support for different counting options.
  * `get_who_mr()` to extract mortality rates from WHO data.

# heemod 0.2.0

## Enhancements

  * added ability to plot matrix.
  * added framework for probabilistic uncertainty analysis.
  * written vignette for probabilistic and sensitivity analysis.
  * added complement alias for matrix definition.
  * `run_*` functions now output a single table instead of a list of tables.
  * variables corresponding to cost and effect must now be specified in `run_models()`.
  * plotting for sensitivity and probabilistic analysis.
  
## Bug fixes

  * added `*_` functions.
  
## Removed

  * `run_model_newdata()` cannot run `uneval_model` anymore.
  * removed need to use `define_state_list()`.

# heemod 0.1.0

  * intial CRAN submission.
