# heemod devel

## New features

  * Models can be run from tabular input (see `vignette("tabular", "heemod")`).
  * New options `heemod.verbose` and `heemod.memotime`.
  * More informative messages, especially in verbose mode.
  * Added option to pool female and male mortality rates in WHO data.
  * Use WHO data cached localy in case of connection problems.
  * New functions: `get_counts()` and `get_init()`.
  * Smart sex conversion for `get_who_mr()`.

# heemod 0.4.0

## New features

  * `run_demographics()` to compute population-level values, with vignette.
  * `run_heterogeneity()` for heterogeneity analysis, with vignette.
  * `run_models_tabular()` to import models from tabular data, with vignette.
  * `look_up()` to look up values in external data.
  
## Enhancements

  * `plot_sensitivity()` now plots by default the widest bar on top (thanks to @MattWiener).
  * convenience functions for converting rates to probabilities.
  * models can be run without state values, to compute counts.
  * much more informative error messages.
  * objects can be converted to the `R` code to generate them (same idea as `dput()`, but easier to read).

## Bug fixes

  * _really_ fixed problem when argument to `discount()` was not defined as a parameter.
  
## Changes

  * `eval_model_newdata()`, the function behind resampling and sensitivity analysis now returns list-variables in order to prepare the heterogeneity/demographic analysis update.

# heemod 0.3.3

## New features

  * added a vignette to exactly reproduce results from Decision Modelling for Health Economic Evaluation.

## Bug fixes

  * fixed problem when argument to `discount()` was not defined as a parameter.
  * corrected several errors in the vignettes (thanks to Michael Schenkenberg from SBU, Stockholm, Sweden).
  * updated mortality rate tests to reflect GHO database update.

# heemod 0.3.2

## Bug fixes

  * fix mishandling of matrix index with `C` in `eval_matrix()` (thanks to @MattWiener).
  * fix problem with upcoming version of `tidyr`.

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
