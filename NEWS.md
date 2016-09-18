# heemod devel

## New features

  * `define_sensitivity()` now accepts any expression as input, and can call references to model parameteres.
  * Discount rates can now be specified as parameters (allows for rates to be modified in DSA & PSA).
  
## Bugfixes

  * Edges where P=0 are not plotted anymore for transition matrices.

# heemod 0.4.0

## New features

  * `update()` for heterogeneity analysis and to compute population-level values, with vignette.
  * `run_models_tabular()` to import models from tabular input, with vignette.
  * `look_up()` to look up values from external data.
  * Added option to pool female and male mortality rates in WHO data.
  * Counting method now defaults to life-table.
  
## Enhancements

  * `plot_sensitivity()` now plots by default the widest bar on top.
  * Convenience functions for converting rates to probabilities.
  * Models can be run without state values, to compute counts only.
  * Much more informative error messages.
  * Objects can be converted to the `R` code to generate them (same idea as `dput()`, but easier to read).
  * New options `heemod.verbose` and `heemod.memotime`.
  * More informative messages, especially in verbose mode.
  * Use WHO data cached localy in case of connection problems.
  * New functions: `get_counts()` and `get_init()` to get state membership counts.
  * Smart sex code conversion for `get_who_mr()`.

## Bug fixes

  * _really_ fixed problem when the argument to `discount()` was not defined as a parameter.
  
## Changes

  * `eval_model_newdata()`, the function behind resampling and sensitivity analysis now returns list-variables.
  
## Acknowledments

  * Thanks to [Matthew Wiener](https://github.com/MattWiener), [Zdenek Kabat](https://github.com/ZdenekKabat) and [Vojtech Filipec](https://github.com/vojtech-filipec) for their great contributions to this update.

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
