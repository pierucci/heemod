# heemod - Health Economic Evaluation MODelling

[![Travis-CI Build Status](https://travis-ci.org/pierucci/heemod.svg?branch=devel)](https://travis-ci.org/pierucci/heemod) [![](http://www.r-pkg.org/badges/version/heemod)](http://www.r-pkg.org/pkg/heemod) [![Coverage Status](https://img.shields.io/codecov/c/github/pierucci/heemod/devel.svg)](https://codecov.io/github/pierucci/heemod?branch=devel)

Toolset for Health Economic Evaluation Modelling for decision trees or cohort simulations. Provides a simple and consistent interface for Markov models specification, comparison, sensitivity and probabilistic analysis, input of survival models, etc. Models with time varying properties (non-homogeneous Markov models and semi-Markov models) are supported.

Most of the analyses presented in [Decision Modelling for Health Economic Evaluation](http://ukcatalogue.oup.com/product/9780198526629.do) can be performed with `heemod`. See `vignette("i-reproduction", "heemod")` for an exact reproduction of the analyses from the book.

You can install:

  * the latest released version from CRAN with:

```r
install.packages("heemod")
```

  * the latest development version from github with:

```r
devtools::install_github("pierucci/heemod@devel")
```

## Features

Main features:

  * Graphical user interface with `shiny`.
  * Accounting for time-dependency:
    * For both model time and state time.
    * Time-varying transition probabilities.
    * Time-varying values attached to states.
  * Probabilistic uncertainty analysis (PSA).
    * With correlated resampling.
    * Covariance analysis for PSA.
    * Expected value of perfect information (EVPI).
  * Deterministic sensitivity analysis (DSA).
  
Other features:
  
  * Multiple state membership correction methods (life-table, half-cycle, etc.).
  * Demographic analysis to compute population-level results.
  * Heterogeneity analysis.
  * Parallel computing support.
  * Features for budget impact analysis.

## Learning heemod

To get started read the introduction in `vignette("a-introduction", "heemod")`. Time-varying probabilities and values are explained in `vignette("b-time-dependency", "heemod")`.

Specific analysis examples (mostly inspired from [Decision Modelling for Health Economic Evaluation](http://ukcatalogue.oup.com/product/9780198526629.do)) can be found in the following vignettes:

  * Homogeneous Markov model in `vignette("c-homogeneous", "heemod")`.
  * Non-homogeneous Markov model in `vignette("d-non-homogeneous", "heemod")`.
  * Probabilistic uncertainty analysis in `vignette("e-probabilistic", "heemod")`.
  * Deterministic sensitivity analysis in `vignette("f-sensitivity", "heemod")`.
  * Heterogeneity & Demographic analysis in `vignette("g-heterogeneity", "heemod")`.
  * Running the models from tabular inputs in `vignette("h-tabular", "heemod")`.

## Devs

[Kevin Zarca](http://www.urc-eco.fr/Kevin-ZARCA,402) and [Antoine Filipović-Pierucci](https://pierucci.org).

<h1 align="center">
<a href="http://www.urc-eco.fr">
	<img width="220" src="./inst/media/logo.png" alt="">
</a>
</h1>

## Contributors

  * [Matthew Wiener](https://github.com/MattWiener)
  * [Zdenek Kabat](https://github.com/zkabat)
  * [Vojtech Filipec](https://github.com/vojtech-filipec)
