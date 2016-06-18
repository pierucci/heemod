# heemod - Health Economic Evaluation MODelling

[![Travis-CI Build Status](https://travis-ci.org/pierucci/heemod.svg?branch=master)](https://travis-ci.org/pierucci/heemod) [![](http://www.r-pkg.org/badges/version/heemod)](http://www.r-pkg.org/pkg/heemod) [![Coverage Status](https://img.shields.io/codecov/c/github/pierucci/heemod/master.svg)](https://codecov.io/github/pierucci/heemod?branch=master)

`heemod` is an `R` toolset for health economic evaluation modelling. It aims to provide a simple and consistent interface for Markov models specification and comparison. Non-homogeneous Markov models (with time varying properties) are supported.

Most of the analyses presented in [Decision Modelling for Health Economic Evaluation](http://ukcatalogue.oup.com/product/9780198526629.do) can be performed with `heemod`.

This package is still a *beta* version.

You can install:

  * the latest released version from CRAN with:

```r
install.packages("heemod")
```

  * the latest development version from github with:

```r
devtools::install_github("pierucci/heemod")
```

## Main features

  * Graphical user interface with `shiny`.
  * Time-varying transition probabilities.
  * Time-varying values attached to states.
  * Heterogeneity analysis.
  * Probabilistic uncertainty analysis.
  * Deterministic sensitivity analysis.
  * Multiple state membership correction methods (life-table, half-cycle...).

## Learning heemod

To get started read the intro vignette (or `vignette("introduction", package = "heemod")`).

Specific analysis examples (mostly inspired from Decision Modelling for Health Economic Evaluation) can be found in the following vignettes:

  * Homogeneous Markov model (or `vignette("homogeneous", package = "heemod")`).
  * Non-homogeneous Markov model (or `vignette("non-homogeneous", package = "heemod")`).
  * Probabilistic uncertainty analysis in `vignette("probabilistic", package = "heemod")`.
  * Deterministic sensitivity analysis in `vignette("sensitivity", package = "heemod")`.
  * See vignette `vignette("reproduction", package = "heemod")` for an exact reproduction of the analyses from the book.

## Future developments

Upcoming vignettes:

  * Heterogeneity analysis in `vignette("heterogeneity", package = "heemod")`.

## Devs

[Kevin Zarca](http://www.urc-eco.fr/Kevin-ZARCA,402) and [Antoine Filipović-Pierucci](https://pierucci.github.io/).
