# heemod - Health Ecomomic Evaluation MODelling

[![Travis-CI Build Status](https://travis-ci.org/pierucci/heemod.svg?branch=master)](https://travis-ci.org/pierucci/heemod)

`heemod` is an `R` toolset for health ecomomic evaluation modelling. It aims to provide a simple and consistent interface for Markov models specification and comparison. Non-homogenous Markov models (with time varying properties) are supported.

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

  * Time-varying transition probabilities.
  * Time-varying values attached to states.
  * Heterogeneity analysis.
  * Probabilistic incertitude analysis.
  * Deterministic sensitivity analysis.

## Learning heemod

To get started read the [intro vignette](https://cran.r-project.org/web/packages/heemod/vignettes/introduction.html) (or `vignette("introduction", package = "heemod")`).

Specific analysis examples (mostly inspired from Decision Modelling for Health Economic Evaluation) can be found in the following vignettes:

  * [Homogenous Markov model](https://cran.r-project.org/web/packages/heemod/vignettes/homogenous.html) (or `vignette("homogenous", package = "heemod")`).
  * [Non-homogenous Markov model](https://cran.r-project.org/web/packages/heemod/vignettes/non-homogenous.html) (or `vignette("non-homogenous", package = "heemod")`).
  * Probabilistic incertitude analysis in `vignette("probabilistic", package = "heemod")`.
  * Deterministic sensitivity analysis in `vignette("sensitivity", package = "heemod")`.

## Future developments

In version 1.0.0 :

  * Multiple state membership correction methods.

Upcoming vignettes:

  * Heterogeneity analysis in `vignette("heterogeneity", package = "heemod")`.

## Devs

[Kevin Zarca](http://www.urc-eco.fr/Kevin-ZARCA,402) and [Antoine Filipović-Pierucci](https://pierucci.github.io/).
