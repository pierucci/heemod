# heemod - Health Economic Evaluation MODelling

[![Travis-CI Build Status](https://travis-ci.org/pierucci/heemod.svg?branch=devel)](https://travis-ci.org/pierucci/heemod) [![](http://www.r-pkg.org/badges/version/heemod)](http://www.r-pkg.org/pkg/heemod) [![Coverage Status](https://img.shields.io/codecov/c/github/pierucci/heemod/devel.svg)](https://codecov.io/github/pierucci/heemod?branch=devel)

`heemod` is an `R` toolset for health economic evaluation modelling. It aims to provide a simple and consistent interface for Markov models specification and comparison (for applications such as decision trees or cohort simulation). Non-homogeneous Markov models (with time varying properties) are supported.

Most of the analyses presented in [Decision Modelling for Health Economic Evaluation](http://ukcatalogue.oup.com/product/9780198526629.do) can be performed with `heemod`. See `vignette("i-reproduction", "heemod")` for an exact reproduction of the analyses from the book.

This package is still a *beta* version.

You can install:

  * the latest released version from CRAN with:

```r
install.packages("heemod")
```

  * the latest development version from github with:

```r
devtools::install_github("pierucci/heemod@devel")
```

## Main features

  * Graphical user interface with `shiny`.
  * Time-varying transition probabilities.
  * Time-varying values attached to states.
  * Heterogeneity analysis.
  * Probabilistic uncertainty analysis.
  * Deterministic sensitivity analysis.
  * Multiple state membership correction methods (life-table, half-cycle...).
  * Demographic analysis to compute population-level results.
  * Heterogeneity analysis.

## Learning heemod

To get started read the introduction in `vignette("a-introduction", "heemod")`.

Specific analysis examples (mostly inspired from [Decision Modelling for Health Economic Evaluation](http://ukcatalogue.oup.com/product/9780198526629.do)) can be found in the following vignettes:

  * Homogeneous Markov model in `vignette("b-homogeneous", "heemod")`.
  * Non-homogeneous Markov model in `vignette("c-non-homogeneous", "heemod")`.
  * Probabilistic uncertainty analysis in `vignette("d-probabilistic", "heemod")`.
  * Deterministic sensitivity analysis in `vignette("e-sensitivity", "heemod")`.
  * Heterogeneity & Demographic analysis in `vignette("g-heterogeneity", "heemod")`.
  * Run the models from tabular inputs in `vignette("h-tabular", "heemod")`.

## Devs

[Kevin Zarca](http://www.urc-eco.fr/Kevin-ZARCA,402) and [Antoine Filipović-Pierucci](https://pierucci.github.io/).

<h1 align="center">
<a href="http://www.urc-eco.fr">
	<img width="220" src="./inst/media/logo.png" alt="">
</a>
</h1>

## Contributors

  * [Matthew Wiener](https://github.com/MattWiener)
  * [Zdenek Kabat](https://github.com/ZdenekKabat)
  * [Vojtech Filipec](https://github.com/vojtech-filipec)
