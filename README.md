# heemod - Health Economic Evaluation MODelling

[![Travis-CI Build Status](https://travis-ci.org/pierucci/heemod.svg?branch=devel)](https://travis-ci.org/pierucci/heemod) [![](http://www.r-pkg.org/badges/version/heemod)](http://www.r-pkg.org/pkg/heemod) [![Coverage Status](https://img.shields.io/codecov/c/github/pierucci/heemod/devel.svg)](https://codecov.io/github/pierucci/heemod?branch=devel)

Markov Models for Health Economic Evaluations. An implementation of the modelling and reporting features described in reference textbooks and guidelines: deterministic and probabilistic sensitivity analysis, heterogeneity analysis, time dependency on state-time and model-time (semi-Markov and non-homogeneous Markov models), etc.

You can install:

  * the latest released version from CRAN with:

```r
install.packages("heemod")
```

  * the latest development version from github with:

```r
devtools::install_github("pierucci/heemod")
```

  * `heemod` can be cited with:
  
Filipović-Pierucci A, Zarca K and Durand-Zaleski I (2017).
[“Markov Models for Health Economic Evaluation: The R
Package heemod.”](https://arxiv.org/abs/1702.03252) _ArXiv e-prints_. R package version
0.8.0, 1702.03252, <URL: https://pierucci.org/heemod>.

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
  
  * Multiple state membership correction methods (life-table, custom method, etc.).
  * Demographic analysis to compute population-level results.
  * Heterogeneity analysis.
  * Parallel computing support.
  * Features for budget impact analysis.
  * Interface with [SAVI](http://savi.shef.ac.uk/SAVI/) and [BCEA](https://sites.google.com/a/statistica.it/gianluca/bcea).

Most of the analyses presented in [Decision Modelling for Health Economic Evaluation](http://ukcatalogue.oup.com/product/9780198526629.do) can be performed with `heemod`. See the *Reproducing Exact Results from DMHEE* vignette for an exact reproduction of the analyses from the book.

## Learning heemod

To get started read the *An Introduction to `heemod`* vignette. Specific analysis examples (mostly inspired from [Decision Modelling for Health Economic Evaluation](http://ukcatalogue.oup.com/product/9780198526629.do)) can be found in the package vignettes.

## Devs

[Kevin Zarca](http://www.urc-eco.fr/Kevin-ZARCA,402) and [Antoine Filipović-Pierucci](https://pierucci.org).

## Contributors

  * [Matthew Wiener](https://github.com/MattWiener)
  * [Zdenek Kabat](https://github.com/zkabat)
  * [Vojtech Filipec](https://github.com/vojtech-filipec)
  * [Jordan Amdahl](https://github.com/jrdnmdhl)
  * [Yonatan Carranza Alarcon](https://github.com/salmuz)
  * [Vince Daniels](https://github.com/daniels4321)
  