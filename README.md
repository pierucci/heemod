`heemod` is a toolset for health ecomomic evaluation modelling. It aims to provide a simple and consistent interface for Markov models specification and comparison. Non homogenous Markov models (with time varying properties) are supported.

Most of the analyses presented in [Decision Modelling for Health Economic Evaluation](http://ukcatalogue.oup.com/product/9780198526629.do) can be performed with `heemod`.

This package is still a *beta* version.

You can install:

  * the latest released version from CRAN with:

```r
install.packages("heemod")
```

  * the latest development version from github with (warning: package `devtools` needed):

```r
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github("pierucci/heemod")
```

# Main features

  * Time-varying transition probabilities.
  * Time-varying values attached to states.
  * Heterogeneity analysis.

# Learning heemod

To get started read the intro vignette: `vignette("introduction", package = "dplyr")`.

Specific analysis examples (mostly inspired from Decision Modelling for Health Economic Evaluation) can be found in the following vignettes:

  * Homogenous Markov model in `vignette("homogenous", package = "heemod")`.
  * Non-homogenous Markov model in `vignette("non-homogenous", package = "heemod")`.
  * Heterogeneity analysis in `vignette("heterogeneity", package = "heemod")`.
  * Probabilistic incertitude analysis in `vignette("probabilistic", package = "heemod")`.
  * Model comparison in `vignette("comparison", package = "heemod")`.

# Future developments

In version 1.0 :

  * Probabilistic incertitude analysis.
  * Deterministic sensitivity analysis.
  * Graphical analysis of Markov models.
