#' Convenience Probability Density Functions for 
#' Probabilistic Analysis
#' 
#' @param mean Distribution mean.
#' @param sd Distribution standard deviation.
#' @param n Number of observed case for multinomial 
#'   distribution.
#' @param total Population size for multinomial
#'   distribution.
#'   
#' @return A function taking a probability as argument and 
#'   returning the quantile from the specified ditribution.
#' @export
#' 
r_norm <- function(mean, sd) {
  function(x) qnorm(p = x, mean = mean, sd = sd)
}

#' @rdname r_norm
r_multinom <- function(n, total) {
  function(x) qbeta(x, n, total - n)
}
