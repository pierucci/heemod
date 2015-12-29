#' Convenience Probability Density Functions for 
#' Probabilistic Analysis
#' 
#' @param mean Distribution mean.
#' @param sd Distribution standard deviation.
#' @param ... Dirichlet distribution parameters.
#'   
#' @export
#' 
norm <- function(mean, sd) {
  list(r_norm(mean, sd))
}
r_norm <- function(mean, sd) {
  function(x) qnorm(p = x, mean = mean, sd = sd)
}

#' @rdname norm
#' @export
multinom <- function(...) {
  list_param <- list(...)
  total = sum(unlist(list_param))
  structure(
    lapply(list_param, function(x) r_multinom(x, total)),
    class = c("list", "multinom_param")
  )
}
r_multinom <- function(n, total) {
  function(x) qbeta(x, n, total - n)
}

