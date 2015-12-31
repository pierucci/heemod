#' Convenience Probability Density Functions for 
#' Probabilistic Analysis
#' 
#' @param mean Distribution mean.
#' @param sd Distribution standard deviation.
#' @param ... Dirichlet distribution parameters.
#'   
#' @export
#' 
normal <- function(mean, sd) {
  list(r_norm(mean, sd))
}
r_norm <- function(mean, sd) {
  function(x) qnorm(p = x, mean = mean, sd = sd)
}

#' @rdname normal
#' @export
lognormal <- function(mean, sd) {
  list(r_lognormal(mean, sd))
}
r_lognormal <- function(meanlog, sdlog) {
  function(x) qlnorm(p = x, meanlog = meanlog, sdlog = sdlog)
}

#' @rdname normal
#' @export
make_gamma <- function(mean, sd) {
  list(r_gamma(mean^2/sd^2, sd^2/mean))
}
r_gamma <- function(shape, scale) {
  function(x) qgamma(p = x, shape = shape, scale = scale)
}

#' @rdname normal
#' @export
prop <- function(prob, size) {
  list(r_binom(prob, size))
}
r_binom <- function(prob, size) {
  function(x) qbinom(p = x, size = size, prob = prob) / size
}

#' @rdname normal
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

