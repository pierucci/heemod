#' Probability Density Functions for 
#' Probabilistic Uncertainty Analysis
#' 
#' @name density
#' @param mean Distribution mean.
#' @param sd Distribution standard deviation.
#' @param ... Dirichlet distribution parameters.
#' @param prob Proportion.
#' @param size Size of sample used to estimate proportion.
#' @param meanlog Mean on the log scale.
#' @param sdlog SD on the log scale.
#' @param mu Mean on the lgit scale.
#' @param sigma SD on the logit scale.
#'   
#' @export
normal <- function(mean, sd) {
  list(r_norm(mean, sd))
}
r_norm <- function(mean, sd) {
  function(x) stats::qnorm(p = x, mean = mean, sd = sd)
}

#' @rdname density
#' @export
lognormal <- function(mean, sd, meanlog, sdlog) {
  if (missing(sdlog)) sdlog <- sqrt(log(1 + sd^2/mean^2))
  if (missing(meanlog)) meanlog <- log(mean) - sdlog^2/2
  
  list(r_lognormal(meanlog, sdlog))
}
r_lognormal <- function(meanlog, sdlog) {
  function(x) stats::qlnorm(p = x, meanlog = meanlog, sdlog = sdlog)
}

#' @rdname density
#' @export
make_gamma <- function(mean, sd) {
  list(r_gamma(mean^2/sd^2, sd^2/mean))
}
r_gamma <- function(shape, scale) {
  function(x) stats::qgamma(p = x, shape = shape, scale = scale)
}

#' @rdname density
#' @export
prop <- function(prob, size) {
  list(r_binom(prob, size))
}
r_binom <- function(prob, size) {
  function(x) stats::qbinom(p = x, size = size, prob = prob) / size
}

#' @rdname density
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
  function(x) stats::qbeta(x, n, total - n)
}


#' @rdname density
#' @export
logitnormal <- function(mu, sigma) {
  list(r_logitnormal(mu, sigma))
}
r_logitnormal <- function(mu, sigma) {
  function(x) logitnorm::qlogitnorm(p = x, mu = mu, sigma = sigma)
}
