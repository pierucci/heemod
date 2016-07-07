#' Convenience Functions to Compute Probabilities
#' 
#' 
#' These convienience functions make it easier to compute
#' transition probabilities from indidence rates, OR, RR,
#' or probabilities estimated on a different timeframe.
#'
#' @name probability
#' @param p Probability.
#' @param r Rate.
#' @param or Odds ratio.
#' @param rr Relative risk.
#' @param to Compute probability for that timeframe.
#' @param from Timeframe of the original probability.
#' @param per Number of person-time corresponding to the rate. 
#'
#' @return A probability.
#'
#' @example inst/examples/example_transform.R
NULL

#' @export
#' @rdname probability
prob_to_prob <- function(p, to = 1, from = 1) {
  r <- - log(1 - p) / from
  rate_to_prob(r, to = to)
}

#' @export
#' @rdname probability
rate_to_prob <- function(r, to = 1, per = 1) {
  r <- r / per
  1 - exp(- r * to)
}

#' @export
#' @rdname probability
or_to_prob <- function(or, p) {
  stopifnot(
    or >= 0,
    p >= 0,
    p <= 1
  )
  res <- 1 / (1 + exp(- log(or) + log(p / (1 - p))))
  
  stopifnot(
    res >= 0,
    res <= 1
  )
  
  res
}

#' @export
#' @rdname probability
rr_to_prob <- function(rr, p) {
  res <- rr * p
  stopifnot(
    rr >= 0,
    p >= 0,
    p <= 1,
    res >= 0,
    res <= 1
  )
  res
}
