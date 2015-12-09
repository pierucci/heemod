#' Check Wholenumbers
#' 
#' @param x numeric.
#' @param tol the smallest positive floating-point number x
#'   such that 1 + x != 1.
#'   
#' @return A logical scalar.
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

#' Discount a Quantity Over Time
#' 
#' @param x numeric. A quantity to discount.
#' @param r discount rate.
#' @param first logical. Should the discouting start at the first value ?
#'   
#' @return A numeric vector of the same length as \code{x}.
#' @export
#' 
#' @examples
#' 
#' discount(rep(10, 5), .02)
#' discount(rep(10, 5), .02, first = FALSE)
#' 
discount <- function(x, r, first = TRUE) {
  x / (1 + r) ^ (seq_along(x) - (1 - isTRUE(first)))
}

#' Check if All the Elements of a List Are the Same
#'
#' @param x a list.
#'
#' @return A logical scalar.
#' 
list_all_same <- function(x) {
  length(x) == 0 |
  all(unlist(
    Map(function(y) identical(y, x[[1]]), x)
  ))
}
