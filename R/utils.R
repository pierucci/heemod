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
#' This function should only take as an \code{x} argument
#' the names of variables already defined in
#' \code{\link{define_parameters}} or
#' \code{\link{define_state}},
#' and not numeric constants.
#' 
#' @param x numeric. A quantity to discount.
#' @param r discount rate.
#' @param first logical. Should the discouting start 
#'   at the first value ?
#'   
#' @return A numeric vector of the same length as \code{x}.
#' @export
#' 
#' @examples
#' 
#' discount(rep(10, 5), .02)
#' discount(rep(10, 5), .02, first = FALSE)
#' 
discount <- function(x, r, first = FALSE) {
  stopifnot(
    r >= 0,
    r <= 1
  )

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

#' Returns "s" if x > 1
#'
#' @param x integer.
#'
#' @return "s" or ""
plur <- function(x) {
  if (x > 1) "s" else ""
}

#' Get values from a specific model
#' 
#' @param x A data.frame with results from several models.
#' @param m Model name or position.
#'   
#' @return A data.frame with results from only one model.
get_model <- function(x, m) {
  model_names <- unique(x$.model_names)
  names(model_names) <- model_names
  x[x$.model_names == model_names[m], ]
}

#' Check Names
#' 
#' Throws an error if any of the names are reserved.
#'
#' Reserved names are \code{markov_cycle} and anything starting with \code{.}.
#'
#' @param x A character vector of names.
#'
#' @return Nothing, just throws an error if a reserved name is encountered.
check_names <- function(x) {
  if (is.null(x)) {
    stop("Names must exist.")
  }
  if (anyNA(x)) {
    stop("Missing names are not allowed.")
  }
  if (any("" %in% x)) {
    stop("Empty string names are not allowed.")
  }
  if (any("markov_cycle" %in% x)) {
    stop("'markov_cycle' is a reserved name.")
  }
  if (any("C" %in% x)) {
    stop("'C' is a reserved name.")
  }
  if (any(grepl("^\\.", x))) {
    stop("Names starting with '.' are reserved.")
  }
}

#' Make Syntactically Valid Names
#'
#' Compared to \code{\link{make.names}} this function also 
#' converts characters to lower case and replaces \code{.} by \code{_}.
#'
#' @param x A character vector.
#'
#' @return A character vector.
make_names <- function(x) {
  gsub("\\.+", "_", make.names(tolower(x)))
}
