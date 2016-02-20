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
discount <- function(x, r, first = FALSE) {
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

#' Get Mortality Rate
#' 
#' Convenience function for non-homogeneous models. Given 
#' age, sex and a reference table, returns mortality rate.
#' 
#' The \code{ref} table must contain 3 columns named 
#' \code{age}, \code{sex} and \code{rate}.
#' 
#' @param age Age.
#' @param sex Sex.
#' @param ref Reference mortality rate table, see details
#'   for structure.
#'   
#' @return A vector of mortality rates.
#' @export
#' 
#' @examples
#' 
#' age_seq <- 31:38
#' sex_seq <- rep(1:2, 4)
#' 
#' tab_ref <- data.frame(
#'   age = c("[30,35)", "[30,35)", "[35,40)", "[35,40)"),
#'   sex = c(1, 2, 1, 2),
#'   rate = c(.02, .01, .05, .04)
#' )
#' 
#' get_mortality_rate(
#'   age = cut(age_seq, c(30, 35, 40), right = FALSE),
#'   sex = sex_seq,
#'   ref = tab_ref
#' )
#' 
get_mortality_rate <- function(age, sex, ref) {
  tab <- data.frame(age = age, sex = sex)
  res <- merge(tab, ref)$rate
  switch(
    as.character(sign(nrow(tab) - length(res))),
    "0" = {
      res
    },
    "-1" = {
      stop("Multiple matches for a single age/sex case in 'get_mortality_rate'.")
    },
    "1" = {
      stop("No matches for at least one age/sex case in 'get_mortality_rate'.")
    }
  )
}
