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
#' Reserved names are \code{markov_cycle} and anything starting 
#' with \code{.}.
#'
#' @param x A character vector of names.
#'
#' @return Nothing, just throws an error if a reserved name is 
#' encountered.
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

#' Check Model Index
#'
#' @param x A result from \code{\link{run_models}}.
#' @param i A model index, character or numeric.
#' @param allow_multiple logical. Allow multiple model index?
#'
#' @return Nothing, just throws an error if an incorrect model 
#' index is used.
check_model_index <- function(x, i, allow_multiple = FALSE) {
  
  if(length(i) != 1 & ! allow_multiple) {
    stop("Model index must have length 1.")
  }
  
  if (! (is.character(i) | is.numeric(i))) {
    stop("Model index must be either numeric or character.")
  }
  
  if (is.numeric(i) & (any(i > get_model_count(x)) | any(i < 1))) {
    stop(sprintf("Model index out of range [%i - %i].", 1, get_model_count(x)))
  }
  
  if (is.character(i) & any(! i %in% get_model_names(x))) {
    stop(sprintf(
      "Model index is not the name of a model (%s).",
      paste(get_model_names(x), collapse = " - ")
    ))
  }
}

wtd_summary <- function(x, weights = NULL) {
  if (is.null(weights)) {
    res <- summary(x)
    
  } else if (all(is.na(x))) {
    res <- rep(NA, 6)
    
  } else {
    w_mean <- Hmisc::wtd.mean(x, weights = weights)
    w_q <- Hmisc::wtd.quantile(x, weights = weights,
                               probs = c(0, .25, .5, .75, 1))
    res <- c(w_q[1], w_q[2], w_q[3], w_mean, w_q[4], w_q[5])
  }
  
  setNames(res, c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."))
}

#' Safely Convert From Characters to Numbers
#' 
#' These function return an error if a conversion fails.
#' 
#' @name safe-conversion
#' @param x A character vector.
#' @param f A conversion function.
#'   
#' @return A \code{numeric} of \code{integer} vector.
safe_convert <- function(x, f) {
  na1 <- is.na(x)
  res <- f(x)
  na2 <- is.na(res)
  
  if (any(pb <- na1 != na2)) {
    stop(sprintf(
      "Failed to convert values: %s.",
      paste(x[pb], collapse = ", ")
    ))
  }
  
  res
}

#' @rdname safe-conversion
as_numeric_safe <- function(x) {
  safe_convert(x, as.numeric)
}

#' @rdname safe-conversion
as_integer_safe <- function(x) {
  safe_convert(x, as.integer)
}
