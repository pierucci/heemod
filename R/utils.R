#' Check Wholenumbers
#' 
#' @param x numeric.
#' @param tol the smallest positive floating-point number x 
#'   such that 1 + x != 1.
#'   
#' @return A logical scalar.
#'   
#' @keywords internal
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

#' Discount a Quantity Over Time
#' 
#' This function should only take as an \code{x} argument 
#' the names of variables already defined in 
#' \code{\link{define_parameters}} or 
#' \code{\link{define_state}}, and not numeric constants.
#' 
#' @param x numeric. A quantity to discount.
#' @param r discount rate.
#' @param first logical. Should the discouting start at the
#'   first value ?
#'   
#' @return A numeric vector of the same length as \code{x}.
#' @export
#' 
#' @examples
#' 
#' discount(rep(10, 5), .02)
#' discount(rep(10, 5), .02, first = FALSE)
#' 
#' @keywords internal
discount <- function(x, r, first = FALSE) {
  if (length(r) > 1) r <- r[1]
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
#' @keywords internal
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
#' @return \code{"s"} or \code{""}.
#'   
#' @keywords internal
plur <- function(x) {
  if (x > 1) "s" else ""
}
#' @rdname plur
plur_y <- function(x) {
  if (x > 1) "ies" else "y"
}

#' Check Names
#' 
#' Throws an error if any of the names are reserved.
#' 
#' Reserved names are \code{markov_cycle} and anything
#' starting with \code{.}.
#' 
#' @param x A character vector of names.
#'   
#' @return Nothing, just throws an error if a reserved name
#'   is encountered.
#'   
#' @keywords internal
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
  if (any("state_cycle" %in% x)) {
    stop("'state_cycle' is a reserved name.")
  }
  if (any("C" %in% x)) {
    stop("'C' is a reserved name.")
  }
  if (any("strategy" %in% x)) {
    stop("'strategy' is a reserved name.")
  }
  if (any(grepl("^\\.", x))) {
    stop("Names starting with '.' are reserved.")
  }
}

#' Make Syntactically Valid Names
#' 
#' Compared to \code{\link{make.names}} this function also 
#' converts characters to lower case and replaces \code{.}
#' by \code{_}.
#' 
#' @param x A character vector.
#'   
#' @return A character vector.
#'   
#' @keywords internal
make_names <- function(x) {
  gsub("\\.+", "_", make.names(tolower(x)))
}

#' Check Strategy Index
#' 
#' @param x A result from \code{\link{run_model}}.
#' @param i A strategy index, character or numeric.
#' @param allow_multiple logical. Allow multiple strategy
#'   index?
#'   
#' @return Strategy names.
#'   
#' @keywords internal
check_strategy_index <- function(x, i, allow_multiple = FALSE) {
  
  if(length(i) != 1 & ! allow_multiple) {
    stop("Strategy index must have length 1.")
  }
  
  if (! (is.character(i) | is.numeric(i))) {
    stop("Strategy index must be either numeric or character.")
  }
  
  if (is.numeric(i) & (any(i > get_strategy_count(x)) | any(i < 1))) {
    stop(sprintf("Strategy index out of range [%i - %i].",
                 1, get_strategy_count(x)))
  }
  
  if (is.character(i) & any(! i %in% get_strategy_names(x))) {
    stop(sprintf(
      "Strategy index is not the name of a strategy (%s).",
      paste(get_strategy_names(x), collapse = " - ")
    ))
  }
  
  res <- get_strategy_names(x)
  names(res) <- res
  
  res[i]
}

#' Weighted Summary
#' 
#' Compute a weighted summary of a numeric vector.
#' 
#' If \code{weights} is \code{NULL} an unweighted summar is
#' returned.
#' 
#' @param x A numeric vector.
#' @param weights A vector of weights, same length as 
#'   \code{x}.
#'   
#' @return A vector with values \code{Min., 1st Qu., Median,
#'   Mean, 3rd Qu., Max.}.
#'   
#' @keywords internal
wtd_summary <- function(x, weights = NULL) {
  if (is.null(weights)) {
    res <- summary(x)
    
  } else if (all(is.na(x))) {
    res <- rep(NA, 6)
    
  } else {
    if (! requireNamespace("Hmisc")) {
      stop("'Hmisc' package required to produce weighted summary.")
    }
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
#' @return A converted vector.
#'   
#' @keywords internal
safe_convert <- function(x, f) {
  na1 <- is.na(x)
  res <- suppressWarnings(f(x))
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
  res_int <- safe_convert(x, as.integer)
  res_num <- safe_convert(x, as.numeric)
  
  if (! isTRUE(all.equal(res_int, res_num))) {
    stop(sprintf(
      "Floating point values coerced to integer: %s.",
      paste(
        res_num[abs(res_int - res_num) > sqrt(.Machine$double.eps)],
        collapse = ", "
      )
    ))
  }
  res_int
}

#' Convert Data Frame Factor Variables to Character
#' 
#' @param x A data frame.
#'   
#' @return A data frame.
#'   
#' @keywords internal
clean_factors <- function(x) {
  if (any(unlist(lapply(x, is.factor)))){
    for (i in seq_along(x)) {
      if (is.factor(x[[i]])) {
        x[[i]] <- as.character(x[[i]])
      }
    }
  }
  x
}

to_text_dots <- function(x, name = TRUE) {
  n <- names(x)
  ex <- unlist(lapply(
    x,
    function(y) if (any(is.na(y))) NA else
      deparse(y$expr, width.cutoff = 500L)
  ))
  
  
  if (name) {
    stopifnot(
      length(n) == length(ex)
    )
    paste(n, ex, sep = " = ")
  } else {
    ex
  }
}

interleave <- function(...) {
  .dots <- list(...)
  id <- unlist(lapply(.dots, seq_along))
  c(...)[order(id)]
}

#' Insert Elements in Vector
#' 
#' Insert a vector in another vector.
#' 
#' To insert an element at the beginning use a \code{pos} 
#' value of 0.
#' 
#' Duplicated positions are not allowed.
#' 
#' @param x A vector (or a list).
#' @param pos Integer. Insert after which elements?
#' @param what Vector of elements to insert.
#'   
#' @return A vector.
#'   
#' @examples
#' 
#' heemod:::insert(letters, c(0, 5, 26), c("xxx", "yyy"))
#' 
#' @keywords internal
insert <- function(x, pos, what) {
  
  stopifnot(
    all(pos >= 0),
    all(pos <= length(x)),
    ! any(duplicated(pos))
  )
  
  res <- c(x, rep(what, length(pos)))
  
  id  <- c(
    seq_along(x),
    rep(pos, each = length(what)) +
      seq(0, .9, length.out = length(what))
  )
  res[order(id)]
}

get_tm_pos <- function(row, col, n) {
  (row - 1) * n + col
}

pretty_names <- function(x) {
  if (is_matrix <- inherits(x, "matrix")) {
    n <- colnames(x)
  } else {
    n <- names(x)
  }
  
  names(n) <- n
  
  ref <- tibble::tibble(
    from = c(".cost", ".effect",
             ".dcost", ".deffect",
             ".icer", ".dref",
             ".model_names"),
    to = c("Cost", "Effect",
           "Cost Diff.", "Effect Diff.",
           "ICER", "Ref.",
           "Strategy")
  ) %>% 
    dplyr::filter_(~ from %in% n)
  
  n[ref$from] <- ref$to
  
  if (is_matrix) {
    colnames(x) <- n
  } else (
    names(x) <- n
  )
  
  x
}
