#' Get count number
#' 
#' @param x transition matrix
#' @param init numeric vector, same length as number of 
#'   model states. Number of individuals in each model state
#'   at the beginning.
#' @return A count matrix
#'   
#' @keywords internal
get_mat_total <- function(x, init) {
  mod1 <- x * init
  diag(mod1) <- diag(mod1) - init
  return(mod1)
}


#' Get count matrix and difference between two cycles
#' 
#' @param x transition matrix
#' @param init numeric vector, same length as number of 
#'   model states. Number of individuals in each model state
#'   at the beginning.
#' @param inflow numeric vector, similar to `init`.
#'   Number of new individuals in each state per cycle.
#'   
#' @return A length 2 list of matrix : the count matrix for each cycle and the diff matrix 
#'   showing the difference of counts between two cycles.
#'   
#' @keywords internal
get_counts_diff <- function(x, init, inflow) {
  lapply(seq(1, length(x) + 1), function(i){
    if (i == length(x) + 1) return(list(init, NULL))
    init <- init + unlist(inflow[i, ])
    mat <- get_mat_total(x[[i]], init)
    res <- list(init, mat)
    init <<- colSums(mat) + init
    return(res)
  })
}

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
#' @param x numeric. A quantity to discount.
#' @param r discount rate.
#' @param first logical. Should discouting start at the
#'   first value ?
#' @param period Number of cycle per unit of discount rate.
#'   
#' @return A numeric vector of the same length as `x`.
#' @export
#' 
#' @examples
#' 
#' discount(rep(10, 5), .02)
#' discount(rep(10, 5), .02, first = FALSE)
#' 
#' @keywords internal
discount <- function(x, r, first = FALSE, period = 1) {
  if (length(r) > 1) r <- r[1]
  stopifnot(
    r >= 0,
    r <= 1,
    period > 0
  )
  
  dr <- trunc((seq_along(x) - (1 - isTRUE(first))) / period)
  x / (1 + r) ^ dr
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
#' @return `"s"` or `""`.
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
#' Reserved names are `markov_cycle` and anything
#' starting with `.`.
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
  if (any("model_time" %in% x)) {
    stop("'model_time' is a reserved name.")
  }
  if (any("state_cycle" %in% x)) {
    stop("'state_cycle' is a reserved name.")
  }
  if (any("state_time" %in% x)) {
    stop("'state_time' is a reserved name.")
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
#' Compared to [make.names()] this function also 
#' converts characters to lower case and replaces `.`
#' by `_`.
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
#' @param x A result from [run_model()].
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
#' If `weights` is `NULL` an unweighted summar is
#' returned.
#' 
#' @param x A numeric vector.
#' @param weights A vector of weights, same length as 
#'   `x`.
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
    w_mean <- wtd_mean(x, weights = weights)
    w_q <- wtd_quantile(x, weights = weights,
                        probs = c(0, .25, .5, .75, 1))
    res <- c(w_q[1], w_q[2], w_q[3], w_mean, w_q[4], w_q[5])
  }
  
  setNames(res, c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."))
}

wtd_quantile <- function(x, weights = rep(1L, length(x)),
                         probs = seq(0, 1, .25)) {
  i <- order(x)
  quant <- cumsum(weights[i]) - weights[i] / 2
  quant <- (quant - quant[1]) / (quant[length(quant)] - quant[1])
  
  stats::approx(x = quant, y = x[i], xout = probs,
                method = "linear")$y
}

wtd_mean <- function(x, weights = rep(1L, length(x))) {
  sum(x * weights) / sum(weights)
}

#' Safely Convert From Characters to Numbers
#' 
#' These function return an error if a conversion fails.
#' 
#' @name safe_conversion
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

#' @rdname safe_conversion
as_numeric_safe <- function(x) {
  safe_convert(x, as.numeric)
}

#' @rdname safe_conversion
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
  ex <- if (is.atomic(x)) {
    format(x)
  } else {
    unlist(lapply(
      x,
      function(y) if (any(is.na(y))) NA else
        deparse(y$expr, width.cutoff = 500L)
    ))
  }
  
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
#' To insert an element at the beginning use a `pos` 
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
    dplyr::filter(.data$from %in% n)
  
  n[ref$from] <- ref$to
  
  if (is_matrix) {
    colnames(x) <- n
  } else (
    names(x) <- n
  )
  
  x
}

to_dots <- function(x) {
  UseMethod("to_dots")
}

to_dots.default <- function(x) {
  lazyeval::as.lazy_dots(lapply(
    x, function(x) x
  ))
}

to_dots.list <- function(x) {
  f <- function(x) {
    if (inherits(x, "character") || inherits(x, "factor")) {
      structure(
        list(
          expr = as.character(x),
          env = globalenv()
        ),
        class = "lazy"
      )
    } else {
      x
    }
  }
  
  lazyeval::as.lazy_dots(
    lapply(x, f)
  )
}

# transforms factors to characters in a df
clean_factors <- function(x) {
  for (n in names(x)) {
    if (inherits(x[[n]], "factor")) {
      x[[n]] <- as.character(x[[n]])
    }
  }
  x
}

# formula operations

is_one_sided <- function(x) {
  length(x) == 2
}

lhs <- function(x) {
  if (is_one_sided(x)) {
    stop("Cannont extract left hand side of a one-sided formula.")
  } else {
    x[[2]]
  }
}

rhs <- function(x) {
  if (is_one_sided(x)) {
    x[[2]]
  } else {
    x[[3]]
  }
}

make_call <- function(x, collapse) {
  if (length(x) > 1) {
    as.call(
      list(
        as.name(collapse),
        as.name(x[1]),
        make_call(x[-1], collapse = collapse)
      )
    )
  } else {
    as.name(x)
  }
}

reshape_long <- function(data, key_col, value_col,
                         gather_cols, na.rm = FALSE) {
  idvar <- names(data)[! names(data) %in% gather_cols]
  
  ids <- return_ids(data, idvar)
  
  stopifnot(
    all(! duplicated(ids))
  )
  
  d <- data
  d <- d[, ! (names(data) %in% gather_cols), drop = FALSE]
  res <- do.call(
    rbind,
    lapply(gather_cols,
           function(col) {
    d[, key_col] <- col
    d[, value_col] <- data[, col]
    d
  }))
  
  if (na.rm) {
    res <- res[! is.na(res[[value_col]]), ]
  }
  
  return(res)
}

return_ids <- function(data, idvar) {
  if (length(idvar)) {
    tab_id <- data[idvar]
    atomic_id <- unlist(lapply(tab_id, is.atomic))
    for (id in idvar[! atomic_id]) {
      tab_id[id] <- seq_len(nrow(data))
    }
    if (length(idvar) > 1L) {
      ids <- interaction(tab_id[, idvar], drop = TRUE)
    } else {
      ids <- tab_id[[idvar]]
    }
  } else {
    ids <- seq_len(nrow(data))
  }
  ids
}

reshape_wide <- function(data, key_col, value_col, fill = NA) {
  idvar <- names(data)[! names(data) %in% c(key_col, value_col)]
  
  ids <- return_ids(data, idvar)
  
  unique_ids <- ids[! duplicated(ids)]
  
  stopifnot(
    all(! is.na(data[[key_col]]))
  )
  
  res <- data[! duplicated(ids), idvar, drop = FALSE]
  
  cbind(
    res,
    do.call(
      cbind,
      stats::setNames(
        object = lapply(
          unique(data[[key_col]]),
          function(x) {
            ret <- vector(
              mode = class(data[[value_col]]),
              length = nrow(res))
            ret <- fill
            index_key <- data[[key_col]] == x
            ret[unique_ids %in% ids[index_key]] <-
              data[index_key, ][[value_col]]
            ret
          }
        ),
        nm = unique(data[[key_col]])
      )
    )
  )
}
