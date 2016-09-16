#' Define a Sensitivity Analysis
#' 
#' Define parameter variations for a Markov model 
#' sensitivity analysis.
#' 
#' @param ... A list of parameter names and min/max values 
#'   of the form \code{var1, min(var1), max(var1), var2,
#'   min(var2), max(var2), ...}.
#' @param par_names String vector of parameter names.
#' @param min_dots,max_dots Used to work around
#'   non-standard evaluation.
#'   
#' @return A \code{sensitivity} object.
#' @export
#' 
#' @examples
#' 
#' define_sensitivity(
#'   a, 10, 45,
#'   b, .5, 1.5
#' )
#' 
define_sensitivity <- function(...) {
  .dots <- lazyeval::lazy_dots(...)
  
  if (! length(.dots) %% 3 == 0) {
    stop("Incorrect number of elements in correlation definition, the correct form is A, B, cor(A, B)...")
  }
  
  par_names <- character()
  min_dots <- lazyeval::lazy_dots()
  max_dots <- lazyeval::lazy_dots()
  
  for (i in seq_along(.dots)) {
    if (i %% 3 == 1) {
      par_names <- c(par_names, deparse(.dots[[i]]$expr))
    } else if (i %% 3 == 2) {
      min_dots <- c(min_dots, list(.dots[[i]]))
    } else {
      max_dots <- c(max_dots, list(.dots[[i]]))
    }
  }
  
  names(min_dots) <- par_names
  names(max_dots) <- par_names
  
  define_sensitivity_(par_names, min_dots, max_dots)
}

#' @rdname define_sensitivity
define_sensitivity_ <- function(par_names, min_dots, max_dots) {
  
  check_names(par_names)
  
  stopifnot(
    all(par_names == names(min_dots)),
    all(par_names == names(max_dots))
  )
  dots <- c(min_dots, max_dots)
  
  if (any(duplicated(par_names))) {
    stop("Some names are duplicated.")
  }
  
  tab <- tibble::tibble()
  for (i in seq_along(dots)) {
    tab <- dplyr::bind_rows(
      tab,
      tibble::tibble_(dots[i])
    )
  }
  
  structure(
    tab,
    class = c("sensitivity", class(tab)),
    variables = par_names
  )
}
