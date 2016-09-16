#' Define a Sensitivity Analysis
#' 
#' Define parameter variations for a Markov model 
#' sensitivity analysis.
#' 
#' Parameter variations windows are given as length 2 vector
#' of the form \code{c(min, max)}.
#' 
#' @param ... A named list of min and max values that 
#'   parameters will take.
#' @param .dots Used to work around non-standard evaluation.
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
      min_dots <- c(min_dots, .dots[[i]])
    } else {
      max_dots <- c(max_dots, .dots[[i]])
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
  
  if (! all(unlist(lapply(.dots, function(x) length(x))) == 2)) {
    stop("Incorrect number of elements in sensitivity definition, the correct form is A = c(A_min, A_max)...")
  }
  
  if (any(duplicated(names(.dots)))) {
    stop("Some names are duplicated.")
  }
  
  f <- function(x, y) {
    x <- dplyr::data_frame(x = x)
    names(x) <- y
    x
  }
  
  list_df <- mapply(f , .dots, names(.dots), SIMPLIFY = FALSE)
  
  structure(
    Reduce(dplyr::bind_rows, list_df),
    class = c("sensitivity", class(list_df[[1]])),
    variables = names(.dots)
  )
}
