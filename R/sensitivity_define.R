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
#'   
#' @return A \code{sensitivity} object.
#' @export
#' 
#' @examples
#' 
#' define_sensitivity(
#'   a = c(10, 45),
#'   b = c(.5, 1.5)
#' )
#' 
define_sensitivity <- function(...) {
  .dots <- list(...)
  define_sensitivity_(.dots)
}

define_sensitivity_ <- function(.dots) {
  check_names(names(.dots))
  
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
