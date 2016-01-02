
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
  define_sensitiviy_(.dots)
}

define_sensitivity_<- function(.dots) {
  stopifnot(
    all(unlist(lapply(.dots, function(x) length(x))) == 2),
    ! is.null(names(.dots)),
    ! any(names(.dots) == ""),
    ! any(duplicated(names(.dots)))
  )
  
  list_sens <- lapply(.dots, function(x) sort(x))
}
