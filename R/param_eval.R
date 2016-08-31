#' Evaluate Markov model parameters
#' 
#' Evaluate parameters specified through 
#' \code{define_parameters}, for a given number of cycles.
#' 
#' @param x an \code{uneval_parameters} object.
#' @param cycles integer. Number of cycles to simulate.
#'   
#' @return An object of class \code{eval_parameters} 
#'   (actually a data.frame with one column per parameter 
#'   and one row per cycle).
#'   
#' @example inst/examples/example_eval_parameters.R
#'   
#' @keywords internal
eval_parameters <- function(x, cycles = 1) {
  # other datastructure?
  res <- dplyr::mutate_(
    tibble::tibble(
      markov_cycle = seq_len(cycles)
    ),
    .dots = x
  )
  
  structure(
    res,
    class = c("eval_parameters", class(res))
  )
}
