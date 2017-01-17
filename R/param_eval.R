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
eval_parameters <- function(x, cycles = 1,
                            strategy_name = NA) {
  # update calls to dispatch_strategy()
  x <- dispatch_strategy_hack(x)
  
  start_tibble <- tibble::tibble(
    markov_cycle = seq_len(cycles),
    strategy = strategy_name
  )
  
  # other datastructure?
  res <- try(
    dplyr::mutate_(
      start_tibble,
      .dots = x
    )
  )
  
  ## if we run into an error, figure out which parameter caused it -
  ##    unfortunately I don't know a better way to do that than to
  ##    go through sublists until we hit the error and then back up
  if (inherits(res, "try-error")) {
    long_res <- lapply(
      seq_along(x),
      function(i) {
        try(dplyr::mutate_(
          start_tibble,
          .dots = x[seq_len(i)]
        ), silent = TRUE)
      }
    )
    which_errors <- sapply(
      long_res,
      function(this_res) {
        inherits(this_res, "try-error")
      })
    param_num <- min(which(which_errors))
    param_name <- get_parameter_names(x)[param_num]
    
    stop(sprintf(
      "Error in parameter: %s.", param_name),
      call. = FALSE)
  }
  
  structure(
    res,
    class = c("eval_parameters", class(res))
  )
}
