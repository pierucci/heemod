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
eval_parameters <- function(x, cycles = 1, strategy_name = NULL) {
  # other datastructure?
  
    start_tibble <- 
      tibble::tibble(
        markov_cycle = seq_len(cycles)
      )

    if(!is.null(strategy_name))
      start_tibble$strategy <- rep(strategy_name, cycles)
  
  res <- try(
      dplyr::mutate_(
      start_tibble,
      .dots = x
    )
  )
  
  ## if we run into an error, figure out which parameter caused it -
  ##    unfortunately I don't know a better way to do that than to
  ##    go through sublists until we hit the error and then back up
  if(inherits(res, "try-error")){
    long_res <- 
      lapply(1:length(x), function(i){
        try(dplyr::mutate_(
          start_tibble,
          .dots = x[1:i]
        ), silent = TRUE)
      }
      )
    which_errors <- sapply(long_res, function(this_res){inherits(this_res, "try-error")})
    param_num <- min(which(which_errors == TRUE))
    param_name <- names(x)[param_num]
    if(!is.na(param_name))
      name_message <- paste("; parameter name:", param_name)
    else
      name_message <- character(0)
    stop(paste("error in parameter ", param_num, name_message, sep = ""))
  }
  
  structure(
    res,
    class = c("eval_parameters", class(res))
  )
}
