#' Evaluate Markov model parameters
#' 
#' Evaluate parameters specified through 
#' `define_parameters`, for a given number of cycles.
#' 
#' @param x an `uneval_parameters` object.
#' @param cycles integer. Number of cycles to simulate.
#'   
#' @return An object of class `eval_parameters` 
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
    model_time = seq_len(cycles),
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
  
  if((use_fn <- options()$heemod.inf_parameter) != "ignore") {
    
    if(any(these_are_inf <- sapply(res, is.infinite))) {
      inf_param_nums <- unique(which(these_are_inf, arr.ind = TRUE)[,2])
      inf_param_names <- names(res)[inf_param_nums]
      
      error_message <- paste(
        "Infinite parameter values:",
        paste(inf_param_names, collapse = ", "),
        ";\n",
        "See the option heemod.inf_parameter, which",
        "can be 'ignore', 'warning', or 'stop' (the default)."
      )
      get(use_fn)(error_message)
    }
  }
  ## if we run into an error, figure out which parameter caused it -
  ##    this is efficient enough unless we have a parameter that's
  ##    very expensive to calculate.
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

eval_init <- function(x, parameters) {
  to_keep <- names(x)
  
  dplyr::mutate_(.data = parameters, .dots = x)[to_keep]
}
