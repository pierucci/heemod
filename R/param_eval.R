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
  
  # old_classes <- class(x)
  # if (length(x)) x <- structure(x[!has_state_time(x)],
  #                          class = old_classes)
  
  start_tibble <- data.frame(
    model_time = seq_len(cycles),
    markov_cycle = seq_len(cycles),
    strategy = strategy_name,row.names = NULL,stringsAsFactors = F
  ) #%>% 
    #tibble::as_tibble()
  
  x_tidy <- compat_lazy_dots(x)
  
  # other datastructure?
  res <- try({
    lapply(seq_along(x_tidy), function(i){
      #parameters[names(x)[i]] <<- eval(rlang::quo_squash(x_tidy[[i]]), parameters)
      start_tibble[names(x)[i]] <<- rlang::eval_tidy(x_tidy[[i]], data = start_tibble)
    })
    start_tibble
  }, silent = TRUE
    # dplyr::mutate(
    #   start_tibble,
    #   !!!x_tidy
    # ), silent = TRUE
  )
  
  if ((use_fn <- options()$heemod.inf_parameter) != "ignore") {
    
    if (any(these_are_inf <- sapply(res, is.infinite))) {
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
        try(dplyr::mutate(
          start_tibble,
          !!!x_tidy[seq_len(i)]
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
    text_error <- long_res[[param_num]]
    
    stop(sprintf(
      "Error in parameter: %s: %s", param_name, text_error),
      call. = FALSE)
  }
  
  structure(
    res,
    class = c("eval_parameters", class(res))
  )
}

eval_init <- function(x, parameters) {
  to_keep <- names(x)
  x_tidy <- compat_lazy_dots(x)
  if (length(to_keep)) {
    lapply(seq_along(x_tidy), function(i){
      #parameters[to_keep[i]] <<- eval(rlang::quo_squash(x_tidy[[i]]), parameters)
      parameters[to_keep[i]] <<- rlang::eval_tidy(x_tidy[[i]], data = parameters)
    })
    parameters[to_keep]
        
    #dplyr::mutate(.data = parameters, !!!x_tidy)[to_keep]
  } else {
    tibble::tibble()
  }
}

eval_starting_values <- eval_inflow <- eval_init
