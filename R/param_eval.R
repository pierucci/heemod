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
                            strategy_name = NA, max_state_time = cycles) {
  # update calls to dispatch_strategy()
  x <- dispatch_strategy_hack(x)
  
  # Long form tibble w/ state_time and model_time
  start_tibble <- tibble::tibble(
    model_time = rep(seq_len(cycles), max_state_time),
    markov_cycle = rep(seq_len(cycles), max_state_time),
    state_time = rep(seq_len(max_state_time), each=cycles),
    strategy = strategy_name
  )
  
  # other datastructure?
  res <- try(
    dplyr::mutate_(
      start_tibble,
      .dots = x
    ), silent = TRUE
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

eval_init <- function(x, parameters, expand) {
  
  # Assinging NULLS to avoid CMD Check issues
  .state <- .limit <- model_time <- state_time <- .value <- NULL
  
  to_keep <- names(x)
  
  init_df <- dplyr::mutate_(.data = parameters %>% dplyr::filter(model_time == 1), .dots = x) %>%
    .[c("state_time", to_keep)] %>%
    reshape2::melt(
      id.vars = c("state_time"),
      variable.name = ".state",
      value.name = ".value"
    ) %>%
    dplyr::mutate(.state = as.character(.state)) %>%
    dplyr::left_join(expand, by = c(".state" =  ".state", "state_time" = "state_time")) %>%
    dplyr::filter(state_time <= .limit) %>%
    dplyr::mutate(
      .value = ifelse(state_time > 1, 0, .value)
    )
  
  stopifnot(
    all(init_df$.value >= 0),
    all(!is.na(init_df$.value))
  )
  
  init_vector <- init_df$.value
  names(init_vector) <- init_df$.full_state
  
  init_vector
  
}

eval_starting_values <- function(x, parameters) {
  
  # Assinging NULLS to avoid CMD Check issues
  state_time <- NULL
  
  
  to_keep <- names(x)
  
  start_df <- dplyr::mutate_(
    .data = parameters %>% dplyr::filter(state_time == 1),
    .dots = x
  )[to_keep]
  
  start_df[nrow(start_df), ] <- 0
  
  start_df
  
}

eval_inflow <- function(x, parameters, expand) {
  
  # Assinging NULLS to avoid CMD Check issues
  .state <- .limit <- state_time <- .value <- NULL
  
  to_keep <- names(x)
  inflow_df <- dplyr::mutate_(.data = parameters, .dots = x)[c("model_time", "state_time", to_keep)] %>%
    reshape2::melt(
      id.vars = c("model_time", "state_time"),
      variable.name = ".state",
      value.name = ".value"
    ) %>%
    dplyr::mutate(.state = as.character(.state)) %>%
    dplyr::left_join(expand, by = c(".state" =  ".state", "state_time" = "state_time")) %>%
    dplyr::filter(state_time <= .limit) %>%
    dplyr::mutate(.value = ifelse(state_time > 1, 0, .value)) %>%
    dplyr::ungroup()
  
  stopifnot(
    all(inflow_df$.value >= 0),
    all(!is.na(inflow_df$.value))
  )
  
  all_state_names <- unique(inflow_df$.full_state)
  inflow_mat <- inflow_df %>%
    reshape2::acast(
      model_time ~ factor(.full_state, levels = all_state_names),
      value.var = ".value",
      fill = 0
    )
  
  tibble::as.tibble(inflow_mat)
  
}
