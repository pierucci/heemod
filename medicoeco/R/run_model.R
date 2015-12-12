#' Run one or more Markov Model
#' 
#' Runs a set of unevaluated Markov Models. All models 
#' should have the same states and state value names.
#' 
#' A classical situation where more than one model needs to 
#' be run is to compare different care startegies.
#' 
#' In order to compute comparisons Markov Models must be 
#' similar (same states and state value names). Thus models 
#' should only differ through parameters, transition matrix 
#' cell values and values attached to states (but not value 
#' names).
#' 
#' The initial number of individuals in each model state and
#' the number of cycle will be the same for all models.
#' 
#' This function can run only one model.
#' 
#' @param ... \code{uneval_model} objects.
#' @param init numeric vector, same length as number of 
#'   model states. Number of individuals in each model state
#'   at the beginning.
#' @param cycles positive integer. Number of Markov Cycles 
#'   to compute.
#' @param count_args Additional arguments passed as a list 
#'   to \code{compute_counts}.
#'   
#' @return A list of individual counts and state values per 
#'   cycle and total state values.
#' @export
#' 
#' @examples 
#' 
run_models <- function(..., init, cycles, count_args = NULL) {
  
  list_models <- list(...)
  
  model_names <- names(list_models)
  
  if (is.null(model_names)) {
    message("No named model -> generating names.")
    model_names <- LETTERS[seq_along(list_models)]
    names(list_models) <- model_names
  }
  
  if (any(model_names == "")) {
    warning("Not all models are named -> generating names.")
    model_names <- LETTERS[seq_along(list_models)]
    names(list_models) <- model_names
  }
  
  stopifnot(
    all("uneval_model" %in% unlist(lapply(list_models, class))),
    list_all_same(lapply(list_models,
                         . %>% get_state_names %>% sort)),
    list_all_same(lapply(list_models,
                         . %>% get_state_value_names %>% sort))
  )
  
  run_mod <- function(model) {
    e_model <- eval_model(model = model, 
                          init = init, 
                          cycles = cycles,
                          count_args = count_args)
    values <- compute_values(e_model)
    total_sum <- colSums(values[- 1])
    
    structure(
      list(
        table_counts = get_counts(e_model),
        table_values = values,
        total_values = total_sum
      ), 
      class = "eval_model",
      init = init,
      cycles = cycles,
      count_args = if (is.null(count_args)) NA else count_args
    )
  }
  
  structure(
    lapply(list_models, run_mod),
    names = model_names,
    class = "eval_model_list",
    init = init,
    cycles = cycles,
    count_args = if (is.null(count_args)) NA else count_args
  )
}

#' @export
#' @rdname run_models
run_model <- function(...) {
  run_models(...)
}

#' Compute State Values per Cycle
#' 
#' Given an evaluated model, computes the total state values
#' per cycle.
#' 
#' @param x An object of class \code{eval_model}.
#'   
#' @return A data.frame of state values, one column per
#'   state value and one row per cycle.
#'   
compute_values <- function(x) {
  states_names <- get_state_names(get_states(x))
  state_values_names <- get_state_value_names(get_states(x))
  
  res <- data.frame(
    markov_cycle = get_parameters(x)$markov_cycle
  )
  # bottleneck!
  for (state_value in state_values_names) {
    res[state_value] <- 0
    
    for (state in states_names) {
      res[state_value] <-
        res[state_value] +
        get_counts(x)[, state] * 
        get_states(x)[[state]][, state_value]
    }
  }
  res
}


