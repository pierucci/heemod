#' Title
#' 
#' @param model An \code{uneval_model} object.
#' @param init numeric vector, same length as number of 
#'   model states. Number of individuals in each model state
#'   at the beginning.
#' @param cycles positive integer. Number of Markov Cycles 
#'   to compute.
#' @param ... Additional arguments passed to
#'   \code{compute_counts}.
#'   
#' @return A list of individual counts and state values per
#'   cycle and total state values.
#' @export
#' 
#' @examples
simulate_cohort <- function(model, init, cycles, ...) {
  e_model <- eval_model(model = model, 
                        init = init, 
                        cycles = cycles,
                        ...)
  values <- compute_values(e_model)
  total_sum <- colSums(values[- 1])
  
  list(
    table_counts = get_counts(e_model),
    table_values = values,
    total_values = total_sum
  )
}

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
compute_values <- function(x) {
  states_names <- get_state_names(get_states(x))
  state_values_names <- get_state_value_names(get_states(x))
  
  res <- data_frame(
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


