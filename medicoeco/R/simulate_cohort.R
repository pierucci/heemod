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

compute_values <- function(x) {
  states_names <- names_states(get_states(x))
  state_values_names <- names_state_values(get_states(x))
  
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


