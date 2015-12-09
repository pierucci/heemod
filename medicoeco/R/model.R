define_model <- function(
  parameters = define_parameters(),
  transition_matrix = define_matrix(),
  states = define_states()
) {
  
  stopifnot(
    state_count(states) == 0 |
      state_count(states) == matrix_order(transition_matrix),
    names_parameters(parameters) %>%
      intersect(names_state_values(states)) %>%
      length %>%
      equals(0),
    identical(
      sort(names_states(states)),
      sort(names_states(transition_matrix))
    )
  )
  
  structure(
    list(
      parameters = parameters,
      transition_matrix = transition_matrix,
      states = states
    ), class = "uneval_model")
}

print.uneval_model <- function(x, ...) {
  cat(sprintf(
    "An unevaluated Markov model:
    %i parameters,
    %i states,
    %i state values.\n",
    length(names_parameters(get_parameters(x))),
    get_states(x) %>% state_count,
    get_states(x) %>% names_state_values %>% length
  ))
}

get_parameters <- function(x)
  x$parameters

get_matrix <- function(x)
  x$transition_matrix

get_states <- function(x)
  x$states

get_counts <- function(x)
  x$counts

eval_model <- function(
  model,
  cycles = 1, 
  init = c(1, rep(0, state_count(get_states(model)) - 1)),
  ...
) {
  parameters <- eval_parameters(get_parameters(model),
                                cycles = cycles)
  transition_matrix <- eval_matrix(get_matrix(model),
                                   parameters)
  states <- eval_states(get_states(model), parameters)
  
  count_table <- compute_counts(transition_matrix, init, ...)
  
  list(
    parameters = parameters,
    transition_matrix = transition_matrix,
    states = states,
    counts = count_table
  )
}

compute_counts <- function(
  transition_matrix, init,
  method = c("final", "initial", "exponential", "linear"),
  round = FALSE
) {
  method <- match.arg(method)
  
  
  list_counts <- Reduce(
    "%*%",
    transition_matrix,
    init,
    accumulate = TRUE
  )
  
  res <- matrix(
    unlist(list_counts),
    byrow = TRUE,
    ncol = matrix_order(transition_matrix)
  ) %>%
    as.data.frame %>%
    as.tbl
  
  colnames(res) <- names_states(transition_matrix)
  
  n0 <- res[- nrow(res), ]
  n1 <- res[-1, ]
  
  switch(
    method,
    initial = {
      if (round) round(n0) else n0
    },
    final = {
      if (round) round(n1) else n1
    },
    exponential = {
      stop("Unimplemented")
    },
    linear = {
      (n0 + n1) / 2
    },
    {
      stop()
    }
  )
}

