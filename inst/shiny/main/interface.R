shiny_subset <- function(x, elem_names) {
  res <- list()
  for (n in elem_names) {
    res <- c(res, list(x[[n]]))
  }
  names(res) <- elem_names
  res
}

ux_nb_models <- function(input) {
  input$nbStrategies
}

ux_model_names <- function(input) {
  unlist(
    shiny_subset(
      input,
      paste0("strategyName", seq_len(ux_nb_models(input)))
    )
  )
}

ux_nb_parameters <- function(values) {
  values$nbGlobalParameters
}

ux_nb_states <- function(input) {
  input$nbStates
}

ux_nb_state_values <- function(input) {
  input$nbStateVariables
}

ux_state_value_names <- function(input) {
  unlist(
    shiny_subset(
      input,
      paste0("variableStateName", seq_len(ux_nb_state_values(input)))
    )
  )
}

ux_state_names <- function(input) {
  unlist(
    shiny_subset(
      input,
      paste0("stateName", seq_len(ux_nb_states(input)))
    )
  )
}

ux_parameters <- function(input, values, model_number) {
  seq_param <- seq_len(ux_nb_parameters(values))
  
  names_parameters <- unlist(
    shiny_subset(
      input,
      paste0("globalParamName", seq_param)
    )
  )
  values_parameters <- unlist(
    shiny_subset(
      input,
      paste0("globalParamValue", model_number, seq_param)
    )
  )
  
  test <- function(x) {
    if (is.null(x)) {
      FALSE
    } else if (x == "") {
      FALSE
    } else {
      TRUE
    }
  }
  
  if (test(values_parameters)) {
    names(values_parameters) <- names_parameters
    define_parameters_(
      lazyeval::as.lazy_dots(values_parameters)
    )
  } else {
    define_parameters()
  }
}

ux_matrix <- function(input, model_number) {
  res <- try({
    nb_states <- ux_nb_states(input)
    
    mat_values <- shiny_subset(
      input,
      paste0(
        "transmatrix",
        model_number,
        rep(seq_len(nb_states), each = nb_states),
        rep(seq_len(nb_states), nb_states)
      )
    )
    
    define_matrix_(
      .dots = lazyeval::as.lazy_dots(mat_values),
      state_names = ux_state_names(input)
    )
  })
  
  if ("try-error" %in% class(res)) {
    NULL
  } else {
    res
  }
}

ux_state <- function(input, model_number, state_number) {
  state_value_names <- ux_state_value_names(input)
  
  state_values <- sprintf(
    "heemod::discount(%s, %e)",
    unlist(
      shiny_subset(
        input,
        paste0(
          "stateVariable",
          model_number,
          seq_len(ux_nb_state_values(input)),
          state_number
        )
      )
    ),
    unlist(
      shiny_subset(
        input,
        paste0(
          "discountingRate",
          model_number,
          seq_len(ux_nb_state_values(input))
        )
      )
    ) / 100
  )
  
  names(state_values) <- state_value_names
  
  define_state_(
    lazyeval::as.lazy_dots(state_values)
  )
}

ux_state_list <- function(input, model_number) {
  nb_states <- ux_nb_states(input)
  
  list_states <- lapply(
    seq_len(nb_states),
    function(x)
      ux_state(
        input = input,
        model_number = model_number,
        state_number = x
      )
  )
  names(list_states) <- ux_state_names(input)
  define_state_list_(list_states)
}

ux_model <- function(input, values, model_number) {
  define_model_(
    parameters = ux_parameters(
      input = input,
      values = values,
      model_number = model_number
    ),
    transition_matrix = ux_matrix(
      input = input,
      model_number = model_number
    ),
    states = ux_state_list(
      input = input,
      model_number = model_number
    )
  )
}

ux_init <- function(input) {
  c(1000, rep(0, ux_nb_states(input) - 1))
}

ux_cycles <- function(input) {
  10
}

ux_method <- function(input) {
  "beginning"
}

ux_cost <- function(input) {
  lazyeval::as.lazy(input$costVariable)
}

ux_effect <- function(input) {
  lazyeval::as.lazy(input$effectVariable)
}

ux_base_model <- function(input) {
  ux_model_names(input)[1]
}

ux_run_models <- function(input, values) {
  res <- try({
    list_models <- lapply(
      seq_len(ux_nb_models(input)),
      function(x)
        ux_model(
          input = input,
          values = values,
          model_number = x
        )
    )
    names(list_models) <- ux_model_names(input)
    
    run_models_(
      list_models = list_models,
      init = ux_init(input),
      cycles = ux_cycles(input),
      method = ux_method(input),
      cost = ux_cost(input),
      effect = ux_effect(input),
      base_model = ux_base_model(input)
    )
  }, 
  silent = TRUE)
  
  if ("try-error" %in% class(res)) {
    NULL
  } else {
    res
  }
}
