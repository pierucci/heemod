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
  values$nGlobalParameters
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

ux_parameters <- function(input, values) {
  seq_param <- seq_len(ux_nb_parameters(values))
  
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  names_parameters <- unlist(
    shiny_subset(
      input,
      paste0("globalParamName", seq_param)
    )
  ) %>%
    trim
  
  values_parameters <- unlist(
    shiny_subset(
      input,
      paste0("globalParamValue", model_number, seq_param)
    )
  )
  
  param_ok <- names_parameters != "" &
    values_parameters != ""
  
  names_parameters <- names_parameters[param_ok]
  values_parameters <- values_parameters[param_ok]
  
  test <- function(x) {
    if (is.null(x)) {
      FALSE
    } else if (all(x == "")) {
      FALSE
    } else {
      TRUE
    }
  }
  
  res <- if (test(values_parameters) & ux_use_morta(input)) {
    names(values_parameters) <- names_parameters
    
    param_dots <- lazyeval::as.lazy_dots(values_parameters)
    
    param_dots <- c(
      lazyeval::lazy_dots(
        mortality_rate = get_who_mr(
          age = ux_morta_age(input) + markov_cycle,
          sex = ux_morta_sex(input),
          country = ux_morta_country(input)
        )
      ),
      param_dots
    )
    
    define_parameters_(
      param_dots
    )
  } else if (! test(values_parameters) & ux_use_morta(input)) {
    param_dots <- lazyeval::lazy_dots(
      mortality_rate = get_who_mr(
        age = ux_morta_age(input) + markov_cycle,
        sex = ux_morta_sex(input),
        country = ux_morta_country(input)
      )
    )
    
    define_parameters_(
      param_dots
    )
    
  } else if (test(values_parameters)) {
    names(values_parameters) <- names_parameters
    
    param_dots <- lazyeval::as.lazy_dots(values_parameters)
    
    define_parameters_(
      param_dots
    )
    
  } else {
    define_parameters()
  }
  
  res
}

ux_use_morta <- function(input) {
  input$use_morta
}

ux_morta_age <- function(input) {
  input$startAge
}

ux_morta_sex <- function(input) {
  input$gender
}

ux_morta_country <- function(input) {
  input$countryChoice
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
  as.vector(
    unlist(
      shiny_subset(
        input,
        paste0("init", seq_len(ux_nb_states(input)))
      )
    )
  )
}

ux_cycles <- function(input) {
  input$cycles
}

ux_method <- function(input) {
  input$countMethod
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

ux_run_models_raw <- function(input, values) {
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
    # parameters = ux_parameters(
    #   input = input,
    #   values = values
    # ),
    parameters = define_parameters(),
    list_models = list_models,
    init = ux_init(input),
    cycles = ux_cycles(input),
    method = ux_method(input),
    cost = ux_cost(input),
    effect = ux_effect(input),
    base_model = ux_base_model(input)
  )
}

ux_run_models <- function(input, values) {
  res <- try({
    ux_run_models_raw(input, values)
  }, 
  silent = TRUE)
  
  if ("try-error" %in% class(res)) {
    NULL
  } else {
    res
  }
}
