ux_nb_models <- function(input) {
  input$nbStrategies
}

ux_model_names <- function(input) {
  unlist(
    input[paste0("strategyName", seq_len(ux_nb_models(input)))]
  )
}

ux_nb_parameters <- function(valeurs) {
  valeurs$nbGlobalParameters
}

ux_nb_states <- function(input) {
  input$nbStates
}

ux_nb_state_values <- function(input) {
  
}

ux_state_value_names <- function(input) {
  
}

ux_state_names <- function(input) {
  unlist(
    input[paste0("stateName", seq_len(ux_nb_states(input)))]
  )
}

ux_parameters <- function(input, valeurs, model_number) {
  seq_param <- seq_len(ux_nb_parameters(valeurs))
  
  names_parameters <- unlist(
    input[paste0("globalParamName", seq_param)]
  )
  values_parameters <- unlist(
    input[paste0(globalParamValue, model_number, seq_param)]
  )
  names(values_parameters) <- names_parameters
  
  define_parameters_(
    lazyeval::as.lazy_dots(values_parameters)
  )
}

ux_matrix <- function(input, model_number) {
  nb_states <- ux_nb_states(input)
  
  mat_values <- input[paste0(
    "transmatrix",
    model_number,
    rep(nb_states, each = nb_states),
    rep(nb_states, nb_states)
  )]
  
  define_matrix_(
    .dots = lazyeval::as.lazy_dots(mat_values),
    state_names = ux_state_names(input)
  )
}

ux_state <- function(input, model_number, state_number) {
  state_value_names <- ux_state_value_names(input)
  
  state_values <- sprintf(
    "discount(%s, %e)",
    unlist(
      input[paste0(
        "stateVariable",
        model_number,
        seq_len(ux_nb_state_values(input)),
        state_number
      )]
    ),
    unlist(
      input[paste0(
        "discountingRate",
        model_number,
        seq_len(ux_nb_state_values(input))
      )]
    )
  )
  
  names(state_values) <- state_value_names
  
  define_state_(
    lazyeval::as.lazy_dots(state_values)
  )
}

ux_state_list <- function(input, model_number) {
  nb_states <- ux_number_states(input)
  
  list_states <- lapply(
    seq_len(nb_states),
    function(x)
      ux_state(input = input,
               model_number = model_number,
               state_number = x)
  )
  names(list_states) <- ux_state_names(input)
  define_state_list_(list_states)
}

ux_model <- function(input, model_number) {
  define_model_(
    parameters = ux_parameters(
      input = input,
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
  
}

ux_cycles <- function(input) {
  
}

ux_method <- function(input) {
  
}

ux_cost <- function(input) {
  
}

ux_effect <- function(input) {
  
}

ux_base_model <- function(input) {
  
}

ux_run_models <- function(input) {
  list_models <- lapply(
    seq_len(ux_nb_models),
    function(x)
      ux_model(
        input = input,
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
}
