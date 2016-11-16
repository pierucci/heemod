#' @export
#' @rdname define_strategy
define_model <- function(...) {
  warning("'define_model' is deprecated, use 'define_strategy' instead.")
  define_strategy(...)
}

#' @export
#' @rdname define_strategy
define_model_ <- function(...) {
  warning("'define_model_' is deprecated, use 'define_strategy_' instead.")
  define_strategy_(...)
}

#' @export
#' @rdname run_model
run_models <- function(...) {
  warning("'run_models' is deprecated, use 'run_model' (no 's') instead.")
  run_model(...)
}

#' @export
#' @rdname run_model
run_models_ <- function(...) {
  warning("'run_models_' is deprecated, use 'run_model_' (no 's') instead.")
  run_model_(...)
}

#' @export
#' @rdname define_transition
define_matrix <- function(...) {
  warning("'define_matrix' is deprecated, use 'define_transition' instead.")
  define_transition(...)
}

#' @export
#' @rdname define_transition
define_matrix_ <- function(...) {
  warning("'define_matrix_' is deprecated, use 'define_transition_' instead.")
  define_transition_(...)
}

#' @export
#' @rdname define_dsa
define_sensitivity <- function(...) {
  warning("'define_sensitivity' is deprecated, use 'define_dsa' instead.")
  define_dsa(...)
}

#' @export
#' @rdname define_dsa
define_sensitivity_ <- function(...) {
  warning("'define_sensitivity_' is deprecated, use 'define_dsa_' instead.")
  define_dsa_(...)
}

#' @export
#' @rdname define_psa
define_distrib <- function(...) {
  warning("'define_distrib' is deprecated, use 'define_psa' instead.")
  define_psa(...)
}

#' @export
#' @rdname define_psa
define_distrib_ <- function(...) {
  warning("'define_distrib_' is deprecated, use 'define_psa_' instead.")
  define_psa_(...)
}

#' @export
#' @rdname run_dsa
run_sensitivity <- function(model, sensitivity) {
  warning("'run_sensitivity' is deprecated, use 'run_dsa' instead.")
  run_dsa(model = model, dsa = sensitivity)
}

#' @export
#' @rdname run_psa
run_probabilistic <- function(model, resample, N) {
  warning("'run_probabilistic' is deprecated, use 'run_psa' instead.")
  run_psa(model = model, resample = resample, N = N)
}
