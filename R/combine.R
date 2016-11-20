#' Combine Multiple Models
#' 
#' Given a set of models run with different parameters, 
#' return aggregated results to estimate population-level
#' values.
#' 
#' @param newmodels A list of models run over a set of 
#'   multiple parameters.
#' @param weights A vector of weigths, same length as the
#'   number of parameter sets.
#' @param oldmodel The original model.
#'   
#' @return A \code{combined_models} object, mostly similar
#'   to a result from \code{\link{run_model}}. \code{plot}
#'   and \code{summary} methods are available.
#'   
#' @keywords internal
combine_models <- function(newmodels, weights, oldmodel) {
  
  total_weights <- sum(weights)
  strategy_names <- get_strategy_names(oldmodel)
  
  apply_weights <- function(x) {
    sum(x * weights / total_weights)
  }
  
  # collapse each strategy values and counts
  
  list_res <- list()
  list_eval_models <- list()
  
  for (i in seq_along(newmodels)) {
    
    # collapse total values
    collapsed_total_values <- (newmodels[[i]]) %>% 
      dplyr::rowwise() %>% 
      dplyr::do_(~ get_total_state_values(.$.mod)) %>% 
      dplyr::ungroup() %>% 
      dplyr::summarise_all(apply_weights)
    
    tab_counts <- (newmodels[[i]]) %>% 
      dplyr::rowwise() %>% 
      dplyr::do_(.counts = ~ get_counts(.$.mod))
    
    tab_values <- (newmodels[[i]]) %>% 
      dplyr::rowwise() %>% 
      dplyr::do_(.values = ~ get_values(.$.mod))
    
    collapsed_counts <- tab_counts$.counts %>% 
      mapply(
        weights,
        FUN = function(x, y) x * y / total_weights,
        SIMPLIFY = FALSE) %>% 
      Reduce(f = "+")
    
    collapsed_values <- tab_values$.values %>% 
      mapply(
        weights,
        FUN = function(x, y) x * y / total_weights,
        SIMPLIFY = FALSE) %>% 
      Reduce(f = "+")
    
    
    list_res <- c(
      list_res,
      list(collapsed_total_values)
    )
    
    list_eval_models <- 
      c(list_eval_models,
        setNames(list(list(counts = collapsed_counts,
                           values = collapsed_values)),
                 strategy_names[i]))
  }
  
  for (i in seq_along(strategy_names)){
    list_res[[i]]$.strategy_names <- strategy_names[i]
  }
  
  res <- Reduce(dplyr::bind_rows, list_res) %>% 
    dplyr::mutate_(.dots = get_ce(oldmodel))
  
  root_strategy <- get_root_strategy(res)
  noncomparable_strategy <- get_noncomparable_strategy(res)
  
  # central from oldmodel!
  central_strategy <- get_central_strategy(oldmodel)
  
  structure(
    list(
      run_model = res,
      eval_strategy_list = list_eval_models, 
      eval_model_list = list_eval_models, 
      parameters = get_parameters(oldmodel),
      init = get_init(oldmodel),
      cycles = get_cycles(oldmodel),
      method = get_method(oldmodel),
      ce = get_ce(oldmodel),
      oldmodel = oldmodel,
      root_strategy = root_strategy,
      central_strategy = central_strategy,
      noncomparable_strategy = noncomparable_strategy
    ),
    class = c("combined_model", class(res))
  )
}

get_model_results.combined_model <- function(x) {
  get_model_results.run_model(x)
}

get_oldmodel <- function(x) {
  x$oldmodel
}

get_central_strategy.combined_model <- function(x, ...) {
  get_central_strategy.run_model(x, ...)
}
get_root_strategy.combined_model <- function(x, ...) {
  get_root_strategy.run_model(x, ...)
}
get_noncomparable_strategy.combined_model <- function(x, ...) {
  get_noncomparable_strategy.run_model(x, ...)
}

get_parameters.combined_model <- function(x) {
  get_parameters(get_oldmodel(x))
}
get_init.combined_model <- function(x) {
  get_init(get_oldmodel(x))
}
get_cycles.combined_model <- function(x) {
  get_cycles(get_oldmodel(x))
}
get_method.combined_model <- function(x) {
  get_method(get_oldmodel(x))
}
get_ce.combined_model <- function(x) {
  get_ce(get_oldmodel(x))
}

#' @export
print.combined_model <- function(x, ...) {
  print(summary(x, ...))
}

#' @export
plot.combined_model <- function(x, ...) {
  plot.run_model(x, ...)
}

#' @export
summary.combined_model <- function(object, ...) {
  summary.run_model(object, ...)
}

#' @rdname heemod_scale
scale.combined_model <- function(x, center = TRUE, scale = TRUE) {
  scale.run_model(x, center = center, scale = scale)
}

#' @rdname get_counts
#' @export
get_counts.updated_model <- function(x, ...) {
  get_counts(x$combined_model, ...)
}

#' @rdname get_counts
#' @export
get_counts.combined_model <- function(x, ...) {
  get_counts.run_model(x)
}

#' @rdname get_values
#' @export
get_values.updated_model <- function(x, ...) {
  get_values(x$combined_model, ...)
}

#' @rdname get_values
#' @export
get_values.combined_model <- function(x, ...) {
  get_values.run_model(x)
}

get_state_value_names.combined_model <- function(x) {
  get_state_value_names(get_oldmodel(x))
}

