#' Combine Multiple Models
#' 
#' Given a set of models run with different parameters, 
#' return aggregated results to estimate population-level
#' values.
#' 
#' @param list_newmodels A list of models run over a set of 
#'   multiple parameters.
#' @param weights A vector of weigths, same length as the
#'   number of parameter sets.
#' @param oldmodel The original model.
#'   
#' @return A \code{combined_models} object, mostly similar
#'   to a result from \code{\link{run_models}}. \code{plot}
#'   and \code{summary} methods are available.
#'   
#' @keywords internal
combine_models <- function(list_newmodels, weights, oldmodel) {
  
  total_weights <- sum(weights)
  model_names <- names(list_newmodels)
  
  f <- function(x) {
    sum(x * weights / total_weights)
  }
  
  # collapse each new model values and counts
  
  list_res <- list()
  list_eval_models <- list()
    for (i in seq_along(list_newmodels)) {
    collapsed_values <- (list_newmodels[[i]]) %>% 
      dplyr::rowwise() %>% 
      dplyr::do_(~ get_total_state_values(.$.mod)) %>% 
      dplyr::ungroup() %>% 
      dplyr::summarise_all(f)
    
    tab_counts <- (list_newmodels[[i]]) %>% 
      dplyr::rowwise() %>% 
      dplyr::do_(.counts = ~ get_counts(.$.mod))

    tab_vals <- (list_newmodels[[i]]) %>% 
      dplyr::rowwise() %>% 
      dplyr::do_(.vals = ~ get_values(.$.mod))
        
    counts <- tab_counts$.counts %>% 
      mapply(
        weights,
        FUN = function(x, y) x * y / total_weights,
        SIMPLIFY = FALSE) %>% 
      Reduce(f = "+")

    vals <- tab_vals$.vals %>% 
      mapply(
        weights,
        FUN = function(x, y) x * y / total_weights,
        SIMPLIFY = FALSE) %>% 
      Reduce(f = "+")
    
        
    list_res <- c(
      list_res,
      list(collapsed_values)
    )
    
    list_eval_models <- 
      c(
        list_eval_models,
        setNames(list(list(counts = counts,
                           values = vals)),
                 model_names[i])
      )
  }
  
  for (i in seq_along(model_names)){
    list_res[[i]]$.model_names <- model_names[i]
  }
  res <- Reduce(dplyr::bind_rows, list_res) %>% 
    dplyr::mutate_(.dots = attr(oldmodel, "ce"))
  
  structure(
    res,
    class = c("combined_models", class(res)),
    base_model = get_base_model(oldmodel),
    eval_model_list = list_eval_models, 
    parameters = attr(oldmodel, "parameters"),
    init = attr(oldmodel, "init"),
    cycles = attr(oldmodel, "cycles"),
    method = attr(oldmodel, "method"),
    ce = attr(oldmodel, "ce"),
    base_model = attr(oldmodel, "base_model")
  )
}

#' @export
print.combined_models <- function(x, ...) {
  print(summary(x, ...))
}

#' @export
plot.combined_models <- function(x, ...) {
  plot.run_models(x, ...)
}

#' @export
summary.combined_models <- function(object, ...) {
  summary.run_models(object, ...)
}

normalize_ce.combined_models <- function(x, ...) {
  normalize_ce.run_models(x, ...)
}
