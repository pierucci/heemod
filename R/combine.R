#' Run Models on Demographic Data
#'
#' Run a model on a set of demographic data giving population
#' characteristics per strata, return aggregated results
#' to estimate population-level values.
#' 
#' @param x Result from \code{\link{run_models}}.
#' @param demographics A table containing demographic
#' data (see details).
#' 
#' The demographic data table must be a \code{data.frame} with
#' the following properties: the column names must be parameter 
#' names used in \code{\link{define_parameters}}; and an optional
#' column \code{.weights} can give the respective weight of
#' each row in the target population.
#' 
#' Weights are automatillcally scaled. If no weights are provided
#' equal weights are used for each strata.
#'
#' @return A \code{run_demographics} object, mostly
#' similar to a result from \code{\link{run_models}}. \code{plot} and
#' \code{summary} methods are available.
#'
combine_models <- function(list_newmodels, weights, oldmodel) {
  
  total_weights <- sum(weights)
  model_names <- names(list_newmodels)
  
  f <- function(x) {
    sum(x * weights / total_weights)
  }
  
  # collapse each new model values and counts
  
  list_res <- list()
  list_counts <- list()
  for (i in seq_along(list_newmodels)) {
    collapsed_values <- (list_newmodels[[i]]) %>% 
      dplyr::rowwise() %>% 
      dplyr::do_(~ get_total_state_values(.$.mod)) %>% 
      dplyr::ungroup() %>% 
      dplyr::summarise_all(f) %>% 
      dplyr::mutate_(.dots = attr(oldmodel, "ce"))
    
    tab_counts <- (list_newmodels[[i]]) %>% 
      dplyr::rowwise() %>% 
      dplyr::do_(.counts = ~ get_counts(.$.mod))
    
    counts <- tab_counts$.counts %>% 
      mapply(
        weights,
        FUN = function(x, y) x * y / total_weights,
        SIMPLIFY = FALSE) %>% 
      Reduce(f = "+")
    
    list_res <- c(
      list_res,
      list(collapsed_values)
    )
    
    list_counts <- c(
      list_counts,
      setNames(list(list(counts = counts)), model_names[i])
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
    eval_model_list = list_counts,
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
