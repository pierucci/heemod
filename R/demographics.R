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
#' @export
#' 
#' @example inst/examples/example_run_demographic.R
#'
run_demographics <- function(x, demographics) {
  if (".weights" %in% names(demographics)) {
    weights <- demographics$.weights
    demographics <- dplyr::select_(demographics, ~ - .weights)
    
  } else {
    message("No weights specified, using equal weights.")
    weights <- rep(1, nrow(demographics))
  }
  
  model_names <- get_model_names(x)
  
  # run each model against new data
  list_newmodels <- lapply(
    model_names,
    function(n) eval_model_newdata(
      x,
      model = n,
      newdata = demographics
    )
  )
  
  total_weights <- sum(weights)
  
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
      dplyr::mutate_(.dots = attr(x, "ce"))
    
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
    dplyr::mutate_(.dots = attr(x, "ce"))
  
  structure(
    res,
    class = c("run_demographics", class(res)),
    base_model = get_base_model(x),
    eval_model_list = list_counts,
    parameters = attr(x, "parameters"),
    init = attr(x, "init"),
    cycles = attr(x, "cycles"),
    method = attr(x, "method"),
    ce = attr(x, "ce"),
    base_model = attr(x, "base_model")
  )
}

#' @export
print.run_demographics <- function(x, ...) {
  print(summary(x, ...))
}

#' @export
plot.run_demographics <- function(x, ...) {
  plot.run_models(x, ...)
}

#' @export
summary.run_demographics <- function(object, ...) {
  summary.run_models(object, ...)
}

normalize_ce.run_demographics <- function(x, ...) {
  normalize_ce.run_models(x, ...)
}
