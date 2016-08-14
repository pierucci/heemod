#' Run Sensitivity Analysis
#' 
#' @param model An evaluated Markov model
#' @param sensitivity An object returned by 
#'   \code{\link{define_sensitivity}}.
#'   
#' @return A \code{data.frame} with one row per model and
#'   parameter value.
#' @export
#' 
#' @example inst/examples/example_run_sensitivity.R
run_sensitivity <- function(model, sensitivity) {
  
  if (! all(c(".cost", ".effect") %in% names(model))) {
    stop("No cost and/or effect defined, sensitivity analysis unavailable.")
  }
  
  init <- attr(model, "init")
  cycles <- attr(model, "cycles")
  method <- attr(model, "method")
  names_models <- get_model_names(model)
  
  list_res <- lapply(
    names_models,
    function(n) eval_model_newdata(model, model = n, newdata = sensitivity)
  )
  
  for (i in seq_along(names_models)) {
    list_res[[i]]$.model_names <- names_models[i]
  }
  
  res <- Reduce(dplyr::bind_rows, list_res)
  res <- res %>% 
    dplyr::rowwise() %>% 
    dplyr::do(get_total_state_values(.$.mod)) %>% 
    dplyr::bind_cols(res)
  
  res <- dplyr::mutate_(res, .dots = attr(model, "ce"))
  
  structure(
    res,
    class = c("eval_sensitivity", class(res)),
    variables = attr(sensitivity, "variables"),
    model_ref = model
  )
}
