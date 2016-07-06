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
  list_models <- attr(model, "uneval_model_list")
  
  list_res <- lapply(list_models, eval_model_newdata,
                     method = method, old_parameters = get_parameters(model),
                     init = init, cycles = cycles, newdata = sensitivity)
  for (n in names(list_res)) {
    list_res[[n]]$.model_names <- n
  }
  
  res <- Reduce(dplyr::bind_rows, list_res)
  
  res <- dplyr::mutate_(res, .dots = attr(model, "ce"))
  
  structure(
    res,
    class = c("eval_sensitivity", class(res)),
    variables = attr(sensitivity, "variables"),
    model_ref = model
  )
}
