#' Iteratively Evaluate a Markov Model With New Parameter 
#' Values
#' 
#' Given a data.frame with on set of new parameters values 
#' per row, iteratively evaluate the model over the set of 
#' new values.
#' 
#' New parameters with a missing value (\code{NA}) do not 
#' replace existing parameters.
#' 
#' @param x Result from \code{\link{run_models}}.
#' @param model Name or index of model to recompute.
#' @param newdata a data.frame whose names match parameters 
#'   names. \code{model} will be evaluated iteratively, 
#'   taking successivel values from each row.
#'   
#' @return A data.frame containing the values of 
#'   \code{newdata} and each Markov Model evaluation in 
#'   \code{res}.
#' 
#' @example inst/examples/example_eval_model_newdata.R
#' 
eval_model_newdata <- function(x, model = 1, newdata) {
  check_model_index(x = x, i = model)
  
  old_parameters <- attr(x, "parameters")
  cycles <- attr(x, "cycles")
  init <- attr(x, "init")
  method <- attr(x, "method")
  uneval_model <- attr(x, "uneval_model_list")[[model]]
  
  eval_newdata <- function(new_parameters, model, old_parameters) {
    new_parameters <- Filter(function(x) !is.na(x), new_parameters)
    
    lazy_new_param <- do.call(lazyeval::lazy_dots, new_parameters)
    
    parameters <- modifyList(
      old_parameters,
      lazy_new_param
    )
    
    eval_model(
      model = model,
      parameters = parameters,
      cycles = cycles,
      init = init,
      method = method
    )
  }
  
  newdata %>% 
    dplyr::rowwise() %>% 
    dplyr::do(
      .mod = eval_newdata(
        .,
        model = uneval_model,
        old_parameters = old_parameters
      )
    ) %>% 
    dplyr::bind_cols(newdata)
}

if(getRversion() >= "2.15.1") utils::globalVariables(c("."))


#' Iteratively Run Markov Models Over New Parameter Sets 
#' (Heterogeneity or Probabilistic analysis)
#' 
#' Given a table of new parameter values with a new 
#' parameter set per line, runs iteratively Markov models 
#' over these sets.
#' 
#' @param x The result of \code{\link{run_models}}.
#' @param newdata A data.frame of new parameter sets, one 
#'   column per parameter and one row per parameter set. Can
#'   be prespecified for heterogeneity analysis or randomly 
#'   drawn with resample for probability analysis.
#'   
#' @return A \code{data.frame} with one row per model/value.
#' @export
#' 
#' @example inst/examples/example_run_newdata.R
#'   
run_newdata <- function(x, newdata) {
  
  if (! any(class(x) %in% "eval_model_list")) {
    stop("Object 'x' must be the result of 'run_models()'.")
  }
  
  ce <- attr(x, "ce")
  
  list_res <- lapply(
    names(x),
    function(n) eval_model_newdata(x, model = n, newdata = newdata)
  )
  
  for (n in names(list_res)) {
    list_res[[n]]$.model_names <- n
  }
  
  res <- Reduce(dplyr::bind_rows, list_res)
  res <- dplyr::mutate_(res, .dots = ce)
  
  structure(res, class = c("eval_newdata", class(res)))
}
