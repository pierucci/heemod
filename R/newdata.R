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
#' @keywords internal
eval_model_newdata <- function(x, model = 1, newdata) {
  check_model_index(x = x, i = model)
  
  cycles <- attr(x, "cycles")
  init <- attr(x, "init")
  method <- attr(x, "method")
  old_parameters <- attr(x, "parameters")
  uneval_model <- attr(x, "uneval_model_list")[[model]]
  
  newdata %>% 
    dplyr::rowwise() %>% 
    dplyr::do_(
      .mod = ~ eval_newdata(
        .,
        model = uneval_model,
        old_parameters = old_parameters,
        cycles = cycles,
        init = init,
        method = method
      )
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::bind_cols(
      newdata
    )
}

eval_newdata <- function(new_parameters, model, old_parameters,
                         cycles, init, method) {
  
  new_parameters <- Filter(
    function(x) all(! is.na(x)),
    new_parameters
  )
  
  lazy_new_param <- lazyeval::as.lazy_dots(new_parameters)
  
  parameters <- utils::modifyList(
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
