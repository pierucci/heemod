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
#' @param model An \code{uneval_model} object.
#' @param cycles positive integer. Number of Markov Cycles 
#'   to compute.
#' @param init numeric vector, same length as number of 
#'   model states. Number of individuals in each model state
#'   at the beginning.
#' @param newdata a data.frame whose names match parameters 
#'   names. \code{model} will be evaluated iteratively, 
#'   taking successivel values from each row.
#' @param method Counting method.
#'   
#' @return A data.frame containing the values of 
#'   \code{newdata} and each Markov Model evaluation in 
#'   \code{res}.
#'   
#'   
#' @examples
#' 
#' \dontrun{
#' par1 <- define_parameters(
#'   a = 1,
#'   b = 1 / (markov_cycle + a)
#' )
#' 
#' mat1 <- define_matrix(
#'   1-b, b,
#'   0, 1
#' )

#' mod1 <- define_model(
#'   parameters = par1,
#'   transition_matrix = mat1,
#'   define_state(var = a),
#'   define_state(var = a * markov_cycle)
#' )
#' 
#' new_tab <- data.frame(
#'   a = 1:10
#' )
#' 
#' eval_model_newdata(
#'   mod1,
#'   cycles = 5,
#'   init = 1:0,
#'   newdata = new_tab
#' )
#' }
#' 
eval_model_newdata <- function(model, cycles,
                               init, method,
                               newdata) {
  
  eval_newdata <- function(new_params, model) {
    new_params <- Filter(function(x) !is.na(x), new_params)
    
    lazy_new_param <- do.call(lazyeval::lazy_dots, new_params)
    
    model$parameters <- modifyList(
      get_parameters(model),
      lazy_new_param
    )
    eval_model(
      model = model,
      cycles = cycles,
      init = init,
      method = method
    )
  }
  
  dplyr::bind_cols(
    newdata,
    dplyr::do(
      dplyr::rowwise(newdata),
      get_total_state_values(eval_newdata(., model))
    )
  )
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
#' @param init Initial number of individual per state. Not 
#'   needed when working on \code{\link{run_models}} 
#'   results.
#' @param cycles Number of Markov cycles to compute. Not 
#'   needed when working on \code{\link{run_models}} 
#'   results.
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
run_newdata <- function(x, init, cycles, newdata) {
  
  stopifnot(
    any(class(x) %in% "eval_model_list")
  )
  list_models <- attr(x, "uneval_model_list")
  
  stopifnot(
    missing(init),
    missing(cycles)
  )
  
  init <- attr(x, "init")
  cycles <- attr(x, "cycles")
  method <- attr(x, "method")
  
  list_res <- lapply(list_models, eval_model_newdata, method = method,
                     init = init, cycles = cycles, newdata = newdata)
  
  for (n in names(list_res)) {
    list_res[[n]]$.model_name <- n
  }
  
  res <- Reduce(dplyr::bind_rows, list_res)
  
  structure(res, class = c("eval_newdata", class(res)))
}
