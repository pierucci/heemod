#' Simulate Iteratively a Markov Model Over New Parameter
#' Values
#' 
#' This function name should change.
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
#'   
#' @return A data.frame containing the values of \code{newdata} and each Markov Model evaluation in \code{res}.
#' @export
#' 
simulate_cohort_iter <- function(model, cycles,
                                 init, newdata) {
  
  eval_newdata <- function(new_params, model) {
    
    model$parameters <- modifyList(
      get_parameters(model),
      do.call(lazyeval::lazy_dots, new_params)
    )
    simulate_cohort(
      model = model,
      cycles = cycles,
      init = init
    )
  }
  tab %>%
    rowwise %>%
    do(res = eval_newdata(., model)) %>%
    bind_cols(newdata)
}
