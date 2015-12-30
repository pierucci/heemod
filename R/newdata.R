#' Iteratively Evaluate a Markov Model With New Parameter 
#' Values
#' 
#' Given a data.frame with on set of new parameters values 
#' per row, iteratively evaluate the model over the set of
#' new values.
#' 
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
#' @param count_args Additional arguments passed as a list 
#'   to \code{compute_counts}.
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
                               init, count_args = NULL,
                               newdata) {
  
  eval_newdata <- function(new_params, model) {
    
    model$parameters <- modifyList(
      get_parameters(model),
      do.call(lazyeval::lazy_dots, new_params)
    )
    eval_model(
      model = model,
      cycles = cycles,
      init = init,
      count_args = count_args
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
#' @return A list with one data.frame per model.
#' @export
#' 
#' @examples 
#' 
#' mod1 <-
#'   define_model(
#'     parameters = define_parameters(
#'       age_init = 60,
#'       age = age_init + markov_cycle
#'     ),
#'     transition_matrix = define_matrix(
#'       .5, .5,
#'       .1, .9
#'     ),
#'       define_state(
#'         cost = 543 + age * 5
#'       ),
#'       define_state(
#'         cost = 432 + age
#'       )
#'     
#'   )
#' 
#' mod2 <-
#'   define_model(
#'     parameters = define_parameters(
#'       age_init = 60,
#'       age = age_init + markov_cycle
#'     ),
#'     transition_matrix = define_matrix(
#'       .5, .5,
#'       .1, .9
#'     ),
#'       define_state(
#'         cost = 789 * age / 10
#'       ),
#'       define_state(
#'         cost = 456 * age / 10
#'       )
#'     
#'   )
#' 
#' res2 <- run_model(
#'   mod1, mod2,
#'   init = 1:0,
#'   cycles = 10
#' )
#' # generating table with new parameter sets
#' new_tab <- data.frame(
#'   age_init = 40:80
#' )
#' 
#' # with run_model result
#' ndt1 <- run_newdata(res2, newdata = new_tab)
#' 
run_newdata <- function(x, init, cycles, newdata) {
  
  stopifnot(
    class(x) %in% "eval_model_list"
  )
  list_models <- attr(x, "uneval_model_list")
  
  stopifnot(
    missing(init),
    missing(cycles)
  )
  
  init <- attr(x, "init")
  cycles <- attr(x, "cycles")
  
  res <- lapply(list_models, eval_model_newdata,
                init = init, cycles = cycles, newdata = newdata)
  return(res)
}
