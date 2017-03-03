#' Define Markov Model Parameters
#' 
#' Define parameters called to compute the transition matrix
#' or state values for a Markov model. Parameters can be 
#' time dependent by using the `markov_cycle` 
#' parameter.
#' 
#' Parameters are defined sequentially, parameters defined 
#' earlier can be called in later expressions.
#' 
#' Vector length should not be explicitly set, but should 
#' instead be stated relatively to `markov_cycle` 
#' (whose length depends on the number of simulation 
#' cycles). Alternatively, `dplyr` functions such as 
#' `n()` or `row_numbers()` can be used.
#' 
#' This function relies heavily on the `dplyr` package.
#' Parameter definitions should thus mimic the use of 
#' functions such as `mutate`.
#' 
#' Variable names are searched first in the parameter 
#' definition (only parameters defined earlier are visible) 
#' then in the environment where `define_parameters` 
#' was called.
#' 
#' For the `modify` function, existing parameters are 
#' modified, but no new parameter can be added. Parameter 
#' order matters since only parameters defined earlier can 
#' be referenced in later expressions.
#' 
#' @param ... Name-value pairs of expressions definig 
#'   parameters.
#' @param .OBJECT An object of class 
#'   `uneval_parameters`.
#' @param .dots Used to work around non-standard evaluation.
#'   
#' @return An object of class `uneval_parameters` 
#'   (actually a named list of `lazy` expressions).
#' @export
#' 
#' @example inst/examples/example_define_parameters.R
#'   
define_parameters <- function(...) {
  .dots <- lazyeval::lazy_dots(...)
  define_parameters_(.dots)
}

#' @rdname define_parameters
#' @export
define_parameters_ <- function(.dots) {
  
  if (length(.dots)){
    check_names(names(.dots))
  }
  structure(.dots,
            class = c("uneval_parameters", class(.dots)))
}

#' Return parameters names
#' 
#' Extract parameters names.
#' 
#' @param x An object with parameters.
#'   
#' @return A character vector of parameter names.
#'   
#' @keywords internal
get_parameter_names <- function(x) {
  UseMethod("get_parameter_names")
}

get_parameter_names.updated_model <- function(x) {
  get_parameter_names(get_model(x))
}

get_parameter_names.uneval_parameters <- function(x) {
  names(x)[! names(x) %in% c("markov_cycle", "strategy",
                             "model_time")]
}

get_parameter_names.eval_parameters <- function(x) {
  get_parameter_names.uneval_parameters(x)
}

get_parameter_names.run_model <- function(x) {
  get_parameter_names(get_parameters(x))
}

#' Modify Object
#' 
#' This generic function allows the modification of various 
#' objects such as parameters, transitions matrix or states.
#' 
#' More details are available on the respective help page of
#' each object definition.
#' 
#' @param .OBJECT Various objects.
#' @param ... Modifications.
#'   
#' @return Same class as `x`.
#' @export
#' 
modify <- function(.OBJECT, ...) {
  UseMethod("modify")
}

modify_ <- function(.OBJECT, .dots, ...) {
  UseMethod("modify_")
}

#' @export
#' @rdname define_parameters
modify.uneval_parameters <- function(.OBJECT, ...) {
  .dots <- lazyeval::lazy_dots(...)
  
  modify_(.OBJECT = .OBJECT, .dots = .dots)
}

modify_.uneval_parameters <- function(.OBJECT, .dots) {
  
  check_names(names(.dots))
  
  utils::modifyList(.OBJECT, .dots)
}


#' Define Inflow for a BIA
#' 
#' This function is a placeholder.
#' 
#' This function only takes constant values. Eventually
#' time-dependant expression will be accepted (with
#' model-time dependency only).
#' 
#' @param ... Name-value pairs of expressions definig
#'   inflow.
#'   
#' @return An object similar to the return value of
#'   [define_parameters()].
#' @export
#' 
define_inflow <- function(...) {
  # placeholder
  # eventually should be like
  # define_parameters()
  c(...)
}
