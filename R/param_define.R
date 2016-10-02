#' Define Markov Model Parameters
#' 
#' Define parameters called to compute the transition matrix
#' or state values for a Markov model. Parameters can be 
#' time dependent by using the \code{markov_cycle} 
#' parameter.
#' 
#' Parameters are defined sequencially, parameters defined 
#' earlier can be called in later expressions.
#' 
#' Vector length should not be explicitly set, but should 
#' instead be stated relatively to \code{markov_cycle} 
#' (whose length depends on the number of simulation 
#' cycles). Alternatively, \code{dplyr} functions such as 
#' \code{n()} or \code{row_numbers()} can be used.
#' 
#' This function relies heavily on the \code{dplyr} package.
#' Parameter definitions should thus mimic the use of 
#' functions such as \code{mutate}.
#' 
#' Variable names are searched first in the parameter 
#' definition (only parameters defined earlier are visible) 
#' then in the environment where \code{define_parameters} 
#' was called.
#' 
#' For the \code{modify} function, existing parameters are 
#' modified, but no new parameter can be added. Parameter 
#' order matters since only parameters defined earlier can 
#' be referenced in later expressions.
#' 
#' @param ... Name-value pairs of expressions definig 
#'   parameters.
#' @param .OBJECT An object of class 
#'   \code{uneval_parameters}.
#' @param .dots Used to work around non-standard evaluation.
#'   
#' @return An object of class \code{uneval_parameters} 
#'   (actually a named list of \code{lazy} expressions).
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
#' Extract parameters names from an \code{uneval_parameters}
#' or \code{eval_parameters} object.
#' 
#' @param x \code{uneval_parameters} or 
#'   \code{eval_parameters} object.
#'   
#' @return A character vector of parameter names.
#'   
#' @keywords internal
get_parameter_names <- function(x) {
  names(x)[names(x) != "markov_cycle"]
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
#' @return Same class as \code{x}.
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
  # !mod!
  # message d'erreur informatif quand parametres pas dans
  # bon ordre
  #
  
  stopifnot(
    all(names(.dots) %in% names(.OBJECT))
  )
  
  utils::modifyList(.OBJECT, .dots)
}
