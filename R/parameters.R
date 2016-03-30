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

#' @export
#' @rdname define_parameters
define_parameters_ <- function(.dots) {
  
  if (length(.dots)){
    check_names(names(.dots))
  }
  structure(.dots,
            class = c("uneval_parameters", class(.dots)))
}

#' Evaluate Markov model parameters
#' 
#' Evaluate parameters specified through 
#' \code{define_parameters}, for a given number of cycles.
#' 
#' @param x an \code{uneval_parameters} object.
#' @param cycles integer. Number of cycles to simulate.
#'   
#' @return An object of class \code{eval_parameters}
#'   (actually a data.frame with one column per parameter
#'   and one row per cycle).
#' 
#' @examples
#' 
#' \dontrun{
#' param <- define_parameters(
#'   age_start = 60,
#'   age = age_start + markov_cycle
#' )
#' 
#' eval_parameters(param, cycles = 15)
#' }
#' 
eval_parameters <- function(x, cycles = 1) {
  # other datastructure?
  res <- dplyr::mutate_(
    data.frame(
      markov_cycle = seq_len(cycles)
    ),
    .dots = x
  )
  
  structure(
    res,
    class = c("eval_parameters", class(res))
  )
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
  
  modifyList(.OBJECT, .dots)
}

#' @export
print.uneval_parameters <- function(x, ...) {
  cat(sprintf("%i unevaluated parameter%s.\n\n",
              length(x), plur(length(x))))
  n <- names(x)
  ex <- unlist(lapply(x, function(y) deparse(y$expr, width.cutoff = 500L)))
  stopifnot(
    length(n) == length(ex)
  )
  cat(paste(n, ex, sep = " = "), sep = "\n")
}

#' @export
print.eval_parameters <- function(x, width = Inf, ...) {
  cat(sprintf(
    "%i evaluated parameter%s, %i Markov cycle%s.\n\n",
    ncol(x) - 1,
    plur(ncol(x) - 1),
    nrow(x),
    plur(nrow(x))
  ))
  
  print(dplyr::as.tbl(x), width = width, ...)
}
