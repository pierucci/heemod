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
#' Vector length should not be explicitely set, but should 
#' instead be stated relatively to \code{markov_cycle} 
#' (whose length depends on the number of simulation 
#' cycles). Alternatively, \code{dplyr} functions such as 
#' \code{n()} or \code{row_numbers()} can be used.
#' 
#' This function relies heavily on the \code{dplyr} and 
#' \code{lazy_eval} packages. Parameter definitions should 
#' thus mimic the use of functions such as \code{mutate}.
#' 
#' Variable names are searched first in the parameter 
#' definition (only parameters defined earlier are visible) 
#' then in the environment where \code{define_parameters} 
#' was called.
#' 
#' For the \code{modify} function, existing parameters are 
#' modified, new parameters are added at the end by default 
#' if \code{BEFORE} is not specified. Parameter order 
#' matters since only parameters defined earlier can be 
#' referenced in later expressions.
#' 
#' @param ... Name-value pairs of expressions definig
#'   parameters.
#' @param x An object of class \code{uneval_parameters}.
#' @param BEFORE character, length 1. Name of parameters 
#'   before which new parameters are to be added.
#'   
#' @return An object of class \code{uneval_parameters} 
#'   (actually a named list of \code{lazy} expressions).
#' @export
#' 
#' @examples
#' 
#' # parameter 'age' depends on time:
#' # simulating a cohort starting at 60 yo
#' 
#' define_parameters(
#'   age_start = 60,
#'   age = age_start + markov_cycle
#' )
#' 
#' # other uses of markov_cycle are possible
#' 
#' define_parameters(
#'   top_time = ifelse(markov_cycle < 10, 1, 0)
#' )
#'   
#' # more elaborate: risk function
#' 
#' define_parameters(
#'   rate = 1 - exp(- markov_time * .5)
#' )
#' 
#' \dontrun{
#' # dont explicitely state lengths
#' define_parameters(
#'   var = seq(1, 15, 2)
#' )
#' }
#' 
#' # instead rely on markov_cycle or dplyr 
#' # functions such as n() or row_number()
#' 
#' define_parameters(
#'  var = seq(from = 1, length.out = n(), by = 3),
#'  var2 = seq(1, length(markov_cycle), 2)
#' )
#' 
#' param <- define_parameters(
#'   age_start = 60,
#'   age = age_start + markov_cycle
#' )
#' 
#' # adding new parameters
#' 
#' modify(
#'   param,
#'   const = 4.4,
#'   age_2 = age ^ 2
#' )
#' 
#' # modify existing parameters
#' 
#' modify(
#'   param,
#'   age_start = 40
#' )
#' 
#' # specify new parameter position
#' 
#' modify(
#'   param,
#'   var = 3.14,
#'   BEFORE = "age"
#' )
#' 
define_parameters <- function(...) {
  .dots <- lazyeval::lazy_dots(...)
  
  stopifnot(
    all(names(.dots) != "markov_cycle")
  )
  
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
#' @export
#' 
#' @examples
#' param <- define_parameters(
#'   age_start = 60,
#'   age = age_start + markov_cycle
#' )
#' 
#' eval_parameters(param, cycles = 15)
#' 
eval_parameters <- function(x, cycles = 1) {
  # other datastructure?
  res <- data.frame(
    markov_cycle = seq(from = 0, to = cycles - 1)
  ) %>%
    mutate_(.dots = x)
  
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
#' @export
#' 
get_parameter_names <- function(x) {
  names(x)[names(x) != "markov_chain"]
}


#' Modify Object
#' 
#' This generic function allows the modification of various 
#' objects such as parameters, transitions matrix or states.
#' 
#' More details are available on the respective help page of
#' each object definition.
#' 
#' @param x Various objects.
#' @param ... Modifications.
#'   
#' @return Same class as \code{x}.
#' @export
#' 
modify <- function(x, ...) {
  UseMethod("modify")
}

#' @export
#' @rdname define_parameters
modify.uneval_parameters <- function(x, ..., BEFORE) {
  .dots <- lazyeval::lazy_dots(...)
  
  # !mod!
  # message d'erreur informatif quand parametres pas dans
  # bon ordre
  #
  # voire correction automatique ?
  
  if (! missing(BEFORE)) {
    new_values <- setdiff(
      names(.dots),
      c("markov_chain", get_parameter_names(x))
    )
    res <- modifyList(x, .dots)
    
    pos_before <- which(names(res) == BEFORE)
    
    stopifnot(
      length(BEFORE) == 1
    )
    
    c(
      res[seq_len(pos_before - 1)],
      res[new_values],
      res[seq(from = pos_before, to = length(res))]
    )
  } else {
    modifyList(x, .dots)
  }
}

#' @export
print.uneval_parameters <- function(x, ...) {
  cat(sprintf("%i unevaluated parameter%s.\n\n",
              length(x), plur(length(x))))
  n <- names(x)
  ex <- unlist(lapply(x, function(y) deparse(y$expr)))
  
  paste(n, ex, sep = " = ") %>%
    cat(sep = "\n")
}

#' @export
print.eval_parameters <- function(x, ...) {
  cat(sprintf(
    "%i evaluated parameter%s, %i markov cycle%s.\n\n",
    ncol(x) - 1,
    plur(ncol(x) - 1),
    nrow(x),
    plur(nrow(x)),
  ))
  
  print(as.tbl(x), ...)
}
