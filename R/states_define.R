#' Define a Markov Model State
#' 
#' Define the values characterising a Markov Model state for
#' 1 cycle.
#' 
#' As with \code{\link{define_parameters}}, state values are
#' defined sequencially. Later state definition can thus
#' only refer to values defined earlier.
#' 
#' For the \code{modify} function, existing values are 
#' modified, no new values can be added. Values order 
#' matters since only values defined earlier can be 
#' referenced in later expressions.
#' 
#' @param ... Name-value pairs of expressions defining state
#'   values.
#' @param .OBJECT An object of class \code{state}.
#' @param .dots Used to work around non-standard evaluation.
#'   
#' @return An object of class \code{state} (actually a named
#'   list of \code{lazy} expressions).
#' @export
#' 
#' @example inst/examples/example_define_state.R
#'   
define_state <- function(...) {
  .dots <- lazyeval::lazy_dots(...)
  
  define_state_(.dots)
}

#' @export
#' @rdname define_state
define_state_ <- function(.dots) {
  check_names(names(.dots))
  structure(.dots,
            class = c("state", class(.dots)))
}

#' @export
#' @rdname define_state
modify.state <- function(.OBJECT, ...) {
  .dots <- lazyeval::lazy_dots(...)
  
  modify_(.OBJECT = .OBJECT, .dots = .dots)
}

modify_.state <- function(.OBJECT, .dots) {
  check_names(names(.dots))
  # !mod!
  # message d'erreur informatif quand valeurs pas dans
  # bon ordre
  
  if (! all(names(.dots) %in% names(.OBJECT))) {
    stop(sprintf(
      "The following state values are not defined: %s.",
      names(.dots)[names(.dots) %in% names(.OBJECT)]
    ))
  }
  
  utils::modifyList(.OBJECT, .dots)
}

#' Define Markov Model State List
#' 
#' Define the states of a Markov model by combining 
#' \code{state} objects.
#' 
#' State names have to correspond to those specified through
#' \code{\link{define_transition}}.
#' 
#' All states should have the same value names.
#' 
#' The \code{modify} function can modify existing states or 
#' add new ones.
#' 
#' @param ... Name-value pairs of expressions defining model
#'   states.
#' @param .OBJECT An \code{uneval_states} object.
#' @param .dots List of states, only used by 
#'   \code{define_state_list_} to avoid using \code{...}.
#'   
#' @return An object of class \code{uneval_state_list} (a 
#'   list of \code{state} objects).
#'   
#' @examples
#' \dontrun{
#' s1 <- define_state(cost = 1, util = 1)
#' s2 <- define_state(cost = 3, util = .4)
#' 
#' states_mod <- define_state_list(
#'   healthy = s1,
#'   sick = s2
#' )
#' 
#' states_mod
#' 
#' s1_bis <- define_state(cost = 0, util = 1)
#' s3 <- define_state(cost = 10, util = .1)
#' 
#' modify(
#'   states_mod,
#'   healthy = s1_bis,
#'   sicker = s3
#' )
#' }
#'   
#' @keywords internal
define_state_list <- function(...) {
  .dots <- list(...)
  
  define_state_list_(.dots)
}

#' @rdname define_state_list
define_state_list_ <- function(.dots) {
  
  state_names <- names(.dots)
  
  if (is.null(state_names)) {
    message("No named state -> generating names.")
    state_names <- LETTERS[seq_along(.dots)]
    names(.dots) <- state_names
  }
  
  if (any(state_names == "")) {
    warning("Not all states are named -> generating names.")
    state_names <- LETTERS[seq_along(.dots)]
    names(.dots) <- state_names
  }
  
  if (any(duplicated(names(.dots)))) {
    stop("Some state names are duplicated.")
  }
  
  if (! all(unlist(lapply(.dots,
                          function(x) "state" %in% class(x))))) {
    
    .x <- names(.dots)[! unlist(lapply(
      .dots,
      function(x) "state" %in% class(x)))]
    
    stop(sprintf(
      "Incorrect state object%s: %s",
      plur(length(.x)),
      paste(.x, collapse = ", ")
    ))
  }
  
  check_states(.dots)
  
  structure(
    .dots,
    class = c("uneval_state_list", class(.dots))
  )
}

#' @rdname define_state_list
modify.uneval_state_list <- function(.OBJECT, ...) {
  .dots <- list(...)
  
  modify_(.OBJECT = .OBJECT, .dots = .dots)
}

modify_.uneval_state_list <- function(.OBJECT, .dots) {
  res <- utils::modifyList(.OBJECT, .dots)
  check_states(res)
  
  res
}

#' Check Model States for Consistency
#' 
#' For internal use.
#' 
#' All states should have the same value names.
#' 
#' @param x An object of class \code{uneval_states}.
#'   
#' @return \code{NULL}
#'   
#' @keywords internal
check_states <- function(x){
  if (! list_all_same(lapply(x, length))) {
    stop("Number of state values differ between states.")
  }
  
  if (! list_all_same(lapply(x, function(y) sort(names(y))))) {
    stop("State value names differ between states.")
  }
  NULL
}

#' Return Number of State
#' 
#' For internal use.
#' 
#' Work with both \code{uneval_states} and
#' \code{eval_states}.
#' 
#' @param x An object containing states.
#'   
#' @return An integer: number of states.
#'   
#' @keywords internal
get_state_number <- function(x){
  # !mod!
  # rename get_state_count
  length(get_state_names(x))
}

#' Return Names of State Values
#' 
#' @param x An object containing states.
#' @param ... Additional arguments passed to methods.
#'   
#' @return A character vector of state value names.
#'   
#' @keywords internal
get_state_value_names <- function(x){
  UseMethod("get_state_value_names")
}

get_state_value_names.uneval_state_list <- function(x) {
  names(x[[1]])
}

get_state_value_names.state <- function(x){
  names(x)
}

#' Get State Names
#' 
#' Retrieve state names from an object containing states.
#' 
#' @param x An object containing states.
#' @param ... Additional arguments passed to methods.
#'   
#' @return A character vector of state names.
#'   
#' @keywords internal
get_state_names <- function(x, ...){
  UseMethod("get_state_names")
}

get_state_names.default <- function(x, ...){
  names(x)
}
