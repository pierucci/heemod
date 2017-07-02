#' Define a Markov Model State
#' 
#' Define the values characterising a Markov Model state for
#' 1 cycle.
#' 
#' As with [define_parameters()], state values are
#' defined sequentially. Later state definition can thus
#' only refer to values defined earlier.
#' 
#' For the `modify` function, existing values are 
#' modified, no new values can be added. Values order 
#' matters since only values defined earlier can be 
#' referenced in later expressions.
#' 
#' @param ... Name-value pairs of expressions defining state
#'   values.
#' @param .OBJECT An object of class `state`.
#' @param .dots Used to work around non-standard evaluation.
#'   
#' @return An object of class `state` (actually a named
#'   list of `lazy` expressions).
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

#' Define a Markov Model State Transition
#' 
#' Define the values characterising a Markov Model trasition into,
#' out of, or between states.
#' 
#' As with [define_parameters()], state transition values are
#' defined sequentially. Later state definition can thus
#' only refer to values defined earlier.
#' 
#' For the `modify` function, existing values are 
#' modified, no new values can be added. Values order 
#' matters since only values defined earlier can be 
#' referenced in later expressions.
#' 
#' @param from Character vector of names of from-states for which
#' value is applied.  When not given, value is applied to any from
#' state.
#' @param to Character vector of names of to-states for which
#' value is applied.  When not given, value is applied to any to
#' state.
#' @param ... Name-value pairs of expressions defining state
#'   values.
#' @param .OBJECT An object of class `state_transition`.
#' @param .dots Used to work around non-standard evaluation.
#'   
#' @return An object of class `state_transition` (actually a named
#'   list of `lazy` expressions).
#' @export
define_state_transition <- function(from = NA, to = NA, ...) {
  if(any(from[!is.na(from)] %in% to[!is.na(from)])) {
    stop("State transition may not include same state both in 'from' and 'to'.")
  }
  .dots <- lazyeval::lazy_dots(...)
  define_state_transition_(from = from, to = to, .dots)
}

#' @export
#' @rdname define_state_transition
define_state_transition_ <- function(to, from, .dots) {
  check_names(names(.dots))
  structure(
    .dots,
    class = c("state_transition", class(.dots)),
    to = to,
    from = from
  )
}

#' @export
#' @rdname define_state_transition
modify.state_transition <- function(.OBJECT, ...) {
  .dots <- lazyeval::lazy_dots(...)
  
  modify_(.OBJECT = .OBJECT, .dots = .dots)
}

modify_.state_transition <- function(.OBJECT, .dots) {
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
#' `state` and `state_transition` objects.
#' 
#' State names have to correspond to those specified through
#' [define_transition()].  State transitions do not need to
#' be named.
#' 
#' All states should have the same value names.
#' 
#' The `modify` function can modify existing states and state
#' transitions or add new ones.
#' 
#' @param ... Name-value pairs of expressions defining model
#'   states and state transtiions.
#' @param .OBJECT An `uneval_states` object.
#' @param .dots List of states, only used by 
#'   `define_state_list_` to avoid using `...`.
#'   
#' @return An object of class `uneval_state_list` (a 
#'   list of `state` objects).
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
  
  # Separate states and state transitions
  st_sel <- sapply(.dots, function(x) "state_transition" %in% class(x))
  st_dots <- .dots[st_sel]
  st_names <- names(st_dots)
  st_states <- unlist(lapply(st_dots, function(x) c(attr(x, "from"), attr(x, "to"))))
  s_sel <- sapply(.dots, function(x) "state" %in% class(x))
  s_dots <- .dots[s_sel]
  s_names <- names(s_dots)
  
  if (is.null(s_names)) {
    message("No named state -> generating names.")
    s_names <- LETTERS[seq_along(s_dots)]
    names(s_dots) <- s_names
  }
  
  if (any(s_names == "")) {
    warning("Not all states are named -> generating names.")
    s_names <- LETTERS[seq_along(s_dots)]
    names(s_dots) <- s_names
  }
  
  if (any(duplicated(names(s_dots)))) {
    stop("Some state names are duplicated.")
  }
  
  if(length(st_dots) > 0) {
    # Check if any nonexistent states are referenced in state
    # transitions
    st_states_check <- !(st_states %in% s_names) & !is.na(st_states)
    invalid_state_refs <- st_states[st_states_check]
    if(length(invalid_state_refs) > 0) {
      stop(sprintf(
        "Invalid states%s reference in state_transition: %s",
        plur(length(invalid_state_refs)),
        paste(invalid_state_refs, collapse = ", ")
      ))
    }
  }
  
  if (! all(st_sel | s_sel)) {
    # Removed more detailed error message here since it didn't generalize
    # well to state transitions.
    stop("Invalid state object provided.")
  }
  
  check_states(.dots)
  
  res <- structure(
    s_dots,
    class = c("uneval_state_list", class(s_dots))
  )
  
  if(length(st_dots) > 0) {
    attr(res, "transitions") <- define_state_transition_list_(st_dots)
  }
  res
}

#' @rdname define_state_list
modify.uneval_state_list <- function(.OBJECT, ...) {
  .dots <- list(...)
  
  modify_(.OBJECT = .OBJECT, .dots = .dots)
}

modify_.uneval_state_list <- function(.OBJECT, .dots) {
  
  # Separate states and state transitions
  st_sel <- sapply(.dots, function(x) "state_transition" %in% class(x))
  st_dots <- .dots[st_sel]
  st_names <- names(st_dots)
  st_states <- unlist(lapply(.dots, function(x) c(attr(x, "from"), attr(x, "to"))))
  s_sel <- sapply(.dots, function(x) "state" %in% class(x))
  s_dots <- .dots[s_sel]
  s_names <- names(s_dots)
  
  # Update states
  res <- utils::modifyList(.OBJECT, s_dots)
  
  # Update state transitions
  if(!is.null(attr(.OBJECT, "transitions")) & length(st_dots) > 0) {
    attr(res, "transitions") <- utils::modifyList(attr(.OBJECT, "transitions"), st_dots)
  }
  
  check_states(append(res, attr(res, "transitions")))
  
  res
}


#' @export
#' @rdname define_state_transition
modify.state_transition <- function(.OBJECT, ...) {
  .dots <- lazyeval::lazy_dots(...)
  
  modify_(.OBJECT = .OBJECT, .dots = .dots)
}

modify_.state_transition <- function(.OBJECT, .dots) {
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

#' Define Markov Model State Transition List
#' 
#' Define the state transitions of a Markov model by combining 
#' `state_transition` objects.
#' 
#' All states transitions should have the same value names
#' as model states.
#' 
#' The `modify` function can modify existing state
#' transitions or add new ones.
#' 
#' @param ... expressions defining model state transtiions.
#' @param .OBJECT An `uneval_state_transition_list` object.
#' @param .dots List of states, only used by 
#'   `define_state_list_` to avoid using `...`.
#'   
#' @return An object of class `uneval_state_list` (a 
#'   list of `state` objects).
#'   
#' @examples
#' \dontrun{
#' s1 <- define_state_transition(from = "A", cost = 1, util = 1)
#' s2 <- define_state_transition(from = "B", cost = 3, util = .4)
#' 
#' states_trans_mod <- define_state_transition_list(
#'   s1,
#'   s2
#' )
#' 
#' states_mod
#' 
#' s1_bis <- define_state_transition_list(from = "A", cost = 0, util = 1)
#' s3 <- define_state(to = "A", cost = 10, util = .1)
#' 
#' modify(
#'   states_mod,
#'   s1_bis,
#'   s3
#' )
#' }
#'   
#' @keywords internal
define_state_transition_list <- function(...) {
  .dots <- list(...)
  
  define_state_transition_list_(.dots)
}

#' @rdname define_state_transition_list
define_state_transition_list_ <- function(.dots) {
  
  res <- structure(
    .dots,
    class = c("uneval_state_transition_list", class(.dots))
  )
  
  res
}

#' @rdname define_state_list
modify.uneval_state_transition_list <- function(.OBJECT, ...) {
  .dots <- list(...)
  
  modify_(.OBJECT = .OBJECT, .dots = .dots)
}

modify_.uneval_state_transition_list <- function(.OBJECT, .dots) {
  
  # Update states
  res <- utils::modifyList(.OBJECT, s_dots)
  
  res
}

#' Check Model States for Consistency
#' 
#' For internal use.
#' 
#' All states should have the same value names.
#' 
#' @param x An object of class `uneval_states`.
#'   
#' @return `NULL`
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
#' Work with both `uneval_states` and
#' `eval_states`.
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
