#' Define Transition Matrix for Markov Model
#' 
#' Define a matrix of transition probabilities. Probability 
#' can depend on parameters defined with 
#' [define_parameters()], and can thus be 
#' time-dependent.
#' 
#' Parameters names are searched first in a parameter object
#' defined with [define_parameters()] and linked 
#' with the matrix through [define_strategy()]; 
#' then in the environment where the matrix was defined.
#' 
#' Matric cells are listed by row.
#' 
#' The completary probability of all other row probabilities
#' can be conveniently reffered as `C`.
#' 
#' Only matrix size is checked during this step (the matrix 
#' must be square). Other conditions (such as row sums being
#' equal to 1) are tested later, during model evaluation.
#' 
#' For the `modify` function existing matrix cells are 
#' replaced with the new expression. Cells are referenced by
#' name. Cell naming follows the `cell_x_y` convention,
#' with `x` being the row number and `y` the 
#' column number.
#' 
#' @param ... Name-value pairs of expressions definig matrix
#'   cells. Can refer to parameters defined with 
#'   [define_parameters()]. For `plot`, 
#'   additional arguments passed to
#'   [diagram::plotmat()].
#' @param state_names character vector, optional. State 
#'   names.
#' @param .OBJECT An object of class `uneval_matrix`.
#' @param x An `uneval_matrix` to plot.
#' @param relsize Argument passed to
#'   [diagram::plotmat()].
#' @param shadow.size Argument passed to 
#'   [diagram::plotmat()].
#' @param latex Argument passed to
#'   [diagram::plotmat()].
#' @param .dots Used to work around non-standard evaluation.
#'   
#' @return An object of class `uneval_matrix` (actually
#'   a named list of `lazy` expressions).
#' @export
#' 
#' @example inst/examples/example_define_transition.R
#'   
define_transition <- function(..., state_names) {
  .dots <- lazyeval::lazy_dots(...)
  
  if (missing(state_names)) {
    message("No named state -> generating names.")
    state_names <- LETTERS[seq_len(sqrt(length(lazyeval::lazy_dots(...))))]
  }
  
  define_transition_(.dots = .dots, state_names = state_names)
}

#' @rdname define_transition
define_transition_ <- function(.dots, state_names) {
  
  n <- sqrt(length(.dots))
  
  if (! is.wholenumber(n)) {
    stop(sprintf(
      "Impossible to build a square matrix with %i elements.",
      length(.dots)))
  }
  
  if (! length(state_names) == n) {
    stop(sprintf(
      "Length of 'state_names' (%i) and size of matrix (%i x %i) differ.",
      length(state_names), n, n
    ))
  }
  
  if (! length(unique(state_names)) == length(state_names)) {
    stop("A least one state name is duplicated.")
  }
  
  names(.dots) <- sprintf("cell_%i_%i",
                          rep(seq_len(n), each = n),
                          rep(seq_len(n), n))
  
  structure(.dots,
            class = c("uneval_matrix", class(.dots)),
            state_names = as.vector(state_names))
}

get_state_names.uneval_matrix <- function(x, ...){
  attr(x, "state_names")
}

#' Return Markov Model Transition Matrix Order
#' 
#' A generic that works both with `uneval_matrix` and
#' `eval_matrix`.
#' 
#' For internal use.
#' 
#' @param x A transition matrix, evaluated or not.
#'   
#' @return An integer: matrix order.
#'   
#' @keywords internal
get_matrix_order <- function(x){
  UseMethod("get_matrix_order")
}

get_matrix_order.uneval_matrix <- function(x){
  sqrt(length(x))
}

#' @export
#' @rdname define_transition
modify.uneval_matrix <- function(.OBJECT, ...){
  
  # !mod!
  # modifier par rr simplment
  
  .dots <- lazyeval::lazy_dots(...)
  
  modify_(.OBJECT = .OBJECT, .dots = .dots)
}

modify_.uneval_matrix <- function(.OBJECT, .dots){
  
  # !mod!
  # modifier par rr simplment
  
  if (! all(names(.dots) %in% names(.OBJECT))) {
    stop(sprintf(
      "Trying to modify undefined cells (%s).",
      paste(names(.dots)[! names(.dots) %in% names(.OBJECT)], collapse = ", ")
    ))
  }
  
  utils::modifyList(.OBJECT, .dots)
}
