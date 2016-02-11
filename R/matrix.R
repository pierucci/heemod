#' Define Transition Matrix for Markov Model
#' 
#' Define a matrix of transition probabilities. Probability 
#' can depend on parameters defined with 
#' \code{\link{define_parameters}}, and can thus be 
#' time-dependent.
#' 
#' Parameters names are searched first in a parameter object
#' defined with \code{\link{define_parameters}} and linked 
#' with the matrix through \code{\link{define_model}}; then 
#' in the environment where the matrix was defined.
#' 
#' Matric cells are listed by row.
#' 
#' The completary probability of all other row probabilities
#' can be conveniently reffered as \code{C}.
#' 
#' Only matrix size is checked during this step (the matrix 
#' must be square). Other conditions (such as rowsums being 
#' equal to 1) are tested later, during model evaluation.
#' 
#' For the \code{modify} function existing matrix cells are 
#' replaced with the new expression. Cells are referenced by
#' name. Cell naming follows the \code{cell_x_y} convention,
#' with \code{x} being the row number and \code{y} the 
#' column number.
#' 
#' @param ... Name-value pairs of expressions definig matrix
#'   cells. Can refer to parameters defined with 
#'   \code{\link{define_parameters}}. For \code{plot},
#'   additional arguments passed to \code{digram::plotmat}.
#' @param state_names character vector, optional. State 
#'   names.
#' @param .OBJECT An object of class \code{uneval_matrix}.
#' @param x An \code{uneval_matrix} to plot.
#' @param relsize Argument passed to \code{\link{plotmat}}.
#' @param shadow.size Argument passed to \code{\link{plotmat}}.
#'   
#' @return An object of class \code{uneval_matrix} (actually
#'   a named list of \code{lazy} expressions).
#' @export
#' 
#' @example inst/examples/example_define_matrix.R
#' 
define_matrix <- function(
  ...,
  state_names
) {
  .dots <- lazyeval::lazy_dots(...)
  
  if (missing(state_names)) {
    message("No named state -> generating names.")
    state_names <- LETTERS[seq_len(sqrt(length(lazyeval::lazy_dots(...))))]
  }
  
  define_matrix_(.dots = .dots, state_names = state_names)
}

define_matrix_ <- function(
  .dots,
  state_names
) {
  
  n <- sqrt(length(.dots))
  
  stopifnot(
    is.wholenumber(n),
    length(state_names) == n,
    length(unique(state_names)) == length(state_names)
  )
  
  names(.dots) <- sprintf("cell_%i_%i",
                          rep(seq_len(n), each = n),
                          rep(seq_len(n), n))
  
  structure(.dots,
            class = c("uneval_matrix", class(.dots)),
            state_names = state_names)
}

#' Check Markov Model Transition Matrix
#' 
#' Check whether a matrix fullfills the conditions to be a 
#' transition matrix.
#' 
#' This function is called by \code{\link{eval_matrix}} and 
#' should not be used directly.
#' 
#' Checks whether all rows sum to 1 and all probabilities
#' are between 0 and 1.
#' 
#' @param x a matrix.
#' @param ... A list of informations to print when checks 
#'   fail, for debugging purposes.
#'   
#' @return NULL
#'   
check_matrix <- function(x, ...) {
  info <- list(...)

  stopifnot(
    isTRUE(
      all.equal(rowSums(x), rep(1, nrow(x)))
    ),
    all(x >= 0 & x <= 1)
  )
}

#' Evaluate Markov Model Transition Matrix
#' 
#' Evaluate a transition matrix using evaluated parameters.
#' 
#' Runs checks on the transition matrix during evaluation.
#' 
#' @param x an \code{uneval_matrix} object.
#' @param parameters an \code{eval_parameters} object.
#'   
#' @return An \code{eval_matrix} object (actually a list of 
#'   transition matrix, one per cycle).
eval_matrix <- function(x, parameters) {
  
  tab_res <- mutate_(parameters, C = -pi, .dots = x)[names(x)]
  
  n <- get_matrix_order(x)
  
  f <- function(...) {
    res <- matrix(c(...),
                  byrow = TRUE,
                  nrow = n)
    posC <- res == -pi
    
    stopifnot(
      rowSums(posC) <= 1
    )
    res[posC] <- 0
    valC <- 1 - rowSums(res)[rowSums(posC) == 1]
    res[posC] <- valC
    
    check_matrix(res)
    list(res)
  }
  
  # bottleneck!
  
  res <- unlist(
    dplyr::do(
      dplyr::rowwise(tab_res),
      res = f(unlist(.))
    )$res,
    recursive = FALSE
  )

  structure(res,
            class = c("eval_matrix", class(res)),
            state_names = get_state_names(x))
}

get_state_names.uneval_matrix <- function(x, ...){
  attr(x, "state_names")
}

get_state_names.eval_matrix <- function(x, ...){
  attr(x, "state_names")
}

#' Return Markov Model Transition Matrix Order
#' 
#' A generic that works both with
#' \code{uneval_matrix} and \code{eval_matrix}.
#' 
#' For internal use.
#'
#' @param x A transition matrix, evaluated or not.
#'
#' @return An integer: matrix order.
get_matrix_order <- function(x){
  UseMethod("get_matrix_order")
}

get_matrix_order.uneval_matrix <- function(x){
  sqrt(length(x))
}

get_matrix_order.eval_matrix <- function(x){
  ncol(x[[1]])
}

#' @export
#' @rdname define_matrix
modify.uneval_matrix <- function(.OBJECT, ...){
  
  # !mod!
  # modifier par rr simplment
  
  .dots <- lazyeval::lazy_dots(...)
  
  modify_(.OBJECT = .OBJECT, .dots = .dots)
}

modify_.uneval_matrix <- function(.OBJECT, .dots){
  
  # !mod!
  # modifier par rr simplment
  
  stopifnot(
    all(names(.dots) %in% names(.OBJECT))
  )
  
  modifyList(.OBJECT, .dots)
}

to_char_uneval_matrix <- function(x) {
  ex <- unlist(lapply(x, function(y) deparse(y$expr)))
  ex[ex == "C"] <- ""
  matrix(ex,
         byrow = TRUE,
         ncol = get_matrix_order(x),
         dimnames = list(get_state_names(x),
                         get_state_names(x)))
}

#' @export
print.uneval_matrix <- function(x, ...) {
  cat(sprintf(
    "An unevaluated matrix, %i states.\n\n",
    get_matrix_order(x)
  ))
  
  res <- to_char_uneval_matrix(x)
  
  print(res,
        quote = FALSE,
        ...)
}

#' @export
print.eval_matrix <- function(x, ...) {
  cat(sprintf(
    "An evaluated matrix, %i states, %i markov cycles.\n\n",
    get_matrix_order(x),
    length(x)
  ))
  
  cat("State names:\n\n")
  cat(get_state_names(x), sep = "\n")
  cat("\n")
  
  print(head(x, ...))
  
  if (length(head(x, ...)) < length(x))
    cat("...\n")
}

#' @export
#' @rdname define_matrix
plot.uneval_matrix <- function(x, relsize = .75, shadow.size = 0, curve = 0,  ...) {
  op <- graphics::par(mar = c(0, 0, 0, 0))
  res <- to_char_uneval_matrix(x)
  diagram::plotmat(t(res[rev(seq_len(nrow(res))),rev(seq_len(nrow(res)))]),
  relsize = relsize, shadow.size = shadow.size, curve = curve, ...)
  graphics::par(op)
}
