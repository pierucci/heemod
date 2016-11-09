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
#'   
#' @return \code{NULL}
#'   
#' @keywords internal
check_matrix <- function(x) {
  if (! isTRUE(all.equal(
    range(rowSums(x, dims = 2)),
    c(1, 1)))) {
    stop("Not all transition matrix rows sum to 1.")
  }
  
  if (! all(x >= 0 & x <= 1)) {
    stop("Some transition probabilities are outside the interval [0 - 1].")
  }
}

#' Evaluate Markov Model Transition Matrix
#' 
#' Evaluate a transition matrix using evaluated parameters.
#' 
#' Runs checks on the transition matrix during evaluation.
#' 
#' This functions has been heavily optimized, and thus can
#' be difficult to read. Good luck...
#' 
#' @param x an \code{uneval_matrix} object.
#' @param parameters an \code{eval_parameters} object.
#'   
#' @return An \code{eval_matrix} object (actually a list of 
#'   transition matrices, one per cycle).
#'   
#' @keywords internal
eval_matrix <- function(x, parameters) {
  tab_res <- mutate_(parameters, C = -pi, .dots = x)[names(x)]
  
  n <- get_matrix_order(x)
  
  array_res <- array(unlist(tab_res), dim = c(nrow(tab_res), n, n))
  # possible optimisation
  # dont transpose
  # but tweak dimensions in replace_C
  for(i in 1:nrow(tab_res)){
    array_res[i,,] <- t(array_res[i,,])
  }
  
  array_res <- replace_C(array_res)
  
  check_matrix(array_res)
  
  structure(
    split_along_dim(array_res, 1),
    class = c("eval_matrix", "list"),
    state_names = get_state_names(x)
  )
}

split_along_dim <- function(a, n) {
  # could be maybe optimized?
  setNames(lapply(
    split(a, arrayInd(seq_along(a), dim(a))[, n]),
    array, dim = dim(a)[-n], dimnames(a)[-n]),
    dimnames(a)[[n]])
}

replace_C <- function(x) {
  posC <- x == -pi
  
  if (! all(rowSums(posC, dims = 2) <= 1)) {
    stop("Only one 'C' is allowed per matrix row.")
  }
  
  x[posC] <- 0
  
  valC <- 1 - rowSums(x, dims = 2)[which(posC, arr.ind = TRUE)[, -3]] 
  x[posC] <- valC
  x
}

get_state_names.eval_matrix <- function(x, ...){
  attr(x, "state_names")
}

get_matrix_order.eval_matrix <- function(x){
  ncol(x[[1]])
}
