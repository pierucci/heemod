#' Check Markov Model Transition Matrix
#' 
#' Check whether a matrix fullfills the conditions to be a 
#' transition matrix.
#' 
#' This function is called by [eval_transition()]
#' and should not be used directly.
#' 
#' Checks whether all rows sum to 1 and all probabilities 
#' are between 0 and 1.
#' 
#' @param x a matrix.
#'   
#' @return `NULL`
#'   
#' @keywords internal
check_matrix <- function(x) {
  stopifnot(inherits(x, "array"))
  stopifnot(length(dim(x)) == 3)
  
  if (! isTRUE(all.equal(
    range(rowSums(x, dims = 2)),
    c(1, 1)))) {
    problem_rows <- which(rowSums(x, dims = 2) != 1, arr.ind = TRUE)
    problem_rows <- data.frame(
      cycle = problem_rows[,1], 
                               state = get_state_names(x)[problem_rows[,2]])
    problem_rows <- format.data.frame(problem_rows, justify = "left")
    
    stop(sprintf(
      "Not all transition matrix rows sum to 1:\n%s",
      paste(sprintf(
        "cycle: %s, state: %s",
        problem_rows[,1],
        problem_rows[,2]),
        collapse = "\n")
    ))
    
    
  }
  
  if (! all(x >= 0 & x <= 1)) {
    problem <- which(x < 0 | x > 1, arr.ind = TRUE)
    problem <- data.frame(problem)
    names(problem) <- c("cycle", "from", "to")
    states <- get_state_names(x)
    problem$from <- states[problem$from]
    problem$to <- states[problem$to]
    problem <- format.data.frame(problem, justify = "left")
    
    stop(sprintf(
      "Some transition probabilities are outside the interval [0 - 1]:\n%s",
      paste(sprintf(
        "cycle: %s, from: %s, to: %s",
        problem$cycle, problem$from, problem$to),
        collapse = "\n")
    ))
    
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
#' @param x an `uneval_matrix` object.
#' @param parameters an `eval_parameters` object.
#'   
#' @return An `eval_matrix` object (actually a list of 
#'   transition matrices, one per cycle).
#'   
#' @keywords internal
eval_transition <- function(x, ...) {
  UseMethod("eval_transition")
}

eval_transition.uneval_matrix <- function(x, parameters) {
  
  # update calls to dispatch_strategy()
  x <- dispatch_strategy_hack(x)
  
  tab_res <- dplyr::mutate_(
    parameters,
    .dots = c(
      lazyeval::lazy_dots(C = -pi),
      x))[names(x)]
  
  n <- get_matrix_order(x)
  
  array_res <- array(unlist(tab_res), dim = c(nrow(tab_res), n, n))
  # possible optimisation
  # dont transpose
  # but tweak dimensions in replace_C
  for(i in 1:nrow(tab_res)){
    array_res[i,,] <- t(array_res[i,,])
  }
  
  array_res <- structure(replace_C(array_res),
                         state_names = get_state_names(x))
  
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
get_state_names.array <- function(x, ...){
  attr(x, "state_names")
}

get_matrix_order.eval_matrix <- function(x){
  ncol(x[[1]])
}
