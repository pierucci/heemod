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
  
  if (! isTRUE(all.equal(rowSums(x), rep(1, nrow(x))))) {
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
    valC <- 1 - rowSums(res)[which(posC, arr.ind = TRUE)[, 1]]
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

get_state_names.eval_matrix <- function(x, ...){
  attr(x, "state_names")
}

get_matrix_order.eval_matrix <- function(x){
  ncol(x[[1]])
}