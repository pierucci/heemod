#' @export
print.state <- function(x, ...) {
  cat(sprintf(
    "A state with %i value%s.\n\n",
    length(x), plur(length(x))))
  
  nv <- names(x)
  ex <- unlist(lapply(x, function(y) deparse(y$expr)))
  
  cat(paste(nv, ex, sep = " = "), sep = "\n") 
}

#' @export
print.uneval_state_list <- function(x, ...) {
  n_state <- get_state_number(x)
  n_values <- length(get_state_value_names(x))
  
  cat(sprintf(
    "A list of %i state%s with %i value%s each.\n\n",
    n_state,
    plur(n_state),
    n_values,
    plur(n_values)
  ))
  cat("State names:\n\n")
  cat(get_state_names(x), sep = "\n")
  
  cat("\nState values:\n\n")
  cat(get_state_value_names(x), sep = "\n")
}

#' @export
print.eval_state_list <- function(x, ...) {
  cat(sprintf(
    "%i evaluated state%s, %i Markov cycle%s.\n",
    length(x),
    plur(length(x)),
    nrow(x[[1]]),
    plur(nrow(x[[1]]))
  ))
}
