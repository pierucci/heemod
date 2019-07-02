#' @export
print.state <- function(x, ...) {
  val <- x$.dots
  start <- x$starting_values
  nb_sv <- lapply(start, function(x){
    x$expr != 0
  }) %>% 
    unlist() %>%
    sum()
  
  phrase_start <- ifelse(nb_sv > 0, sprintf(" and %i starting value%s", nb_sv, plur(nb_sv)), "")
  cat(sprintf(
    "A state with %i value%s%s.\n\n",
    length(val), plur(length(val)),
    phrase_start))
  
  nv <- names(val)
  ex <- lapply(val, function(y) paste(deparse(y$expr), collapse = "\n"))
  
  cat(paste(nv, ex, sep = " = "), sep = "\n")
  if (nb_sv > 0){
    nv <- names(start)
    ex <- lapply(seq_along(start), function(i) {
      if (start[[i]]$expr > 0) paste(names(start)[i], deparse(start[[i]]$expr), collapse = "\n", sep = " = ")
    })
    cat("Start", paste(ex[lengths(ex) != 0]), sep = "\n")
  }
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
