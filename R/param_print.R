#' @export
print.uneval_parameters <- function(x, ...) {
  cat(sprintf("%i unevaluated parameter%s.\n\n",
              length(x), plur(length(x))))
  n <- names(x)
  ex <- unlist(lapply(x, function(y) deparse(y$expr, width.cutoff = 500L)))
  stopifnot(
    length(n) == length(ex)
  )
  cat(paste(n, ex, sep = " = "), sep = "\n")
}

#' @export
print.eval_parameters <- function(x, width = Inf, ...) {
  cat(sprintf(
    "%i evaluated parameter%s, %i Markov cycle%s.\n\n",
    ncol(x) - 1,
    plur(ncol(x) - 1),
    nrow(x),
    plur(nrow(x))
  ))
  
  print(dplyr::as.tbl(x), width = width, ...)
}
