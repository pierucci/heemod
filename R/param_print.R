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
