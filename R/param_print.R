#' @export
print.uneval_parameters <- function(x, ...) {
  cat(sprintf("%i unevaluated parameter%s.\n\n",
              length(x), plur(length(x))))
  res <- to_text_dots(x)
  cat(res, sep = "\n")
}
