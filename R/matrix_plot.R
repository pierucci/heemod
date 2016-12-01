to_char_uneval_matrix <- function(x) {
  ex <- unlist(lapply(x, function(y) deparse(y$expr, width.cutoff = 500)))
  ex[ex == "0"] <- ""
  matrix(ex,
         byrow = TRUE,
         ncol = get_matrix_order(x),
         dimnames = list(get_state_names(x),
                         get_state_names(x)))
}

#' @export
print.uneval_matrix <- function(x, ...) {
  cat(sprintf(
    "A transition matrix, %i states.\n\n",
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
    "An evaluated transition matrix, %i states, %i markov cycles.\n\n",
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
#' @rdname define_transition
plot.uneval_matrix <- function(x, relsize = .75,
                               shadow.size = 0,
                               latex = TRUE, ...) {
  if (! requireNamespace("diagram")) {
    stop("'diagram' package required for transition plot.")
  }
  op <- graphics::par(mar = c(0, 0, 0, 0))
  res <- to_char_uneval_matrix(x)
  diagram::plotmat(
    t(res[rev(seq_len(nrow(res))),
          rev(seq_len(nrow(res)))]),
    relsize = relsize, shadow.size = shadow.size,
    absent = "",
    latex = latex, ...
  )
  graphics::par(op)
}
