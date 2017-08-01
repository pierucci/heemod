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

#' Reindent Transition Matrix
#' 
#' @param x A transition matrix.
#' @param print Print result?
#'   
#' @return The reindented matrix as a text string,
#'   invisibly.
#' @export
reindent_transition <- function(x, print = TRUE) {
  if (! requireNamespace("stringr")) {
    stop("Package 'stringer' required.")
  }
  n_col <- get_matrix_order(x)
  sn <- paste0('"', get_state_names(x), '"')
  cells <- to_text_dots(x, name = FALSE)
  max_char <- pmax(
    nchar(sn),
    apply(matrix(cells, ncol = n_col, byrow = TRUE), 2,
          function (x) max(nchar(x)))
  )
  sn_pad <- stringr::str_pad(
    string = sn,
    width = max_char,
    side = "right"
  )
  cells_pad <- stringr::str_pad(
    string = cells,
    width = rep(max_char, length(sn)),
    side = "right"
  )
  res <- do.call(
    stringr::str_c,
    c(split(cells_pad, rep(seq_len(n_col), n_col)),
      sep = ", ",
      collapse = ",\n"))
  res <- paste0(
    "state_names = c(\n",
    paste(sn_pad, collapse = ", "),
    ")\n",
    res
  )
  if (print) {
    cat(res)
  }
  invisible(res)
}
