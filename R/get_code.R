#' Display the Code to Generate an Object
#' 
#' This function returns the \code{R} code to generate an \code{heemod} oj
#'
#' @param x An \code{heemod} object.
#' @param name character. Optional argument giving the name to assign to the object.
#' @param sub logical. Should states or models be referenced by name in \code{define_model}
#'  and \code{run_model} instead of including the entire code?
#' @param ... Additional parameters passed to methods.
#'
#' @return A character string.
#' @export
#'
#' @examples
#' 
#' p <- define_parameters(
#'   a = 2,
#'   b = 3 * x + 1
#' )
#' 
#' get_code(p)
#' 
#' m <- define_matrix(
#'   C, .1,
#'   0, 1 
#' )
#' 
#' get_code(m)
#' 
get_code <- function(x, ...) {
  UseMethod("get_code")
}
# 
# p <- define_parameters(
#   a = 2,
#   b = 2 * x + 1
# )

to_text_dots <- function(x, name = TRUE) {
  n <- names(x)
  ex <- unlist(lapply(x, function(y) deparse(y$expr, width.cutoff = 500L)))
  stopifnot(
    length(n) == length(ex)
  )
  
  if (name) {
    paste(n, ex, sep = " = ")
  } else {
    ex
  }
}

#' @export
#' @rdname get_code
get_code.uneval_parameters <- function(x, name = NULL, ...) {
  if (! is.null(name)) {
    name <- paste(name, "<- ")
  }
  structure(
    paste0(
      name,
      "define_parameters(\n  ",
      paste(to_text_dots(x), collapse = ",\n  "),
      "\n)",
      collapse = ""
    ),
    class = "heemod_code"
  )
}

#' @export
#' @rdname get_code
get_code.uneval_matrix <- function(x, name = NULL, ...) {
  if (! is.null(name)) {
    name <- paste(name, "<- ")
  }
  n <- get_matrix_order(x)
  ex <- to_text_dots(x, name = FALSE)
  sn <- get_state_names(x)
  
  structure(
    paste0(
      name,
      "define_matrix(\n  ",
      "state_names = ",
      paste0("c(\"", paste(sn, collapse = "\", \""), "\"),\n  "),
      paste(
        apply(matrix(ex, nrow = n, byrow = TRUE), 1, paste, collapse = ", "),
        collapse = ",\n  "
      ),
      "\n)",
      collapse = ""
    ),
    class = "heemod_code"
  )
}

#' @export
#' @rdname get_code
get_code.state <- function(x, name = NULL, ...) {
  if (! is.null(name)) {
    name <- paste(name, "<- ")
  }
  
  structure(
    paste0(
      name,
      "define_state(\n  ",
      paste(to_text_dots(x), collapse = ",\n  "),
      "\n)",
      collapse = ""
    ),
    class = "heemod_code"
  )
}

#' @export
#' @rdname get_code
get_code.uneval_state_list <- function(x, ...) {
  structure(
    paste(
      names(x),
      unlist(lapply(x, get_code)),
      sep = " = ",
      collapse = ",\n  "
    ),
    class = "heemod_code"
  )
}

#' @export
#' @rdname get_code
get_code.uneval_model <- function(x, name = NULL, sub = FALSE, ...) {
  if (! is.null(name)) {
    name <- paste(name, "<- ")
  }
  
  if (sub) {
    st <- paste(
      names(x$states),
      make_names(paste0("s_", names(x$states))),
      sep = " = ",
      collapse = ",\n  "
    )
  } else {
    st <- get_code(x$states)
  }
  
  structure(
    paste0(
      name,
      "define_model(\n  ",
      "transition_matrix = ",
      get_code(x$transition_matrix),
      ",\n  ",
      st,
      "\n)"
    ),
    class = "heemod_code"
  )
}

#' @export
#' @rdname get_code
get_code.eval_model_list <- function(x, name = NULL, sub = FALSE, ...) {
  if (! is.null(name)) {
    name <- paste(name, "<- ")
  }
  
  if (sub) {
    md <- paste(
      names(attr(x, "uneval_model_list")),
      make_names(paste0("m_", names(attr(x, "uneval_model_list")))),
      sep = " = ",
      collapse = ",\n  "
    )
  } else {
    md <- paste(
      names(attr(x, "uneval_model_list")),
      unlist(lapply(attr(x, "uneval_model_list"), get_code)),
      sep = " = ",
      collapse = ",\n  "
    )
  }
  
  structure(
    paste0(
      name,
      "run_models(\n  ",
      md,
      ",\n  ",
      "parameters = ",
      get_code(attr(x, "parameters")),
      ",\n  ",
      "init = ",
      paste0("c(", paste(attr(x, "init"), collapse = ", "), "),\n  ", collapse = ""),
      "cycles = ",
      attr(x, "cycles"),
      ",\n  ",
      "method = ",
      paste0("\"", attr(x, "method"), "\"", sep = ""),
      ",\n  ",
      "base_model = ",
      paste0("\"", attr(x, "base_model"), "\"", sep = ""),
      ",\n  ",
      "cost = ",
      deparse(attr(x, "ce")[[1]]$expr, width.cutoff = 500L),
      ",\n  ",
      "effect = ",
      deparse(attr(x, "ce")[[2]]$expr, width.cutoff = 500L),
      "\n)"
    ),
    class = "heemod_code"
  )
  
}

#' @export
print.heemod_code <- function(x, ...) {
  cat(x)
}
