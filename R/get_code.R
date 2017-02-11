#' Display the Code to Generate an Object
#' 
#' This function returns the \code{R} code to generate an
#' \code{heemod} object.
#' 
#' @param x An \code{heemod} object.
#' @param name character. Optional argument giving the name
#'   to assign to the object.
#' @param sub logical. Should states or models be referenced
#'   by name in \code{define_strategy} and \code{run_model}
#'   instead of including the entire code?
#' @param depth Depth of the function call.
#' @param n_space Number of space used for indentation.
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
#' m <- define_transition(
#'   C, .1,
#'   0, 1 
#' )
#' 
#' get_code(m)
get_code <- function(x, ...) {
  UseMethod("get_code")
}

left_pad <- function(x, n_pad, prefix = FALSE) {
  pad <- paste(rep(" ", n_pad), collapse = "")
  pre <- if (prefix) pad else ""
  paste0(pre, gsub("\\n", paste0("\\\n", pad), x))
}

#' @export
#' @rdname get_code
get_code.uneval_parameters <- function(x, name = NULL,
                                       depth = 0, n_space = 2,
                                       ...) {
  if (! is.null(name)) {
    name <- paste(name, "<- ")
  }
  structure(
    left_pad(paste0(
      name,
      "define_parameters(\n  ",
      paste(to_text_dots(x), collapse = ",\n  "),
      "\n)",
      collapse = ""
    ), n_pad = depth * n_space),
    class = "heemod_code"
  )
}

#' @export
#' @rdname get_code
get_code.uneval_matrix <- function(x, name = NULL,
                                   depth = 0, n_space = 2,
                                   ...) {
  if (! is.null(name)) {
    name <- paste(name, "<- ")
  }
  n <- get_matrix_order(x)
  ex <- to_text_dots(x, name = FALSE)
  sn <- get_state_names(x)
  
  structure(
    left_pad(paste0(
      name,
      "define_transition(\n  ",
      "state_names = ",
      paste0("c(\"", paste(sn, collapse = "\", \""), "\"),\n  "),
      paste(
        apply(matrix(ex, nrow = n, byrow = TRUE), 1, paste, collapse = ", "),
        collapse = ",\n  "
      ),
      "\n)",
      collapse = ""
    ), n_pad = depth * n_space),
    class = "heemod_code"
  )
}

#' @export
#' @rdname get_code
get_code.state <- function(x, name = NULL,
                           depth = 0, n_space = 2,
                           ...) {
  if (! is.null(name)) {
    name <- paste(name, "<- ")
  }
  
  structure(
    left_pad(paste0(
      name,
      "define_state(\n  ",
      paste(to_text_dots(x), collapse = ",\n  "),
      "\n)",
      collapse = ""
    ), n_pad = 0),
    class = "heemod_code"
  )
}

#' @export
#' @rdname get_code
get_code.uneval_state_list <- function(x,
                                       depth = 0, n_space = 2,
                                       ...) {
  
  structure(
    left_pad(paste(
      names(x),
      unlist(lapply(x, get_code, depth = depth)),
      sep = " = ",
      collapse = ",\n"
    ), n_pad = depth * n_space, prefix = TRUE),
    class = "heemod_code"
  )
}

#' @export
#' @rdname get_code
get_code.uneval_model <- function(x, name = NULL, sub = FALSE,
                                  depth = 0, n_space = 2,
                                  ...) {
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
    st <- get_code(x$states, depth = depth)
  }
  
  structure(
    left_pad(paste0(
      name,
      "define_strategy(\n  ",
      "transition = ",
      get_code(get_transition(x), depth = depth),
      ",\n",
      st,
      "\n)"
    ), n_pad = depth * n_space),
    class = "heemod_code"
  )
}

#' @export
#' @rdname get_code
get_code.run_model <- function(x, name = NULL, sub = FALSE,
                                depth = 0, n_space = 2,
                                ...) {
  if (! is.null(name)) {
    name <- paste(name, "<- ")
  }
  
  if (sub) {
    md <- paste(
      names(x$uneval_strategy_list),
      make_names(paste0("m_", names(x$uneval_strategy_list))),
      sep = " = ",
      collapse = ",\n"
    )
  } else {
    
    md <- paste(
      names(x$uneval_strategy_list),
      unlist(lapply(x$uneval_strategy_list,
                    get_code, depth = depth + 1)),
      sep = " = ",
      collapse = ",\n  "
    )
  }
  
  structure(
    left_pad(paste0(
      name,
      "run_model(\n  ",
      md,
      ",\n  ",
      "parameters = ",
      get_code(get_parameters(x), depth = depth + 1),
      ",\n  ",
      "init = ",
      paste0("c(", paste(get_init(x), collapse = ", "), "),\n  ", collapse = ""),
      "cycles = ",
      get_cycles(x),
      ",\n  ",
      "method = ",
      paste0("\"", get_method(x), "\"", sep = ""),
      ",\n  ",
      "cost = ",
      deparse(get_ce(x)[[1]]$expr, width.cutoff = 500L),
      ",\n  ",
      "effect = ",
      deparse(get_ce(x)[[2]]$expr, width.cutoff = 500L),
      "\n)"
    ), n_pad = depth * n_space),
    class = "heemod_code"
  )
  
}

#' @export
print.heemod_code <- function(x, ...) {
  cat(x)
}
