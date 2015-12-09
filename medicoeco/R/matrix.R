#' Title
#'
#' @param ... 
#' @param state_names 
#'
#' @return
#' @export
#'
#' @examples
define_matrix <- function(
  ...,
  state_names = LETTERS[seq_len(sqrt(length(list(...))))]
) {
  .dots <- lazyeval::lazy_dots(...)
  n <- sqrt(length(.dots))
  
  stopifnot(
    is.wholenumber(n),
    length(state_names) == n,
    length(unique(state_names)) == length(state_names)
  )
  
  names(.dots) <- sprintf("cell_%i_%i",
                          seq_len(n) %>% rep(each = n),
                          seq_len(n) %>% rep(n))
  
  structure(.dots,
            class = c("uneval_matrix", class(.dots)),
            names_states = state_names)
}

#' Title
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
check_matrix <- function(x, ...) {
  info <- list(...)
  if (! all(all.equal(
    rowSums(x),
    rep(1, nrow(x)))) |
    ! all(x >= 0 & x <= 1)) {
    print(info)
    stop("Incorrect matrix!")
  }
}

#' Title
#'
#' @param x 
#' @param parameters 
#'
#' @return
#' @export
#'
#' @examples
eval_matrix <- function(x, parameters) {
  
  tab_res <- mutate_(parameters, .dots = x)[names(x)]
  
  f <- function(...) {
    res <- matrix(c(...), byrow = TRUE, nrow = matrix_order(x))
    check_matrix(res)
    list(res)
  }
  
  # bottleneck!
  res <- tab_res %>%
    map_rows(f, .labels = FALSE) %$%
    .out %>%
    unlist(recursive = FALSE)
  
  structure(res,
            class = c("eval_matrix", class(res)),
            names_states = names_states(x))
}


names_states.uneval_matrix <- function(x, ...)
  attr(x, "names_states")

names_states.eval_matrix <- function(x, ...)
  attr(x, "names_states")

matrix_order <- function(x, ...)
  UseMethod("matrix_order")

matrix_order.uneval_matrix <- function(x, ...)
  sqrt(length(x))

matrix_order.eval_matrix <- function(x, ...)
  ncol(x[[1]])

update.uneval_matrix <- function(x, ...){
  .dots <- lazyeval::lazy_dots(...)
  
  stopifnot(
    all(names(.dots) %in% names(x))
  )
  
  modifyList(x, .dots)
}

print.uneval_matrix <- function(x, ...) {
  cat(sprintf(
    "An unevaluated matrix, %i states.\n\n",
    matrix_order(x)
  ))
  
  ex <- unlist(lapply(x, function(y) deparse(y$expr)))
  print(matrix(ex,
               byrow = TRUE,
               ncol = matrix_order(x),
               dimnames = list(names_states(x),
                               names_states(x))),
        quote = FALSE,
        ...)
}

print.eval_matrix <- function(x, ...) {
  cat(sprintf(
    "An evaluated matrix, %i states, %i markov cycles.\n\n",
    matrix_order(x),
    length(x)
  ))
  
  cat("State names:\n\n")
  cat(names_states(x), sep = "\n")
  cat("\n")
  
  print(head(x, ...))
  
  if (length(head(x, ...)) < length(x))
    cat("...\n")
}


