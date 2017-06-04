#' @export
define_state_transition <- function(...) {
  if(is.na(from) && is.na(to)) stop("Either from or to must be defined.")
  .dots <- lazyeval::lazy_dots(...)
  
  define_state_transition_(.dots)
}

#' @export
define_state_transition_ <- function(.dots) {
  check_names(names(.dots))
  structure(
    .dots,
    class = c("state_transition", class(.dots))
  )
}
