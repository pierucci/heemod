define_states <- function(...) {
  .dots <- list(...)
  
  check_states(.dots)
  
  structure(.dots,
            class = c("uneval_states", class(.dots)))
}

print.uneval_states <- function(x, ...) {
  cat(sprintf(
    "A list of %i unevaluated states with %i values each.\n\n",
    state_count(x),
    names_state_values(x) %>% length
  ))
  cat("State names:\n\n")
  cat(names_states(x), sep = "\n")
  
  cat("\nState values:\n\n")
  cat(names_state_values(x), sep = "\n")
}

check_states <- function(x)
  stopifnot(
    list_all_same(lapply(x, length)),
    list_all_same(lapply(x, function(y) sort(names(y))))
  )

eval_states <- function(x, parameters) {
  
  f <- function(x) {
    # bottleneck!
    mutate_(parameters, .dots = x)[c("markov_cycle",
                                     names(x))]
  }
  
  res <- lapply(x, f)
  
  structure(res,
            class = c("eval_states", class(res)))
}

print.eval_states <- function(x, ...) {
  cat(sprintf(
    "A list of %i evaluated states with %i values each, %i markov cycles.\n\n",
    state_count(x),
    names_state_values(x) %>% length,
    nrow(x)[[1]]
  ))
  cat("State names:\n\n")
  cat(names_states(x), sep = "\n")
  
  cat("\nState values:\n\n")
  cat(names_state_values(x), sep = "\n")
}

state <- function(...) {
  .dots <- lazyeval::lazy_dots(...)
  structure(.dots,
            class = c("state", class(.dots)))
}

print.state <- function(x, ...) {
  cat(sprintf("An unevaluated state with %i values.\n\n", length(x)))
  
  nv <- names(x)
  ex <- unlist(lapply(x, function(y) deparse(y$expr)))
  
  paste(nv, ex, sep = " = ") %>%
    cat(sep = "\n") 
}

state_count <- function(x)
  length(names_states(x))

names_state_values <- function(x, ...)
  UseMethod("names_state_values")

names_state_values.uneval_states <- function(x)
  names(x[[1]])

names_state_values.eval_states <- function(x)
  names(x[[1]])[-1]

names_states <- function(x, ...)
  UseMethod("names_states")

names_states.default <- function(x, ...)
  names(x)

update.uneval_states <- function(x, ...){
  .dots <- lazyeval::lazy_dots(...)
  
  stopifnot(
    all(names(.dots) %in% names(x))
  )
  
  modifyList(x, .dots)
}

update.state <- function(x, ..., BEFORE) {
  .dots <- lazyeval::lazy_dots(...)
  
  if (! missing(BEFORE)) {
    new_values <- setdiff(names(.dots), names(x))
    res <- modifyList(x, .dots)
    
    pos_before <- which(names(res) == BEFORE)
    
    c(
      res[seq_len(pos_before - 1)],
      res[new_values],
      res[seq(from = pos_before, to = length(res))]
    )
  } else {
    modifyList(x, .dots)
  }
}
