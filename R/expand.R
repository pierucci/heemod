#' @export
has_state_cycle <- function(x, ...) {
  UseMethod("has_state_cycle")
}

#' @export
has_state_cycle.uneval_matrix <- function(x, ...) {
  unlist(lapply(x, function(y) "state_cycle" %in% all.vars(y$expr)))
}

#' @export
has_state_cycle.uneval_state_list <- function(x, ...) {
  unlist(lapply(x, has_state_cycle))
}

#' @export
has_state_cycle.state <- function(x, ...) {
  any(unlist(lapply(x, function(y) "state_cycle" %in% all.vars(y$expr))))
}

substitute_dots <- function(.dots, .values) {
  lazyeval::as.lazy_dots(
    lapply(.dots, lazyeval::interp, .values = .values)
  )
}

expand_state <- function(x, ...) {
  UseMethod("expand_state")
}

#' @export
expand_state.uneval_matrix <- function(x, state_pos,
                                       state_name, cycles, n = 1) {
  L <- length(x)
  N <- sqrt(L)
  
  if (n <= cycles) {
    # positions to insert 0
    i <- seq(0, L - 1, N) + state_pos
    i[state_pos] <- i[state_pos] - 1
    res <- insert(x, i, list(lazyeval::lazy(0)))
    
    # row to duplicate
    new <- res[seq(
      from = get_tm_pos(state_pos, 1, N+1),
      to = get_tm_pos(state_pos, N+1, N+1))]
    
    # edit state_cycle
    new <- substitute_dots(new, list(state_cycle = n))
    
    # and reinsert
    res <- insert(res, (N+1)*(state_pos-1),
                  new)
    
    sn <- get_state_names(x)
    sn[state_pos] <- sprintf(".%s_%i", state_name, n)
    sn <- insert(sn, state_pos, sprintf(".%s_%i", state_name, n + 1))
    
    tm_ext <- define_matrix_(res, sn)
    
    expand_state(
      x = tm_ext,
      state_pos = state_pos + 1,
      state_name = state_name,
      n = n + 1,
      cycles = cycles
    )
  } else {
    x[get_tm_pos(state_pos, 1, N):get_tm_pos(state_pos, N, N)] <-
      substitute_dots(
        x[get_tm_pos(state_pos, 1, N):get_tm_pos(state_pos, N, N)],
        list(state_cycle = n)
      )
    x
  }
}

#' @export
expand_state.uneval_state_list <- function(x, state_name, cycles) {
  
  st <- x[[state_name]]
  x[state_name] <- NULL
  
  id <- seq_len(cycles + 1)
  res <- lapply(
    id,
    function(x) substitute_dots(st, list(state_cycle = x))
  )
  names(res) <- sprintf(".%s_%i", state_name, id)
  
  structure(
    c(x, res),
    class = class(x)
  )
}
