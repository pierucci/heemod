has_state_cycle <- function(x, ...) {
  UseMethod("has_state_cycle")
}

#' @export
has_state_cycle.uneval_matrix <- function(x, ...) {
  unlist(lapply(x, function(y) "state_cycle" %in% all.vars(y$expr)))
}

#' @export
has_state_cycle.part_surv <- function(x, ...) {
  FALSE
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

#' Expand Time-Dependant States into Tunnel States
#' 
#' This function for transition matrices and state values 
#' expands states relying on \code{state_cycle} in a serie
#' of tunnels states.
#' 
#' @param x A transition matrix or a state list.
#' @param state_pos Position of the state to expand.
#' @param state_name Original name of the sate to expand.
#' @param cycles Number of cycle of the model.
#' @param n Postition in the expansion process.
#' @param ... Addition parameters passed to methods.
#'   
#' @return The same object type as the input.
#' @keywords internal
expand_state <- function(x, ...) {
  UseMethod("expand_state")
}

#' @export
#' @rdname expand_state
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
    
    tm_ext <- define_transition_(res, sn)
    
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
#' @rdname expand_state
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


#' Convert Lazy Dots to Expression List
#' 
#' This function is used by \code{\link{interp_heemod}}.
#'
#' @param .dots A lazy dots object.
#'
#' @return A list of expression.
#' @keywords internal
as_expr_list <- function(.dots) {
  setNames(
    lapply(.dots, function(x) x$expr),
    names(.dots)
  )
}

#' Interpolate Lazy Dots
#' 
#' Sequencially interpolates lazy dots, optionnaly using 
#' external references.
#' 
#' The interpolation is sequencial: the second dot is 
#' interpolated using the first, the third using the 
#' interpolated first two, and so on.
#' 
#' @param x A parameter, transition matrix or state list
#'   object.
#' @param more A list of expressions.
#' @param ... Addition parameters passed to methods.
#'   
#' @return An interpolated lazy dots object.
#' @keywords internal
interp_heemod <- function(x, ...) {
  UseMethod("interp_heemod")
}

#' @export
#' @rdname interp_heemod
interp_heemod.default <- function(x, more = NULL, ...) {
  
  res <- NULL
  
  for (i in seq_along(x)) {
    to_interp <- x[[i]]
    for_interp <- c(more, as_expr_list(res))
    funs <- all.funs(to_interp$expr)
    
    if (any(pb <- funs %in% names(for_interp))) {
      stop(sprintf(
        "Some parameters are named like a function, this is incompatible with the use of 'state_cycle': %s.",
        paste(funs[pb], collapse = ", ")
      ))
    }
    
    new <- setNames(list(lazyeval::interp(
      to_interp,
      .values = for_interp
    )
    ), names(x)[i])
    res <- c(res, new)
    
  }
  lazyeval::as.lazy_dots(res)
}


#' @export
#' @rdname interp_heemod
interp_heemod.uneval_matrix <- function(x, ...) {
  res <- interp_heemod.default(x, ...)
  define_transition_(res, get_state_names(x))
}

#' @export
#' @rdname interp_heemod
interp_heemod.state <- function(x, ...) {
  res <- interp_heemod.default(x, ...)
  define_state_(res)
}

#' @export
#' @rdname interp_heemod
interp_heemod.part_surv <- function(x, ...) {
  x
}

#' @export
#' @rdname interp_heemod
interp_heemod.uneval_state_list <- function(x, ...) {
  for (i in seq_along(x)) {
    x[[i]] <- interp_heemod(x[[i]], ...)
  }
  x
}

all.funs <- function(expr) {
  with_funs <- table(all.names(expr))
  without_funs <- table(all.names(expr, functions = FALSE))
  
  with_funs[names(without_funs)] <-
    with_funs[names(without_funs)] -
    without_funs
  names(with_funs)[with_funs > 0]
}

complete_scl <- function(scl, state_names,
                         strategy_names, cycles) {
  uni <- FALSE
  if (is.numeric(scl) && length(scl) == 1 && is.null(names(scl))) {
    uni <- TRUE
    stopifnot(
      scl <= cycles,
      scl > 0,
      ! is.na(scl),
      is.wholenumber(scl)
    )
    cycles <- scl
  }
  
  res <- lapply(
    strategy_names,
    function(x) rep(cycles, length(state_names)) %>% 
      setNames(state_names)
  ) %>% 
    setNames(strategy_names)
  
  if (is.null(scl) || uni) {
    return(res)
  }
  
  check_scl <- function(scl, cycles) {
    if (is.null(names(scl))) {
      stop("'state_cycle_limit' must be named.")
    }
    if (any(duplicated(names(scl)))) {
      stop("'state_cycle_limit' names must be unique.")
    }
    if (any(pb <- ! names(scl) %in% state_names)) {
      stop(sprintf(
        "Some 'state_cycle_limit' names are not state names: %s.",
        paste(names(scl)[pb], collapse = ", ")
      ))
    }
    
    stopifnot(
      ! is.na(scl),
      scl > 0,
      scl <= cycles,
      is.wholenumber(scl)
    )
  }
  
  if (is.numeric(scl)) {
    check_scl(scl, cycles)
    for (i in seq_along(res)) {
      res[[i]][names(scl)] <- scl
    }
    return(res)
  }
  
  if (is.list(scl)) {
    if (any(pb <- ! names(scl) %in% strategy_names)) {
      stop(sprintf(
        "Some 'state_limit_cycle' names are not model names: %s.",
        paste(names(scl)[pb], collapse = ", ")
      ))
    }
    for (n in names(scl)) {
      check_scl(scl[[n]], cycles)
      
      res[[n]][names(scl[[n]])] <- scl[[n]]
    }
    return(res)
  }
  
  stop("'Incorrect 'state_cycle_limit' type.")
}
