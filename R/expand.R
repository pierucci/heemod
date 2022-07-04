has_state_time <- function(x, ...) {
  UseMethod("has_state_time")
}

#' @export
has_state_time.uneval_matrix <- function(x, ...) {
  unlist(lapply(x, function(y) "state_time" %in% all.vars(y$expr)))
}

has_state_time.uneval_parameters <- function(x, ...) {
  unlist(lapply(x, function(y) "state_time" %in% all.vars(y$expr)))
}

#' @export
has_state_time.part_surv <- function(x, ...) {
  FALSE
}

#' @export
has_state_time.uneval_state_list <- function(x, ...) {
  unlist(lapply(x, has_state_time))
}

#' @export
has_state_time.state <- function(x, ...) {
  any(unlist(lapply(x$.dots, function(y) "state_time" %in% all.vars(y$expr))))
}

substitute_dots <- function(.dots, .values) {
  lazyeval::as.lazy_dots(
    lapply(.dots, lazyeval::interp, .values = .values)
  )
}

#' Expand Time-Dependant States into Tunnel States
#' 
#' This function for transition matrices and state values 
#' expands states relying on `state_time` in a serie
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
  m <-  matrix(list(lazyeval::lazy(0)), nrow = cycles + N, ncol = cycles + N)

  tm <- matrix(x,
               byrow = TRUE,
               ncol = get_matrix_order(x))

  val_to_expand <- tm[state_pos, state_pos][[1]]
  for (i in seq.int(1L, cycles)){
    m[i + state_pos -1, i+state_pos] <- list(interp(val_to_expand, state_time = i))
  }


  m[i+ state_pos,i+ state_pos] <- list(val_to_expand)


  pre_col <- tm[,  seq_len(state_pos-1), drop = FALSE]
  m[seq.int(1L, cycles + 1L) + state_pos - 1, seq_len(state_pos-1)] <- rep(pre_col[state_pos,], cycles + 1)
  
  for (i in seq.int(N)){
    for (j in seq.int(N)){
      if (i == state_pos & j == state_pos) next
      if (i <= state_pos & j <= state_pos) {
        m[i, j] <- tm[i, j]
        if (i == state_pos){
          m[seq.int(i, i + cycles), j]  <- tm[i,j]
        }
      } else if (i <= state_pos & j > state_pos){
        m[seq.int(i, i + cycles), j + cycles] <-tm[i, j]
      } else if (i > state_pos & j <= state_pos){
        m[seq.int(i + cycles, N + cycles), j] <- tm[i, j]
      } else if (i > state_pos & j > state_pos){
        m[seq.int(i + cycles, N + cycles),
          seq.int(j + cycles, N + cycles)] <- tm[i, j]
      }

  }
  }

  for (i in seq.int(1, cycles)){
    m[i + state_pos -1, ] <- substitute_dots(m[i + state_pos -1, ], list(state_time = i))
  }

  sn <- get_state_names(x)

  sn <- insert(sn, state_pos, sprintf(".%s_%i", state_name, seq.int(1, cycles+1L)))
  sn <- sn[-state_pos]

  x <- define_transition_(as.lazy_dots(t(m)), sn)


  x[get_tm_pos(state_pos+cycles, 1, N+cycles):get_tm_pos(state_pos+cycles, N+cycles, N+cycles)] <-
    substitute_dots(
      x[get_tm_pos(state_pos+cycles, 1, N+cycles):get_tm_pos(state_pos+cycles, N+cycles, N+cycles)],
      list(state_time = unname(cycles) + 1L)
    )

  x
}

#' @export
#' @rdname expand_state
expand_state.uneval_state_list <- function(x, state_name, cycles) {
  
  st <- x[[state_name]]
  x[state_name] <- NULL
  state_values_names <- get_state_value_names(st)
  num_state_values <-length(state_values_names)
  revert_starting <- setNames(as.list(rep(0, num_state_values)), state_values_names) %>%
    as.lazy_dots()
  
  id <- seq_len(cycles + 1)
  res <- lapply(
    id,
    function(i) {
      list(
        .dots = substitute_dots(st$.dots, list(state_time = i)),
        starting_values = if (i == 1) {
          substitute_dots(st$starting_values, list(state_time = i))
        } else {
          revert_starting
        }
      )
    }
  )
  names(res) <- sprintf(".%s_%i", state_name, id)
  
  structure(
    c(x, res),
    class = class(x)
  )
}

#' @export
#' @rdname expand_state
expand_state.uneval_inflow <- function(x, ...) {
  expand_state.uneval_init(x, ...)
}

#' @export
#' @rdname expand_state
expand_state.uneval_init <- function(x, state_name, cycles) {
  res <- insert(
    x,
    which(names(x) == state_name),
    stats::setNames(
      rep(list(lazyeval::lazy(0)), cycles),
      sprintf(".%s_%i", state_name, seq_len(cycles) + 1))
  )
  
  names(res)[which(names(res) == state_name)] <- sprintf(".%s_1", state_name)
  structure(res, class = class(x))
}

#' Convert Lazy Dots to Expression List
#' 
#' This function is used by [interpolate()].
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
#' Sequentially interpolates lazy dots, optionally using 
#' external references.
#' 
#' The interpolation is sequential: the second dot is 
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
interpolate <- function(x, ...) {
  UseMethod("interpolate")
}

#' @export
#' @rdname interpolate
interpolate.default <- function(x, more = NULL, ...) {
  res <- NULL
  
  for (i in seq_along(x)) {
    to_interp <- x[[i]]
    for_interp <- c(more, as_expr_list(res))
    funs <- all.funs(to_interp$expr)
   
    if (any(pb <- funs %in% names(for_interp))) {
      stop(sprintf(
        "Some parameters are named like a function, this is incompatible with the use of 'state_time': %s.",
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
#' @rdname interpolate
interpolate.uneval_matrix <- function(x, ...) {
  res <- interpolate.default(x, ...)
  define_transition_(res, get_state_names(x))
}

#' @export
#' @rdname interpolate
interpolate.state <- function(x, ...) {
  res <- structure(
    list(
    .dots = interpolate.default(x$.dots, ...),
    starting_values = x$starting_values
    )
  )
  define_state_(res)
}

#' @export
#' @rdname interpolate
interpolate.part_surv <- function(x, ...) {
  x
}

#' @export
#' @rdname interpolate
interpolate.uneval_state_list <- function(x, ...) {
  for (i in seq_along(x)) {
    # y <- structure(x[[i]]$.dots,
    #                class = class(x[[i]]))
    #x[[i]] <- interpolate(y, ...)
    x[[i]] <- interpolate(x[[i]], ...)
  }
  x
}

all.funs <- function(expr) {
  an <- all.names(expr)
  uan <- unique(an)
  with_funs <- tabulate(factor(an, levels = uan))
  without_funs <- tabulate(factor(all.names(expr, functions = FALSE), levels = uan))
  res <- with_funs - without_funs
  names(res) <- uan
  names(res)[res > 0]
}

complete_stl <- function(scl, state_names,
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
      stop("'state_time_limit' must be named.")
    }
    if (any(duplicated(names(scl)))) {
      stop("'state_time_limit' names must be unique.")
    }
    if (any(pb <- ! names(scl) %in% state_names)) {
      stop(sprintf(
        "Some 'state_time_limit' names are not state names: %s.",
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
  
  stop("'Incorrect 'state_time_limit' type.")
}
