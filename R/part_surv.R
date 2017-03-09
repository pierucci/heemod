#' Define Partitioned Survival
#' 
#' Define a partitioned survival model with progression-free
#' survival and overall survival.
#' 
#' @param pfs,os Either results from 
#'   [flexsurv::flexsurvreg()] or 
#'   [define_survival()].
#' @param state_names named character vector, length 3 or 4.
#'   State names for progression-free state, progression, 
#'   (optionally terminal) and death respectively. Elements 
#'   should be named `"progression_free"`, 
#'   `"progression"`, (optionally `"terminal"`), 
#'   and `"death"`. See examples.
#' @param terminal_state Should a terminal state be 
#'   included? Only used when state names are not provided.
#' @param cycle_length The value of a Markov cycle in
#'   absolute time units.
#'   
#' @return A `part_surv` object.
#' @export
#' 
#' @examples
#' dist_pfs <- define_survival("exp", rate = 1)
#' dist_os <- define_survival("exp", rate = .5)
#' 
#' define_part_surv(
#'   pfs = dist_pfs,
#'   os = dist_os,
#'   state_names = c(
#'     progression_free = "A",
#'     progression = "B",
#'     terminal = "C",
#'     death = "D"
#'   )
#' )
#' # identical to:
#' define_part_surv(
#'   pfs = dist_pfs,
#'   os = dist_os,
#'   terminal_state = TRUE
#' )
#' 
define_part_surv <- function(pfs, os, state_names,
                             terminal_state = FALSE,
                             cycle_length = 1) {
  
  if (missing(state_names)) {
    message("No named state -> generating names.")
    
    if (terminal_state) {
      state_names <- LETTERS[seq_len(4)]
      names(state_names) <- c(
        "progression_free",
        "progression",
        "terminal",
        "death"
      )
    } else {
      state_names <- LETTERS[seq_len(3)]
      names(state_names) <- c(
        "progression_free",
        "progression",
        "death"
      )
    }
  } 
  if(is.null(names(state_names))) {
    if (terminal_state) {
      warning("Argument 'terminal_state' ignored when state names are given.")
    }
    state_names <- fix_part_surv_state_names(state_names)
  }
  
  define_part_surv_(
    pfs = lazyeval::lazy_(substitute(pfs), env = parent.frame()),
    os = lazyeval::lazy_(substitute(os), env = parent.frame()),
    state_names = state_names,
    cycle_length = cycle_length)
  }


#' @export
#' @rdname define_part_surv
define_part_surv_ <- function(pfs, os, state_names,
                              cycle_length = 1) {
  
  stopifnot(
    inherits(pfs, "lazy"),
    inherits(os, "lazy"),
    
    length(state_names) %in% 3:4,
    ! is.null(names(state_names)),
    all(names(state_names) %in% c(
      "progression_free",
      "progression",
      "terminal",
      "death"
    )),
    ! any(duplicated(names(state_names))),
    length(cycle_length) %in% 1:2,
    all(cycle_length > 0)
  )
  
  if (length(cycle_length) == 1) {
    cycle_length <- rep(cycle_length, 2)
  }
  
  res <- list(
    pfs = pfs,
    os = os,
    state_names = state_names,
    cycle_length = cycle_length
  )
  
  structure(
    res,
    class = "part_surv"
  )
}

get_state_names.part_surv <- function(x) {
  x$state_names
}

eval_transition.part_surv <- function(x, parameters) {
  
  time_ <- c(0, parameters$markov_cycle)
  
  pfs_dist <- lazyeval::lazy_eval(
    x$pfs, 
    data = dplyr::slice(parameters, 1)
  )
  
  pfs_surv <- compute_surv(
    pfs_dist,
    time = time_,
    cycle_length = x$cycle_length[1],
    type = "surv"
  )
  
  os_dist <- lazyeval::lazy_eval(
    x$os, 
    data = dplyr::slice(parameters, 1)
  )
  
  os_surv <- compute_surv(
    os_dist,
    time = time_,
    cycle_length = x$cycle_length[2],
    type = "surv"
  )
  
  structure(
    list(
      pfs_surv = pfs_surv,
      os_surv = os_surv,
      state_names = x$state_names
    ),
    class = "eval_part_surv")
}

compute_counts.eval_part_surv <- function(x, init,
                                          inflow) {
  
  stopifnot(
    length(x$state_names) %in% 3:4,
    all(names(x$state_names) %in% c(
      "progression_free",
      "progression",
      "terminal",
      "death"
    )),
    ! any(duplicated(names(x$state_names))),
    length(init) == length(x$state_names),
    all(init[-1] == 0)
  )
  
  res <- data.frame(
    progression_free = x$pfs_surv,
    progression      = x$os_surv - x$pfs_surv, 
    death            = 1 - x$os_surv
  )
  
  if (length(x$state_names) == 4) {
    res$terminal <- diff(c(0, res$death))
    res$death <- c(0, res$death[-nrow(res)])
  }
  
  if (any(res < 0)) {
    stop("Negative counts in partitioned model.")
  }
  
  res <- res * sum(init)
  
  names(res) <- x$state_names[names(res)]
  res <- res[x$state_names]
  
  structure(res, class = c("cycle_counts", class(res)))
}

fix_part_surv_state_names <-
  function(state_names) {
    death_state <- c(
      grep("death", state_names, ignore.case = TRUE),
      grep("dead", state_names, ignore.case = TRUE)
    )
    progfree_state <- grep("free", state_names, ignore.case = TRUE)
    progressive_state <-
      setdiff(grep("progress", state_names, ignore.case = TRUE),
              progfree_state)
    terminal_state <- grep("terminal", state_names, ignore.case = TRUE)
    
    if (length(death_state) != 1)
      stop("state representing death must be named ",
           "'death' or 'dead' (case insensitive)")
    if (length(progfree_state) != 1)
      stop("progression free state (only) must have 'free' in its name")
    if (length(progressive_state) != 1)
      stop("progression free state must have 'progress' ",
           "but not 'free', in its name")
    
    stopifnot(length(state_names) %in% c(3, 4))
    if (length(state_names) == 3)
      names(state_names) <-
      c("progression_free",
        "progression",
        "death")[c(progfree_state, progressive_state,
                   death_state)]
    if (length(state_names) == 4) {
      if (length(terminal_state) == 0)
        stop(
          "if there are four states, you must have a state called 'terminal'",
          "(not case sensitive)"
        )
      names(state_names) <-
        c("progression_free",
          "progression",
          "terminal",
          "death")[c(progfree_state,
                     progressive_state,
                     terminal_state,
                     death_state)]
    }
    state_names
  }
