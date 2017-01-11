#' Define Partitioned Survival
#' 
#' Define a partitioned survival model with progression-free
#' survival and overall survival.
#' 
#' @param pfs,os Either results from 
#'   \code{\link[flexsurv]{flexsurvreg}} or 
#'   \code{\link{define_survival}}.
#' @param state_names named character vector, length 3 or 4.
#'   State names for progression-free state, progression, 
#'   (optionally terminal) and death respectively. Elements 
#'   should be named \code{"progression_free"}, 
#'   \code{"progression"}, (optionally \code{"terminal"}), 
#'   and \code{"death"}. See examples.
#' @param terminal_state Should a terminal state be 
#'   included? Only used when state names are not provided.
#' @param km_limit Up to what time should Kaplan-Meier
#'   estimates be used? Model predictions will be used
#'   thereafter. Either a length 2 vector (for \code{pfs}
#'   and \code{os} respectively) or a single values (used
#'   for both distributions).
#' @param cycle_length The value of a Markov cycle in
#'   absolute time units.
#'   
#' @return A \code{part_surv} object.
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
                             km_limit = 0,
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
  } else if (terminal_state) {
    warning("Argument 'terminal_state' ignored when state names are given.")
  }
  
  define_part_surv_(
    pfs = lazyeval::lazy_(substitute(pfs), env = parent.frame()),
    os = lazyeval::lazy_(substitute(os), env = parent.frame()),
    state_names = state_names,
    km_limit = km_limit,
    cycle_length = cycle_length)
}

#' @export
#' @rdname define_part_surv
define_part_surv_ <- function(pfs, os, state_names,
                              km_limit = 0,
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
    
    length(km_limit) %in% 1:2,
    all(km_limit >= 0),
    
    length(cycle_length) %in% 1:2,
    all(cycle_length > 0)
  )
  
  if (length(km_limit) == 1) {
    km_limit <- rep(km_limit, 2)
  }
  
  if (length(cycle_length) == 1) {
    cycle_length <- rep(cycle_length, 2)
  }
  
  res <- list(
    pfs = pfs,
    os = os,
    state_names = state_names,
    km_limit = km_limit,
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
  
  pfs_dist <- lazyeval::lazy_eval(
    x$pfs, 
    data = dplyr::slice(parameters, 1)
  )
  
  pfs_surv <- get_probs_from_surv(
    pfs_dist,
    cycle = parameters$markov_cycle,
    km_limit = x$km_limit[1],
    cycle_length = x$cycle_length[1],
    type = "surv"
  )
  
  os_dist <- lazyeval::lazy_eval(
    x$os, 
    data = dplyr::slice(parameters, 1)
  )
  
  os_surv <- get_probs_from_surv(
    os_dist,
    cycle = parameters$markov_cycle,
    km_limit = x$km_limit[2],
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
                                          method, inflow) {
  
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
  
  if (method != "end") {
    warning("Currently only counting method 'end' is supported with partitioned survival models. ",
            "Option 'method' is ignored.")
  }
  
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
