#' Compute Counts for Partitioned Survival Models
#' 
#' Compute count of individual in each state per cycle based
#' on a partitioned survival model.
#' 
#' The elements of \code{x} should be called \code{pfs} and 
#' \code{os} for progression-free survival and overall
#' survival respectively.
#' 
#' See \code{\link{get_probabilities}} for further 
#' information on \code{km_limit}, and the
#' \code{markov_cycle} arguments.
#' 
#' @param x A list of \code{flexsurvreg} objects, or a list
#'   of lists specifying distributions.
#' @param km_limit To what time should Kaplan-Meier 
#'   estimates be used? Model predictions will be used 
#'   thereafter.
#' @param num_patients The number of patients being modeled.
#' @param state_names Name of the states to be assigned to 
#'   the counts.
#'   
#' @return A \code{cycle_counts} object.
#'   
#' @keywords internal
compute_counts_part_surv <- function(x, km_limit,
                                     num_patients,
                                     markov_cycle,
                                     markov_cycle_length = 1) {
  
  stopifnot(
    length(use_km_until) == 1,
    length(x$state_names) %in% 3:4,
    all(names(x$state_names) %in% c(
      "progression_free",
      "progression",
      "terminal",
      "death"
    )),
    ! any(duplicated(names(x$state_names)))
  )
  
  pfs_surv <- get_probs_from_surv(
    x$pfs, 
    km_limit = km_limit,
    cycle = markov_cycle,
    cycle_length = markov_cycle_length, 
    type = "surv"
  )
  
  os_surv <- get_probs_from_surv(
    x$os, 
    km_limit = km_limit,
    cycle = markov_cycle,
    cycle_length = markov_cycle_length, 
    type = "surv"
  )
  
  res <- data.frame(
    progression_free = pfs_surv,
    progression = os_surv - pfs_surv, 
    death = 1 - os_surv
  )
  
  if (length(x$state_names) == 4) {
    res$terminal <- diff(c(0, res$x3))
    res$death <- c(0, res$x3[-nrow(res)])
  }
  
  res <- res * num_patients
  
  names(res) <- x$state_names[names(res)]
  
  structure(res, class = c("cycle_counts", class(res)))
}

#' Define Partitioned Survival
#' 
#' Define a partitioned survival model with progression-free
#' survival and overall survival.
#' 
#' @param pfs Result of \code{\link{get_prob_from_surv}} for
#'   progression-free survival.
#' @param os Idem for overall survival.
#' @param state_names named character vector, length 3 or 4.
#'   State names for progression-free state, progression, 
#'   (optionally terminal) and death respectively. Elements 
#'   should be named \code{"progression_free"}, 
#'   \code{"progression"}, (optionally \code{"terminal"}), 
#'   and \code{"death"}. See examples.
#' @param terminal_state Should a terminal state be 
#'   included? Only used when state names are not provided.
#' @param .dots Used for NSE, lazy dots with names
#'   \code{pfs} and \code{os}.
#'   
#' @return A \code{part_surv} object.
#' @export
#' 
#' @examples
#' probs_pfs <- define_survival("exp", rate = 1)
#' probs_os <- define_survival("exp", rate = .5)
#' 
#' define_part_surv(
#'   pfs = get_probs_from_surv(probs_pfs, cycle = markov_cycle),
#'   os = get_probs_from_surv(probs_os, cycle = markov_cycle),
#'   state_names = c(
#'     progression_free = "A",
#'     progression = "B",
#'     terminal = "C",
#'     death = "D"
#'   )
#' )
#' #' identical to:
#' define_part_surv(
#'   pfs = get_probs_from_surv(probs_pfs, cycle = markov_cycle),
#'   os = get_probs_from_surv(probs_os, cycle = markov_cycle),
#'   terminal_state = TRUE
#' )
#' 
define_part_surv <- function(..., state_names,
                             terminal_state = FALSE) {
  
  if (missing(state_names)) {
    message("No named state -> generating names.")
    state_names <- LETTERS[seq_len(if (terminal_state) 4 else 3)]
    names(state_names) <- c(
      "progression_free",
      "progression",
      "terminal",
      "death"
    )
  }
  
  .dots <- lazyeval::lazy_dots(...)
  
  define_part_surv_(.dots = .dots, state_names = state_names)
}

#' @export
#' @rdname define_part_surv
define_part_surv_ <- function(.dots, state_names) {
  
  stopifnot(
    length(state_names) %in% 3:4,
    ! is.null(names(state_names)),
    all(names(state_names) %in% c(
      "progression_free",
      "progression",
      "terminal",
      "death"
    )),
    ! any(duplicated(names(state_names))),
    length(.dots) == 2,
    all(sort(names(.dots)) == c("os", "pfs"))
  )
  
  res <- list(
    pfs = .dots$pfs,
    os = .dots$os,
    state_names = state_names
  )
  
  structure(
    res,
    class = "part_surv"
  )
}
