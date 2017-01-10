
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
  
  stopifnot(length(use_km_until) == 1)
  
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
    x1 = pfs_surv,
    x2 = os_surv - pfs_surv, 
    x3 = 1 - os_surv
  )
  
  if (terminal_state) {
    res$x4 <- diff(c(0, res$x3))
    res$x3 <- c(0, res$x3[-nrow(res)])
  }
  
  res <- res * num_patients
  colnames(res) <- state_names
  res <- data.frame(res)
  
  structure(res, class = c("cycle_counts", class(res)))
}

#' Define Partitioned Survival
#' 
#' Define a partitioned survival model with progression-free
#' survival and overall survival.
#' 
#' @param pfs Distribution of progression-free survival.
#' @param os Distribution of overall survival.
#' @param state_names character vector, optional, length 3. State
#'   names for original state, progression and death
#'   respectively.
#' @param km_limit
#'   
#' @return
#' @export
#' 
#' @examples
define_part_surv <- function(pfs, os, state_names, km_limit) {
  if (missing(state_names)) {
    message("No named state -> generating names.")
    state_names <- LETTERS[seq_len(3)]
  } else {
    if (length(state_names) != 3) {
      stop("'state_names' should be of length 3.")
    }
  }
}
