#' Extract Transition Probabilities from a Survival Model
#' 
#' Get probabilities from fitted survival models, to be used
#' by a transition matrix or a partitioned survival model.
#' 
#' If \code{use_km_until = 0}, then only model probabilities
#' will be used.
#' 
#' @param x Either a result from 
#'   \code{\link[flexsurv]{flexsurvreg}} or a list 
#'   specifying a distribution and its arguments.
#' @param cycle The \code{markov_cycle} or 
#'   \code{state_cycle} for which to predict.
#' @param km_limit Up to what time should Kaplan-Meier 
#'   estimates be used? Model predictions will be used 
#'   thereafter. See \code{Details}.
#' @param cycle_length The value of a Markov cycle in 
#'   absolute time units.
#' @param type either \code{prob}, for transition 
#'   probabilities, or \code{surv}, for survival
#' @param ... arguments to be passed to methods.
#'   
#' @return Returns the Markov transition probability for the
#'   cycles.
#'   
#' @export
#' 
#' @examples 
#' 		
#' trans_probs_from_surv(list(dist_name = "exp", rate = 0.01))
#' 
get_probs_from_surv <- function(x, ...){
  UseMethod("get_probs_from_surv")
}

#' @rdname get_probs_from_surv
#' @export
get_probs_from_surv.flexsurvreg <- function(x, cycle,
                                            km_limit = 0,
                                            cycle_length = 1,
                                            type = c("prob", "surv")) {
  
  stopifnot(
    length(x) > 0,
    all(cycle > 0),
    cycle_length > 0,
    km_limit >= 0,
    length(km_limit) == 1
  )
  
  type <- match.arg(type)
  ##    if(pred_type == "surv" & use_km_until == 0)
  ##      stop('prediction of "surv" only with Kaplan-Meier
  ##           probabilities (use_km_until > 0)')
  
  #get times in absolute (not Markov) units
  times_surv <- cycle_length * c(0, cycle)
  
  preds <- rep(NA, length(cycle))
  
  prob_type <- ifelse(
    cycle <= km_limit,
    "km", "pred"
  )
  
  use_km <- prob_type == "km"
  use_pred <- ! use_km
  
  if (any(prob_type %in% "km")) {
    ## get KM-based probabilities - code taken from
    ## plot.flexsurvreg
    mf <- stats::model.frame(x)
    Xraw <- mf[, attr(mf, "covnames.orig"), drop = FALSE]
    dat <- x$data
    isfac <- sapply(Xraw, is.factor)
    mm <-  as.data.frame(stats::model.matrix(x))
    form <-
      "survival::Surv(dat$Y[,\"start\"],dat$Y[,\"stop\"],dat$Y[,\"status\"]) ~ "
    
    form <- paste(form,
                  if (x$ncovs > 0 && all(isfac)) {
                    paste("mm[,", 1:x$ncoveffs, "]", collapse = " + ")
                  } else {
                    1})
    form <- stats::as.formula(form)
    
    #only solve KM until last TTE, and then 
    #  assign the rest NA, which will cause an error
    max.time = floor(max(dat$Y[, "stop"]))
    times_KM = intersect(times_surv, c(0:max.time))
    ## times_KM <- times_surv
    km <- summary(
      survival::survfit(form, data = mm),
      times = times_KM
    )
    if (type == "prob") {
      km_cumhaz <- -log(km$surv)
      km_haz <- diff(km_cumhaz)
      KM_tr_prob <- 1 - exp(-km_haz)
      delta = length(times_surv) - length(times_KM)
      ## setting 0 beyond what we have because NA throws an error
      ## need to check through this more thoroughly
      KM_tr_prob = c(KM_tr_prob, rep(0, delta))
      preds[use_km] <- KM_tr_prob[use_km]
    } else if (type == "surv") {
      preds[use_km] <- km$surv[-length(km$surv)][use_km]
    }
  }
  
  if (any(use_pred)) {
    if (inherits(x, "flexsurvreg")) {
      #get pred-based probabilities
      if(type == "prob") {
        tmp <- as.data.frame(flexsurv::summary.flexsurvreg(
          x,
          t = times_surv,
          type = "cumhaz")
        )
        pred_tr_prob <- 1 - exp(-diff(tmp$est))
        preds[use_pred] <- pred_tr_prob[use_pred]
      } else if (type == "surv") {
        preds[use_pred] <- flexsurv::summary.flexsurvreg(
          x,
          t = times_surv,
          type = "survival")[[1]]$est[-1][use_pred]
      }
    }
  }
  
  stopifnot(
    all(! is.na(preds))
  )
  
  preds
}

#' @rdname get_probs_from_surv
#' @export
get_probs_from_surv.list <- function(x, cycle,
                                     km_limit = 0,
                                     cycle_length = 1,
                                     type = c("prob", "surv")) {
  type <- match.arg(type)
  
  stopifnot(
    "dist_name" %in% names(x),
    all(cycle > 0)
  )
  
  this_fn <- paste0("r", x$dist_name)
  
  correct_names <- setdiff(
    names(x), "dist_name"
  ) %in% names(formals(this_fn))
  
  if (! all(correct_names)) {
    stop(sprintf(
      "Incorrect arguments: %s.",
      paste(setdiff(
        names(x), "dist_name"
      )[! correct_names])))
  }
  
  times_surv <- cycle_length * c(0, cycle)
  Hfn <- get(paste0("H", x$dist_name),
             envir = asNamespace("flexsurv"))
  args <- x[- match("dist_name", names(x))]
  args <- as.list(args)
  args[["x"]] <- times_surv
  cumhaz <- do.call(Hfn, args)
  
  if(type == "prob") {
    pred_tr_prob <- 1 - (1 / exp(diff(cumhaz)))
  } else if (type == "surv") {
    pred_tr_prob <- exp(-cumhaz)[-1]
  }
  
  pred_tr_prob
}

#' Define a Survival Distribution
#' 
#' Define a parametric survival distribution, to be used
#' by \code{\link{get_probs_from_surv}}.
#'
#' @param distribution A parametric survival distribution.
#' @param ... Additional distribution parameters (see xxx).
#'
#' @return A \code{surv_dist} object.
#' @export
#'
#' @examples
#' 
#' define_surv_dist(distribution = "exp", lambda = .5)
#' 
define_surv_dist <- function(distribution = c("exp", "weibull"),
                             ...) {
  distribution <- match.arg(distribution)
  
  structure(
    list(
      distribution = distribution,
      ...
    ),
    class = "surv_dist"
  )
}

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
                                     markov_cycle, markov_cycle_length,
                                     state_names) {
  
  stopifnot(length(use_km_until) == 1)
  
  pfs_surv <- get_probabilities(
    x$pfs, 
    km_limit = km_limit,
    cycle = markov_cycle,
    cycle_length = markov_cycle_length, 
    type = "surv"
  )
  
  os_surv <- get_probabilities(
    x$os, 
    km_limit = km_limit,
    cycle = markov_cycle,
    cycle_length = markov_cycle_length, 
    type = "surv"
  )
  
  res <- cbind(
    pfs_surv,
    os_surv - pfs_surv, 
    1 - os_surv
  )
  
  if ("terminal" %in% state_names) {
    ## fix the "terminal" state
    terminal <- c(0, diff(res[, 4]))
    res[, 3:4] <- cbind(res[, 1:2], terminal, res[,3])
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
