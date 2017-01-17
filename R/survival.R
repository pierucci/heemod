
#' Extract Transition Probabilities from a Survival Model
#' 
#' Get probabilities from survival models.
#' 
#' If \code{use_km_until = 0}, then only model probabilities
#' will be used.
#' 
#' @param x Either a result from 
#'   \code{\link[flexsurv]{flexsurvreg}} or
#'   \code{\link{define_survival}}.
#' @param cycle The \code{markov_cycle} or 
#'   \code{state_cycle} for which to predict.
#' @param km_limit Up to what time should Kaplan-Meier 
#'   estimates be used? Model predictions will be used 
#'   thereafter. See \code{Details}.
#' @param cycle_length The value of a Markov cycle in 
#'   absolute time units.
#' @param type either \code{prob}, for transition 
#'   probabilities, or \code{surv}, for survival
#' @param ... arguments passed to methods.
#'   
#' @return Returns the Markov transition probability for the
#'   cycles.
#'   
#' @export
get_probs_from_surv <- function(x, ...){
  UseMethod("get_probs_from_surv")
}

#' @rdname get_probs_from_surv
#' @export
get_probs_from_surv.flexsurvreg <- function(x, cycle,
                                            km_limit = 0,
                                            cycle_length = 1,
                                            type = c("prob", "surv"),
                                            ...) {
  
  stopifnot(
    length(x) > 0,
    all(cycle > 0),
    cycle_length > 0,
    km_limit >= 0,
    length(km_limit) == 1
  )
  
  type <- match.arg(type)
  
  times_surv <- cycle_length * c(0, cycle)
  
  res <- rep(NA, length(cycle))
  
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
      res[use_km] <- KM_tr_prob[use_km]
    } else if (type == "surv") {
      res[use_km] <- km$surv[-length(km$surv)][use_km]
    }
  }
  
  if (any(use_pred)) {
    if (inherits(x, "flexsurvreg")) {
      if (! requireNamespace("flexsurv")) {
        stop("'flexsurv' package required.")
      }
      
      #get pred-based probabilities
      if(type == "prob") {
        tmp <- as.data.frame(flexsurv::summary.flexsurvreg(
          x,
          t = times_surv,
          type = "cumhaz")
        )
        pred_tr_prob <- 1 - exp(-diff(tmp$est))
        res[use_pred] <- pred_tr_prob[use_pred]
      } else if (type == "surv") {
        res[use_pred] <- flexsurv::summary.flexsurvreg(
          x, t = times_surv,
          type = "survival")[[1]]$est[-1][use_pred]
      }
    } else {
      stop("Currently only survivals models fitted with 'flexsurv' are supported.")
    }
  }
  
  stopifnot(
    all(! is.na(res))
  )
  
  res
}

#' @rdname get_probs_from_surv
#' @export
get_probs_from_surv.surv_dist <- function(x, cycle,
                                          km_limit = 0,
                                          cycle_length = 1,
                                          type = c("prob", "surv"),
                                          ...) {
  type <- match.arg(type)
  
  stopifnot(
    all(cycle > 0)
  )
  
  times_surv <- cycle_length * c(0, cycle)
  
  if (! requireNamespace("flexsurv")) {
    stop("'flexsurv' package required.")
  }
  
  Hf <- get(paste0("H", x$distribution),
             envir = asNamespace("flexsurv"))
  
  args <- x[- match("distribution", names(x))]
  args[["x"]] <- times_surv
  cumhaz <- do.call(Hf, args)
  
  if(type == "prob") {
    res <- 1 - (1 / exp(diff(cumhaz)))
  } else {
    res <- exp(-cumhaz)[-1]
  }
  
  res
}

#' Define a Survival Distribution
#' 
#' Define a parametric survival distribution.
#' 
#' @param distribution A parametric survival distribution.
#' @param ... Additional distribution parameters (see
#'   respective distribution help pages).
#'   
#' @return A \code{surv_dist} object.
#' @export
#' 
#' @examples
#' 
#' define_survival(distribution = "exp", rate = .5)
#' define_survival(distribution = "gompertz", rate = .5, shape = 1)
#' 
define_survival <- function(distribution = c("exp", "weibull",
                                             "lnorm", "gamma", 
                                             "gompertz", "gengamma",
                                             "beta", "triangle"),
                            ...) {
  
  distribution <- match.arg(distribution)
  
  list_arg <- list(...)
  
  if (distribution %in% c("exp", "weibull",
                          "lnorm", "gamma", "beta")) {
    env_f <- asNamespace("stats")
  } 
  if(distribution == "triangle"){
      if( ! requireNamespace("triangle"))
        stop("'triangle' package required.")
      env_f <- asNamespace("triangle")
    }
  if(distribution %in% c("gompertz", "gengamma")){
      if (! requireNamespace("flexsurv")) {
        stop("'flexsurv' package required.")
      }
    env_f <- asNamespace("flexsurv")
    }
  
  rf <- get(paste0("r", distribution),
            envir = env_f)
  
  names_fun <- setdiff(names(list_arg), "distribution")
  names_par <- setdiff(names(formals(rf)), "n")
  
  if(any(names_fun == ""))
     stop("all arguments to the distribution must be named")
  
  correct_names <- names_fun %in% names_par
  
  if (! all(correct_names)) {
    stop(sprintf(
      "Incorrect argument%s: %s.",
      plur(sum(! correct_names)),
      paste(names_fun[! correct_names], collapse = ", ")))
  }
  
  structure(
    list(
      distribution = distribution,
      ...
    ),
    class = "surv_dist"
  )
}

#' Get transition or survival probabilities from fits
#'
#' @param fits Fits from \code{flexsurvreg}, or a list specifying 
#'   distributions
#' @param treatment The treatment for which probabilities are desired 
#' @param km_until Up to what time should Kaplan-Meier estimates be used?  
#' @param markov_cycle The Markov cycles for which to predict.
#' @param markov_cycle_length The value of a Markov cycle in absolute time units.
#' @param pred_type "prob" or "surv", depending on which type of prediction
#'   is desired.
#'
#' @return a vector of probabilities
#' @export
#'
#' @examples
get_surv_probs <-
  function(fits, treatment, km_until, markov_cycle, 
           markov_cycle_length,
           pred_type = c("prob", "surv")){
    if(missing(fits))
      stop("must specify argument 'fits' in get_surv_probs")
    if(missing(treatment))
      stop("must specify argument 'treatment' in get_surv_probs")
    if(missing(km_until))
      stop("must specify argument 'km_until' in get_surv_probs")
    if(missing(markov_cycle))
      stop("must specify argument 'markov_cycle' in get_surv_probs")
    if(missing(markov_cycle_length))
      stop("must specify argument 'markov_cycle_length' in get_surv_probs")
    pred_type <- match.arg(pred_type)
    get_probs_from_surv(fits[[treatment]], 
                          use_km_until = km_until, 
                          markov_cycle = markov_cycle,
                          markov_cycle_length = markov_cycle_length,
                          pred_type = pred_type)
  }


#' Compute Count of Individual in Each State per Cycle
#'   based on a partitioned survival model
#' 
#' Given a partitioned survival model, returns the number of individuals 
#' per state per cycle.
#' 
#' 
#' @param part_surv_obj A \code{flexsurvreg} object, or a list
#'   specifying a distribution.  
#' @param use_km_until  to what time should Kaplan-Meier estimates be used?
#'   Model predictions will be used thereafter.
#' @param num_patients The number of patients being modeled.  Survival
#'   probabilities are between 0 and 1, so we multiply up by num_patients.
#' @param state_names Name of the states to be assigned to the counts.
#'   
#' @details See \code{get_probs_from_surv} for further information on
#'   \code{part_surv_obj}, \code{use_km_until}, and the \code{markov_cycle}
#'   arguments.
#' @return A \code{cycle_counts} object.
#'   
#' @keywords internal
#' @rdname get_probs_from_surv

compute_counts_part_surv <- function(part_surv_obj, 
                                     use_km_until,
                                     num_patients,
                                     markov_cycle, markov_cycle_length,
                                     state_names){
  stopifnot(length(use_km_until) == 1)
  pfs_surv <- get_probs_from_surv(part_surv_obj$PFS_fit, 
                                    use_km_until = use_km_until,
                                    markov_cycle = markov_cycle,
                                    markov_cycle_length = markov_cycle_length, 
                                    pred_type = "surv") 
  os_surv <- get_probs_from_surv(part_surv_obj$OS_fit, 
                                   use_km_until = use_km_until,
                                   markov_cycle = markov_cycle,
                                   markov_cycle_length = markov_cycle_length, 
                                   pred_type = "surv")
  res <- cbind(pfs_surv, os_surv - pfs_surv , 
               1 - os_surv)
  if("terminal" %in% state_names){
    ## fix the "terminal" state
    terminal <- c(0, diff(res[, 4]))
    res[, 3:4] <- cbind(res[, 1:2], terminal, res[,3])
  }
  res <- res * num_patients
  colnames(res) <- state_names
  res <- data.frame(res)
  structure(res, class = c("cycle_counts", class(res)))
}


#'
#' Calculate additional metrics to evaluate fit of survival model.
#' 
#' @param surv_fits A list object from \code{\link{f_fit_survival_models}} that gives
#' 		a collection (list) of parametric survival fit object. 
#' @param metrics Metrics to calculate.
#' @return
#'   A list object of parametric survival fits, containing additional fields for
#' 		the calculated fit metrics. 
#' @details Currently calculates only:
#' 		\itemize{\item Bayesian information criterion (BIC)
#' 		\item -2*log likelihood (-2LL)}  (Objects come with AIC already calculated.)

f_calc_surv_fit_metrics <-
  function(surv_fits, metrics = c("BIC","m2LL")) {
    
    #argument checks
    stopifnot(length(surv_fits) > 0)
    stopifnot(all(metrics %in% c("BIC","m2LL")))
    
    #get current and previous time step in absolute (not Markov) units
    for(metric in metrics)
    {
      if(metric=="BIC") { surv_fits = get_BIC(surv_fits)}
      if(metric=="m2LL") { surv_fits = get_m2LL(surv_fits)}
    }
    
    #returns updated survival fits object
    surv_fits
  }


get_BIC <- function(surv_fits)
{
  out = surv_fits
  for(i in 1:length(out))
  {
    this_BIC = (-2*getElement(out[[i]], "loglik") + (log(as.numeric(getElement(out[[i]], "N")))*getElement(out[[i]], "npars")))
    out[[i]]$BIC <- this_BIC 
  }
  out
}

get_m2LL <- function(surv_fits)
{
  out = surv_fits
  for(i in 1:length(out))
  {
    this_m2LL = -2*getElement(out[[i]], "loglik")
    out[[i]]$m2LL <- this_m2LL 
  }
  out
}
