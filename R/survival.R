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
                                             "gompertz", "gengamma"),
                            ...) {
  
  distribution <- match.arg(distribution)
  
  list_arg <- list(...)
  
  if (distribution %in% c("exp", "weibull",
                          "lnorm", "gamma")) {
    env_f <- asNamespace("stats")
  } else {
    if (! requireNamespace("flexsurv")) {
      stop("'flexsurv' package required.")
    }
    env_f <- asNamespace("flexsurv")
  }
  
  rf <- get(paste0("r", distribution),
            envir = env_f)
  
  names_fun <- setdiff(names(list_arg), "distribution")
  names_par <- setdiff(names(formals(rf)), "n")
  
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
