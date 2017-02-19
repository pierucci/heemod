#' Extract Transition Probabilities from a Survival Model
#' 
#' Get probabilities from survival models.
#' 
#' If `use_km_until = 0`, then only model probabilities
#' will be used.
#' 
#' The results of `get_probs_from_surv` are memoised for
#' `options("heemod.memotime")` (default: 1 hour) to
#' increase resampling performance.
#' 
#' @name get_probs_from_surv
#' @param x Either a result from 
#'   [flexsurv::flexsurvreg()] or
#'   [define_survival()].
#' @param cycle The `markov_cycle` or 
#'   `state_time` for which to predict.
#' @param km_limit Up to what time should Kaplan-Meier 
#'   estimates be used? Model predictions will be used 
#'   thereafter. See `Details`.
#' @param cycle_length The value of a Markov cycle in 
#'   absolute time units.
#' @param type either `prob`, for transition 
#'   probabilities, or `surv`, for survival
#' @param ... arguments passed to methods.
#'   
#' @return Returns the Markov transition probability for the
#'   cycles.
#'   
#' @export
get_probs_from_surv_ <- function(x, ...) {
  UseMethod("get_probs_from_surv_")
}

#' @rdname get_probs_from_surv
#' @export
get_probs_from_surv_.flexsurvreg <- function(x, cycle,
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
  
  times_surv <- cycle_length * c(min(cycle) - 1, cycle)
  
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
      if (type == "prob") {
        tmp <- as.data.frame(summary(
          x,
          t = times_surv,
          type = "cumhaz")
        )
        pred_tr_prob <- 1 - exp(-diff(tmp$est))
        res[use_pred] <- pred_tr_prob[use_pred]
      } else if (type == "surv") {
        res[use_pred] <- summary(
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
get_probs_from_surv_.surv_dist <- function(x, cycle,
                                          km_limit = 0,
                                          cycle_length = 1,
                                          type = c("prob", "surv"),
                                          ...) {
  type <- match.arg(type)
  
  stopifnot(
    all(cycle > 0)
  )
  
  times_surv <- cycle_length * c(min(cycle) - 1, cycle)
  
  if (! requireNamespace("flexsurv")) {
    stop("'flexsurv' package required.")
  }
  
  pf <- get(paste0("p", x$distribution),
             envir = asNamespace("flexsurv"))
  
  args <- x[- match("distribution", names(x))]
  args[["x"]] <- times_surv
  args[["lower.tail"]] <- F
  res <- do.call(pf, args)
  
  if (type == "prob") {
    res <- calc_probs(ret)
  } else {
    res <- res[-1]
  }
  
  res
}

#' @rdname get_probs_from_surv
#' @export
get_probs_from_surv <- memoise::memoise(
  get_probs_from_surv_,
  ~ memoise::timeout(options()$heemod.memotime)
)


