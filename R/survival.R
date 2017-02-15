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
get_probs_from_surv_ <- function(x, ...){
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
get_probs_from_surv_.surv_dist <- function(x, cycle,
                                          cycle_length = 1,
                                          type = c("prob", "surv"),
                                          ...) {

  ## ... here absorbs km_limit, which is used for flexsurvreg
  ## objects but not for surv_dist objects
  type <- match.arg(type)
  
  stopifnot(
    all(cycle > 0)
  )
  
  times_surv <- cycle_length * c(min(cycle) - 1, cycle)
  
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

#' @rdname get_probs_from_surv
#' @export
get_probs_from_surv <- memoise::memoise(
  get_probs_from_surv_,
  ~ memoise::timeout(options()$heemod.memotime)
)

#' Define a Survival Distribution
#' 
#' Define a parametric survival distribution.
#' 
#' @param distribution A parametric survival distribution.
#' @param ... Additional distribution parameters (see
#'   respective distribution help pages).
#'   
#' @return A `surv_dist` object.
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
#' 
#' simple_exp <- list(A = define_survival("exp", rate = .05),
#'                    B = define_survival("gamma", rate = .5, shape = 2))
#' get_surv_probs(simple_exp, treatment = "A", 
#'                km_until = 0, markov_cycle = 1:5,
#'                markov_cycle_length = 1, pred_type = "prob")
#' get_surv_probs(simple_exp, treatment = "B", 
#'                km_until = 0, markov_cycle = 1:5,
#'                markov_cycle_length = 1, pred_type = "prob")

get_surv_probs <-
  function(fits, treatment, km_until, markov_cycle, 
           markov_cycle_length,
           pred_type = c("prob", "surv")){
    if(missing(fits))
      stop("must specify argument 'fits' in get_surv_probs")
    if(missing(treatment))
      stop("must specify argument 'treatment' in get_surv_probs")
    ## TODO:   km_until only required if fits[[treatment]] is flexsurvreg,
    ##         not part_surv
    if(missing(km_until))
      stop("must specify argument 'km_until' in get_surv_probs")
    if(missing(markov_cycle))
      stop("must specify argument 'markov_cycle' in get_surv_probs")
    if(missing(markov_cycle_length))
      stop("must specify argument 'markov_cycle_length' in get_surv_probs")
    pred_type <- match.arg(pred_type)
    get_probs_from_surv(fits[[treatment]], 
                        cycle = markov_cycle,
                        km_limit = km_until, 
                        cycle_length = markov_cycle_length,
                        type = pred_type)
  }



