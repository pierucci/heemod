#' Compute Survival Analysis Curves from Data
#' 
#' @param base_dir base directory for the analysis.
#' @param surv_dir subdirectory in which survival data and
#'   fits are stored.
#' @param data_files files containing survival data.
#' @param fit_files Files containing fits.
#' @param fit_names name(s) to save the fits to, if they are
#'   being saved in a file
#' @param time_col_name,censor_col_name,treatment_col_name 
#'   columns to be used in the analysis
#' @param fit_metric  Once of AIC, BIC, or m2LL
#' @param best_only should only the best fit for each
#'   condition be returned?
#' @param dists the distributions to use to fit the survival
#'   function
#' @param use_envir An environment for the results to be
#'   saved in.
#'   
#' @return A list with two elements:  \itemize{ 
#'   \item{\code{best_models}, a list with the fits for each
#'   data file passed in; and} \item{\code{envir}, an
#'   environment containing the models so they can be
#'   referenced to get probabilities.} }
#' @details By default, the function fits with six different
#'   distribution fucntions: exponential,  Weibull, 
#'   lognormal, Gompertz, gamma, and generalized gamma.
#'   
#'   If data_files is NULL and fit files exists, then
#'   fit_files should have the names of files in which
#'   survival models are kept. If data_files is not NULL,
#'   then survival models will be fit from the data in
#'   data_files (using the \code{flexsurvreg} package), and
#'   if fit_files is also not NULL, the survival models will
#'   be saved in fit_files.
#'   
#'   If \code{best_only = TRUE}, then \code{best_models}
#'   will be a vector of models, one for each group. If
#'   \code{best_only = FALSE}, \code{best_models} will have
#'   a matrix of models, with each row corresponding to a 
#'   distribution and each column to a group.
survival_from_data <- function(base_dir, surv_dir, data_files = NULL, 
                               fit_files = NULL, fit_names = NULL, 
                               time_col_name = "time",
                               censor_col_name = "status",
                               treatment_col_name = "treatment",
                               fit_metric = "AIC",
                               best_only = TRUE,
                               dists = c("exp", "weibull", "lnorm", "gamma", 
                                         "gompertz", "gengamma"),
                               use_envir = NULL) {
  
  dists <- match.args(dists, several.ok = TRUE)
  
  if (length(data_files) == 0 & length(fit_files) == 0) {
    stop("must specify at least one of data_files or fit_files")
  }
  
  if (length(fit_files) > 0 & 
      length(fit_files) != length(fit_names)) {
    stop(paste("must specify the same number of fit_files and",
               "fit_names if any fit_names are specified."))
  }
  
  if (!is.null(base_dir)) {
    full_path <- file.path(base_dir, surv_dir)
  } else {
    full_path <- surv_dir
  }
  
  if (length(data_files) > 0) {
    ## read data from files and fit models 
    survdata <- lapply(data_files, function(this_file) {
      ## if the "file name" begins with "list(", then
      ##   it just needs to be evaluated into a list.
      really_a_list_expr <- substring(this_file, 1, 5) == "list("
      if(really_a_list_expr){
        res <- eval(parse(text = this_file))
      } else {
        res <- read_file(file.path(full_path, this_file))
        class(res) <- c("survdata", class(res))
      }
      res
    })
    ## now if we have a list from above, leave it alone; 
    ##  otherwise fit the data
    best_models <- lapply(survdata, function(this_data){
      if(class(this_data)[1] == "list") {
        this_data
      } else {
        surv_models <- fit_survival_models(
          this_data,
          dists = dists,
          time_col_name = time_col_name, 
          censor_col_name = censor_col_name, 
          treatment_col_name = treatment_col_name,
          covariate_col_names = NULL, 
          fit_indiv_groups = TRUE
        )
        if (best_only) {
          apply(surv_models, 2, f_get_best_surv_model, 
                metric = fit_metric)
        } else {
          surv_models
        }
      }
    })
    ## best_dists <- lapply(best_models, dist_from_fits)
    if(length(fit_files) > 0){
      ## eventually may want a different format for saving these
      ## currently dump for a readable version, save for a complete version
      for(i in seq(along = fit_names)){
        assign(fit_names[i], best_models[[i]])
        write_to_me <- paste(file.path(full_path, fit_files[i]), ".R", sep = "")
        suppressMessages(
          ## suppressMessages because we know we're going to get a warning
          ##   that it may not be possible to completely deparse an object
          ##   from flexsurv
          ## dump(c(fit_names[i], "best_dists"), file = write_to_me)
          dump(c(fit_names[i]), file = write_to_me)
        )
        save_to_me <- gsub(".R$", ".Rdata", write_to_me)
        ## save(list = c(fit_names[i], "best_dists"), file = save_to_me)
        save(list = c(fit_names[i]), file = save_to_me)
      }
    }
    if(!is.null(use_envir))
      for(i in seq(along = fit_names))
        assign(fit_names[i], best_models[[i]], envir = use_envir)
    
  } #close if(length(data_files) > 0)
  
  else{   # that is, if we did not get data files - we'll load previous fits
    for(this_fit_file in fit_files){
      load(paste(file.path(full_path, this_fit_file), ".RData", sep = ""))
      if(!is.null(use_envir))
        load(paste(file.path(full_path, this_fit_file), ".RData", sep = ""), 
             envir = use_envir)
    }
    best_models <- mget(fit_names)
  }
  
  names(best_models) <- fit_names
  res <- list(best_models, envir = use_envir)
  res
}

dist_from_fits <- function(this_fit){
  attr(this_fit, "dist")
}

#' Fit Survival Models
#' 
#' Fit multiple survival models, with various distributions.
#' 
#' No objects are returned for the Kaplan-Meier fit; that 
#' can be obtained from any of the fitted objects by 
#' using the \code{summary.flexsurvreg} function with
#' \code{type = "survival"}.
#' 
#' @param x Survival data to be used.
#' @param dists Distributional forms to be considered in 
#'   fitting using \code{flexsurvreg}.
#' @param group Optional treatment group names.
#' @param time Event times.
#' @param censor Censorship indicators. 0: event 
#'   observed; 1: censored.
#' @param covariates  Not yet implemented.
#' @param fit_by_group Should groups be also fit 
#'   individually?
#'   
#' @return a matrix of flexsurvreg objects, 
#'   with rows corresponding to distributions, and 
#'   columns corresponding to groups.
#'   
#' @export
fit_survival <- function(x,
                         dists = c("exp", "weibull",
                                   "lnorm", "gamma", 
                                   "gompertz", "gengamma"),
                         time, censor, group = NULL,
                         covariates = NULL, 
                         fit_by_group = TRUE) {
  ##
  ## TODO:  so far, covariate_col_names is only a placeholder
  ##  what needs to be decided is whether to allow all combinations
  ##  of covariates, or have people enter the combinations they want,
  ##  or somehow allow both ways (which might be better handled by
  ##  different functions entering in)
  if(!requireNamespace("flexsurv", quietly = TRUE)) {
    stop("'flexsurv' packaged needed to fit survival models.")
  }
  
  stopifnot(
    inherits(x, "data.frame"),
    nrow(x) > 0,
    length(time) == 1,
    length(censor) == 1,
    time %in% names(x),
    censor %in% names(x),
    is.null(group) || length(group) == 1,
    is.null(group) || group %in% names(x),
    is.null(covariates) ||
      all(covariates %in% names(x))
  )
  
  if (! is.null(group)) {
    ## make a list of subgroups, and add a group of all together
    
    if (fit_by_group) {
      groups_list <- stats::setNames(
        as.list(as.character(unique(x[[group]]))),
        as.character(unique(x[[group]]))
      )
    } else {
      groups_list <- list(all = as.character(unique(x[[group]])))
    }
  } else {
    groups_list <- list(no_group = NULL)
  }
  
  formula_base_string <- paste0(
    "survival::Surv(", time, ", ", censor, ")", " ~"
  )
  
  ## cycle through combinations of distributions and subsets,
  ##   getting survival analysis results at each step
  conditions <- do.call(
    expand.grid,
    Filter(
      Negate(is.null),
      list(
        dist = dists,
        group = names(groups_list),
        stringsAsFactors = FALSE
      )
    )
  )
  
  res <- lapply(
    seq_len(nrow(conditions)),
    function(i) {
      if (! is.null(group)) {
        this_group_set <- groups_list[[conditions[i, "group"]]]
        this_data <- x[x[[group]] %in% this_group_set, ]
        num_treatments <- length(unique(this_data[, group]))
        if (num_treatments > 1) {
          ## only include the treatment group in the formula
          ##   if there is more than one treatment in the data set
          
          this_formula <- paste(formula_base_string, group)
        } else {
          this_formula <- paste(formula_base_string, "1")
        }
      } else {
        this_data <- x
        this_formula <- paste(formula_base_string, "1")
      }
      
      flexsurv::flexsurvreg(
        stats::as.formula(this_formula),
        data = this_data,
        dist = conditions[i, "dist"]
      )
    })
  
  dim(res) <- c(length(dists), length(names(groups_list)))
  dimnames(res) <- list(dists, names(groups_list))
  
  structure(res, class = c("fit_survival", class(res)))
}

#' Return Best Fitting Distribution
#' 
#' Given an object from \code{\link{fit_survival}}, returns 
#' the best fitting distribution with regard to the given
#' metric.
#' 
#' Gets the best survival fit, according to a particular 
#' metric. Current metrics are: Akaike information criterion
#' (\code{AIC}), Bayesian information criterion 
#' (\code{BIC}), -2*log(likelihood) (\code{m2LL}).
#' 
#' @param x Object returned by \code{\link{fit_survival}}.
#' @param metric Metric to assess model fit.
#'   
#' @return The best fitting distribution according to the
#'   chosen metric.
get_best_fit <-function(x, metric = c("AIC","BIC","m2LL")) {
  
  metric <- match.arg(metric)
  
  stopifnot(length(x) > 0)
  
  switch(
    metric,
    AIC = {
      mat_fit <- get_AIC(x)
    },
    BIC = {
      mat_fit <- get_BIC(x)
    },
    m2LL = {
      mat_fit <- get_m2LL(x)
    }
  )
  
  dim(mat_fit) <- dim(x)
  dimnames(mat_fit) <- dimnames(x)
  
  for (i in seq_len(ncol(mat_fit))) {
    mat_fit[, i] <- order(mat_fit[, i])
  }
  
  res <- rep(rownames(mat_fit),
             ncol(mat_fit))[as.vector(mat_fit == 1)]
  names(res) <- colnames(mat_fit)
  
  res
}

#' Extract Transition Probabilities from a Survival Model
#' 
#' Get probabilities from fitted survival models, to be used
#' by a transition matrix.
#' 
#' If \code{use_km_until = 0}, then only model probabilities
#' will be used.
#' 
#' The transition probabilities are estimated from a 
#' parametric fit to survival data. This function is to be 
#' used when specifying transition probabilities based on a 
#' survival model.
#' 
#' @param x Either an object of 
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
get_probabilities <- function(x, ...){
  UseMethod("get_probabilities")
}

#' @rdname get_probabilities
#' @export
get_probabilities.flexsurvreg <- function(x, cycle,
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

#' @rdname get_probabilities
#' @export
get_probabilities.list <- function(x, cycle,
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

#' @rdname get_probabilities
#' @export
get_probabilities.fit_survival <- function(x, cycle,
                                           km_limit = 0,
                                           cycle_length = 1,
                                           type = c("prob", "surv"),
                                           group = NULL,
                                           metric = c("AIC","BIC","m2LL")) {
  if (is.null(group) && ! "no_group" %in% colnames(x)) {
    stop("'group' must be supplied when fit is performed by group.")
  }
  
  type <- match.arg(type)
  metric <- match.arg(metric)
  
  best_fit <- get_best_fit(x, metric = metric)
  
  get_probabilities(
    x[[best_fit[group], group]],
    cycle = cycle,
    km_limit = km_limit,
    cycle_length = cycle_length,
    type = type
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
    km_limit = use_km_until,
    cycle = markov_cycle,
    cycle_length = markov_cycle_length, 
    type = "surv"
  )
  
  os_surv <- get_probabilities(
    x$os, 
    km_limit = use_km_until,
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

get_AIC <- function(x) {
  lapply(x, stats::AIC) %>% 
    unlist()
}

get_BIC <- function(x) {
  lapply(
    x,
    function(mod) {
      (-2*getElement(mod, "loglik") +
         (log(as.numeric(getElement(mod, "N")))*
            getElement(mod, "npars"))) 
    }
  ) %>% 
    unlist()
}

get_m2LL <- function(x) {
  lapply(
    x,
    function(mod) {
      - 2 * getElement(mod, "loglik")
    }
  ) %>% 
    unlist()
}
