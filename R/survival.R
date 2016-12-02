#' Title Get survival analysis curves from data
#'
#' @param base_dir base directory for the analysis.
#' @param surv_dir subdirectory in which survival data and fits are stored.
#' @param data_files files containing survival data.
#' @param fit_files Files containing fits.
#' @param fit_names name(s) to save the fits to, if they are being saved in a file
#' @param time_col_name,censor_col_name,treatment_col_name
#'    columns to be used in the analysis
#' @param fit_metric  Once of AIC, BIC, or m2LL
#' @param best_only should only the best fit for each condition
#'   be returned?
#' @param use_envir An environment for the results to be saved in.
#'
#' @return A list with two elements:  \itemize{
#'    \item{\code{best_models}, 
#'    a list with the fits for each data file passed in; and} 
#'    \item{\code{envir}, 
#'    an environment containing the models so they can be referenced to 
#'    get probabilities.}
#'    }
#' @details If data_files is NULL and fit files exists, then fit_files
#'   should have the names of files in which survival models are kept.
#'   If data_files is not NULL, then survival models will be fit from
#'   the data in data_files (using the \code{flexsurvreg} package), and if fit_files
#'   is also not NULL, the survival models will be saved in fit_files.
#'   
#'   If \code{best_only = TRUE}, then \code{best_models} will be a vector
#'   of models, one for each group.   
#'   If \code{best_only = FALSE}, \code{best_models}
#'   will have a matrix of models, with each row corresponding to a 
#'   distribution and each column to a group.
#'
survival_from_data <- 
  function(base_dir, surv_dir, data_files = NULL, 
           fit_files = NULL, fit_names = NULL, 
           time_col_name = "time",
           censor_col_name = "status",
           treatment_col_name = "treatment",
           fit_metric = "AIC",
           best_only = TRUE,
           dists = c("exp", "weibull", "lnorm", "gamma", 
                     "gompertz", "gengamma"),
           use_envir = NULL){

    if(length(data_files) == 0 & length(fit_files) == 0)
      stop("must specify at least one of data_files or fit_files")
    if(length(fit_files) > 0 & 
       length(fit_files) != length(fit_names))
      stop(paste("must specify the same number of fit_files and",
                  "fit_names if any fit_names are specified."))
    
    if(!is.null(base_dir))
      full_path <- file.path(base_dir, surv_dir)
    else
      full_path <- surv_dir

    if(length(data_files) > 0){
      ## read data from files and fit models 
      survdata <- lapply(data_files, function(this_file){
        ## if the "file name" begins with "list(", then
        ##   it just needs to be evaluated into a list.
        really_a_list_expr <- substring(this_file, 1, 5) == "list("
        if(really_a_list_expr){
          res <- eval(parse(text = this_file))
        }
        else{
          res <- read_file(file.path(full_path, this_file))
          class(res) <- c("survdata", class(res))
        }
        res
      })
      ## now if we have a list from above, leave it alone; 
      ##  otherwise fit the data
      best_models <- lapply(survdata, function(this_data){
          if(class(this_data)[1] == "list"){
            this_data
          }
          else{
            surv_models <- f_fit_survival_models(this_data,
                   dists = dists,
                   time_col_name = time_col_name, 
                   censor_col_name = censor_col_name, 
                   treatment_col_name = treatment_col_name,
                   covariate_col_names = NULL, 
                   fit_indiv_groups = TRUE)
            if(best_only)
              apply(surv_models, 2, f_get_best_surv_model, 
                           metric = fit_metric)
            else
              surv_models
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

#' 
#' Fit survival models using different distributions
#'   
#' @param survdata Survival data to be used.
#' @param dists Distributional forms to be considered in fitting using
#'     \code{flexsurvreg}.  By default, includes exponential, Weibull, 
#'     lognormal, gamma, gompertz, and generalized gamma.  Kaplan-Meier
#'     curves will also be stored automatically. 
#' @param treatment_col_name Name of the column in survdata that holds the
#'     treatment group names (for example "control", "treatment", "dose1", and 
#'     so on).
#' @param time_col_name Name of the column in survdata with event times.
#' @param censor_col_name Name of the column in survdata with censorship
#'     indicators.   0:  event observed; 1:  censored.
#' @param covariate_col_names  Not yet implemented
#' @param fit_indiv_groups Should groups be fit individually?  Default TRUE.
#' @return a matrix (with dimnames) of flexsurvreg objects, 
#'     with rows corresponding to
#'     the distributions, and columns corresponding to groups.
#' @details  We don't return a special object for the Kaplan-Meier curve; that
#'     does not depend on the distribution, and can be obtained from
#'     any of the fitted objects by using the summary.flexsurvreg function
#'     with type = "survival". 
f_fit_survival_models <- 
  function(survdata,
           dists = c("exp", "weibull", "lnorm", "gamma", 
                     "gompertz", "gengamma"),
           time_col_name, 
           censor_col_name, 
           treatment_col_name,
           covariate_col_names = NULL, 
           fit_indiv_groups = TRUE){
    ##
    ## TODO:  so far, covariate_col_names is only a placeholder
    ##  what needs to be decided is whether to allow all combinations
    ##  of covariates, or have people enter the combinations they want,
    ##  or somehow allow both ways (which might be better handled by
    ##  different functions entering in)
    if(!requireNamespace("flexsurv", quietly = TRUE))
      stop("flexsurv packaged needed to fit survival models")
    stopifnot(is.data.frame(survdata))
    stopifnot(nrow(survdata) > 0)
    stopifnot(time_col_name %in% names(survdata))
    stopifnot(censor_col_name %in% names(survdata))
    stopifnot(treatment_col_name %in% names(survdata))
    stopifnot(is.null(covariate_col_names) |
                all(covariate_col_names %in% names(survdata)))
    ## make a list of the subgroups, and add a group of all together
    unique_groups <- as.character(unique(survdata[, treatment_col_name]))  
    groups_list <- c(list(unique_groups))
    names(groups_list) <- c("all")
    if(fit_indiv_groups)
    {
      groups_list <- c(as.list(unique_groups), list(unique_groups))
      names(groups_list) <- c(unique_groups, "all")
    }
    
    formula_base_string <- paste("survival::Surv(", time_col_name, ", ", 
                                 censor_col_name, ")",
                                 " ~", sep = "")
    
    ## cycle through combinations of distributions and subsets,
    ##   getting survival analysis results at each step
    conditions <- expand.grid(dist = dists, group = names(groups_list),
                              stringsAsFactors = FALSE)
    all_res <- 
      lapply(1:nrow(conditions), function(i){
        this_group_set <- groups_list[[ conditions[i, "group"] ]]
        this_data <- 
          survdata[survdata[[treatment_col_name]] %in% this_group_set,]
        ## only include the treatment group in the formula
        ##   if there is more than one treatment in the data set
        num_treatments <- length(unique(this_data[, treatment_col_name]))
        if(num_treatments > 1)
          this_formula <- paste(formula_base_string, treatment_col_name)
        else
          this_formula <- paste(formula_base_string, "1")
        flexsurv::flexsurvreg(stats::as.formula(this_formula), data = this_data,
                              dist = conditions[i, "dist"])
      })
    dim(all_res) <- c(length(dists), length(names(groups_list)))
    dimnames(all_res) <- list(dists, names(groups_list))
    all_res
  }



#' Choose the best model out of a set based on a metric.
#'   
#' @param surv_fits A list object from \code{f.get.survival.probs} that gives
#' 		a collection (list) of parametric survival fit object. 
#' @param metric The metric to choose the model by.
#' 		Currently supports selecting the best model using one (and only one) of the
#' 		following metrics:
#' 			1. "AIC": Akaike information criterion
#' 			2. "BIC": Bayesian information criterion
#' 			3. "m2LL": -2*log likelihood
#' @details
#' Gets the best survival model, according to a particular metric.
#' 	Current implementation is limited to selecting the best model
#' 	 using one (and only one) of the following metrics:
#' 		1. Akaike information criterion (AIC)
#' 		2. Bayesian information criterion (BIC)
#' 		3. -2*log likelihood (-2LL)
#' @return The best model according to the chosen metric.

f_get_best_surv_model <-
  function(surv_fits,
           metric=c("AIC")) {
    
    #argument checks
    stopifnot(length(surv_fits) > 0)
    stopifnot(length(metric)==1)
    stopifnot(metric %in% c("AIC","BIC","m2LL"))
    
    #order surv_fits by metric priority
    sorted = surv_fits[order(sapply(surv_fits,'[[',metric))]
    
    #return best model
    best_model <- sorted[[1]]
    attr(best_model, "dist") <- names(sorted)[1]
    best_model
  }

#'
#' Transition probabilities from a survival model. 
#' @param x Either an object of \code{\link[flexsurv]{flexsurvreg}} 
#'   or a list specifying a distribution and its arguments.
#' @param surv_fit An object of class \code{\link[flexsurv]{flexsurvreg}} 
#'   that gives	the parametric survival fit object. 
#' @param dist_list A list object with \code{dist_name} and other elements
#'   giving appropriate arguments to \code{dist_name}
#' @param use_km_until Up to what time should Kaplan-Meier estimates be used?
#'   Model predictions will be used thereafter.
#'   See \code{Details}. 
#' @param markov_cycle The Markov cycles for which to predict.
#' @param markov_cycle_length The value of a Markov cycle in absolute time units.
#' @param pred_type either \code{prob}, for transition probabilities, or 
#'   \code{surv}, for survival
#' @param ... arguments to be passed through by UseMethod
#' @details
#' 	The transition probabilities are estimated from a parametric fit to survival data. 
#' 	This function is to be used when specifying transition probabilities based on 
#' 	a survival model.
#' 
#' 	Kaplan-Meier probabilities will be used until time \code{use_km_until}.	
#' 	For example: if \code{use_km_until = 10},
#' 	then KM estimates will be used for Markov cycles up to number 9,
#' 	and model predictions	would be used thereafter.
#' 	
#' 	If \code{use_km_until = 0}, then only model probabilities will be used.
#' 	\code{use_km_until < 0} will produce an error.
#' 	
#' 	\code{use_km_until} is used only when an object of class \code{flexsurvreg}
#' 	  is passed to the function, because when only a distribution with arguments
#' 	  is passed, there is no Kaplan-Meier curve.

#'   @return
#'   Returns the Markov transition probability for the current Markov cycle,
#' 		as estimated by the Kaplan-Meier curve or from a parametric fit to 
#' 		survival data, as specified by use_km_until.
#' 		@examples 
#' 		trans_probs_from_surv(list(dist_name = "exp", rate = 0.01))

trans_probs_from_surv <-
  function(x, ...){
    UseMethod("trans_probs_from_surv")
  }

#' @rdname trans_probs_from_surv
trans_probs_from_surv.flexsurvreg <-
  function(surv_fit,
           use_km_until = 0,
           markov_cycle,
           markov_cycle_length = 1,
           pred_type = c("prob", "surv")) {
    #argument checks
    stopifnot(length(surv_fit) > 0)
    stopifnot(all(markov_cycle > 0))
    stopifnot(markov_cycle_length > 0)
    stopifnot(use_km_until >= 0)
    stopifnot(length(use_km_until <- unique(use_km_until)) == 1)
    pred_type <- match.arg(pred_type)
    ##    if(pred_type == "surv" & use_km_until == 0)
    ##      stop('prediction of "surv" only with Kaplan-Meier
    ##           probabilities (use_km_until > 0)')
    
    #get times in absolute (not Markov) units
    times_surv = markov_cycle_length * c(0, markov_cycle)
    
    preds <- rep(NA, length(markov_cycle))
    
    if(use_km_until > 0)
      use_km_pred <-
      data.frame(time = c(0, use_km_until + 1e-6),
                 method = c("km", "pred"))
    else
      use_km_pred <- data.frame(time = 0, method = "pred")
    prob_type <- look_up(use_km_pred,
                         time = markov_cycle,
                         bin = "time",
                         value = "method")
    use_km <- prob_type %in% c("KM", "km")
    use_pred <- prob_type == "pred"
    if (any(use_km)) {
      ## get KM-based probabilities - code taken from
      ## plot.flexsurvreg
      mf <- stats::model.frame(surv_fit)
      Xraw <- mf[, attr(mf, "covnames.orig"), drop = FALSE]
      dat <- surv_fit$data
      isfac <- sapply(Xraw, is.factor)
      mm <-  as.data.frame(stats::model.matrix(surv_fit))
      form <-
        "survival::Surv(dat$Y[,\"start\"],dat$Y[,\"stop\"],dat$Y[,\"status\"]) ~ "
      
      form <- paste(form,
                    if (surv_fit$ncovs > 0 && all(isfac))
                      paste("mm[,", 1:surv_fit$ncoveffs, "]", collapse = " + ")
                    else
                      1)
      form <- stats::as.formula(form)
      
      #only solve KM until last TTE, and then 
      #  assign the rest NA, which will cause an error
      max.time = floor(max(dat$Y[, "stop"]))
      times_KM = intersect(times_surv, c(0:max.time))
      ## times_KM <- times_surv
      km <-
        summary(survival::survfit(form, data = mm), times = times_KM)
      if(pred_type == "prob"){
        km_cumhaz <- -log(km$surv)
        km_haz <- diff(km_cumhaz)
        KM_tr_prob <- 1 - exp(-km_haz)
        delta = length(times_surv) - length(times_KM)
        ## setting 0 beyond what we have because NA throws an error
        ## need to check through this more thoroughly
        KM_tr_prob = c(KM_tr_prob, rep(0, delta))
        preds[use_km] <- KM_tr_prob[use_km]
      }
      else
        preds[use_km] <- km$surv[-length(km$surv)][use_km]
    }
    if (any(use_pred)) {
      if (inherits(surv_fit, "flexsurvreg")) {
        #get pred-based probabilities
        if(pred_type == "prob"){
          tmp <-
            as.data.frame(flexsurv::summary.flexsurvreg(surv_fit,
                                                        t = times_surv,
                                                        type = "cumhaz"))
          pred_tr_prob <- 1 - exp(-diff(tmp$est))
          preds[use_pred] <- pred_tr_prob[use_pred]
        }
        if(pred_type == "surv"){
          preds[use_pred] <- 
            flexsurv::summary.flexsurvreg(surv_fit,
                                          t = times_surv,
                                          type = "survival")[[1]]$est[-1][use_pred]
        }
      }
    }
    preds
  }


#' @rdname trans_probs_from_surv
trans_probs_from_surv.list <-
  function(dist_list,
           use_km_until,
           markov_cycle,
           markov_cycle_length = 1,
           pred_type = c("prob", "surv")) {
    stopifnot("dist_name" %in% names(dist_list))
    this_fn <- paste0("r", dist_list$dist_name)
    correct_names <-
      all(setdiff(names(dist_list), "dist_name") %in%
            names(formals(this_fn)))
    if (!correct_names)
      stop("the list does not give the correct arguments for the distribution")
    
    times_surv = markov_cycle_length * c(0, markov_cycle)
    Hfn <- paste0("H", dist_list$dist_name)
    args <- dist_list[-match("dist_name", names(dist_list))]
    args <- as.list(args)
    args[["x"]] <- times_surv
    cumhaz <- do.call(paste0("H", dist_list$dist_name), args)
    if(pred_type == "prob")
      pred_tr_prob <- 1 - (1 / exp(diff(cumhaz)))
    if(pred_type == "surv")
      pred_tr_prob <- exp(-cumhaz)[-1]
    pred_tr_prob
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
    trans_probs_from_surv(fits[[treatment]], 
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
#' @details See \code{trans_probs_from_surv} for further information on
#'   \code{part_surv_obj}, \code{use_km_until}, and the \code{markov_cycle}
#'   arguments.
#' @return A \code{cycle_counts} object.
#'   
#' @keywords internal
#' @rdname trans_probs_from_surv

compute_counts_part_surv <- function(part_surv_obj, 
                                     use_km_until,
                                     num_patients,
                                     markov_cycle, markov_cycle_length,
                                     state_names){
  stopifnot(length(use_km_until) == 1)
  pfs_surv <- trans_probs_from_surv(part_surv_obj$PFS_fit, 
                                    use_km_until = use_km_until,
                                    markov_cycle = markov_cycle,
                                    markov_cycle_length = markov_cycle_length, 
                                    pred_type = "surv") 
  os_surv <- trans_probs_from_surv(part_surv_obj$OS_fit, 
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
