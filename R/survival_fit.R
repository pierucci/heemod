#' Create a partitioned survival model object from tabular input
#'
#' @param base_dir directory where the reference file is located
#' @param ref_file Name of the reference file.
#' @param df_env an environment in which survival results will be stored
#' @param state_names names of states in the models
#' @param model_names names of the models
#'
#' @return a matrix of partitioned survival objects.    The rows
#'   correspond to different distributions used in survival fitting,
#'   the columns to different subsets of the data.
#'
partitioned_survival_from_tabular <- function(base_dir, ref_file, df_env, 
                                              state_names, model_names) {
  ref <- read_file(file.path(base_dir, ref_file))
  ref$full_file <- file.path(base_dir, ref$file)
  
  surv_ref_full_file <- ref[ref$data == "survivalInformation", "full_file"]
  surv_ref_file <- ref[ref$data == "survivalInformation", "file"]
  ## slightly roundabout way of getting the base location back
  location <- gsub(paste0(surv_ref_file, "$"),
                   "",
                   surv_ref_full_file)
  
  ## does some error checking
  si <- get_survival_input(read_file(surv_ref_full_file)) 
  
  ## load or fit survival models into the environment
  surv_inputs <-
    survival_from_data(location, #ref[ref$data == "survivalDataDirectory", "full_file"],
                       si$surv_data_dir,
                       data_files = si$surv_data_files,
                       fit_files = si$fit_files,
                       fit_names = si$fit_names,
                       fit_metric = si$fit_metric,
                       time_col_name = si$time_col_name,
                       censor_col_name = si$censor_col_name,
                       treatment_col_name = si$treatment_col_name,
                       dists = si$dists,
                       best_only = FALSE,
                       use_envir = df_env)
  
  ## switch the survival information to be by model instead of by fit type,
  ##  and put into part_surv format; we'll end up with a matrix
  
  os_fit_ind <- grep("os", si$fit_names, ignore.case = TRUE)
  pfs_fit_ind <- grep("pfs", si$fit_names, ignore.case = TRUE)

  pieces <- seq_len(length(surv_inputs[[1]][[pfs_fit_ind]]))
  
  part_survs <-
    lapply(pieces, function(this_piece) {
      define_part_surv(
        pfs = surv_inputs[[1]][[pfs_fit_ind]][[this_piece]],
        os = surv_inputs[[1]][[os_fit_ind]][[this_piece]],
        state_names = state_names
      )
    })
  
  dim(part_survs) <- dim(surv_inputs[[1]][[pfs_fit_ind]])
  dimnames(part_survs) <- dimnames(surv_inputs[[1]][[pfs_fit_ind]])
  class(part_survs) <- c("part_surv_mat", "matrix")
  part_survs
}


#' Make sure survival inputs have the correct formats
#'
#' @param surv_ref - data frame with survival data information
#'
#' @details For survival analysis, we need, for each condition
#'   (frequently progression-free survival and overall survival)
#'   at least two, and possibly three, elements represented in
#'   the survival reference file.
#'   \itemize{
#'     \item{\code{surv_data_file}:  the name of the file 
#'        with the survival data}
#'     \item{\code{fit_file}: the name of a file with fits.  If
#'     \code{surv_data_file} is not provided, the fit will
#'     be read in from this file; if \code{surv_data_file}
#'     is provided, the fit will be written into this file.}
#'     \item{\code{fit_name}:  name to give the survival fit so
#'     that it can be referred to to extract probabilities.}
#'     } 
#'     The names for the three fields are `fit_name_<label>`,
#'     `surv_data_file_<label>`, and `fit_file_<label>`, where
#'     `<label>` identifies that the fields go together for a single fit.
#'     The labels for the two conditions must be `pfs` and `os` 
#'     (or `PFS` and `OS'; they are not case sensitive).
                                   
#' At least one of \code{surv_data_file} and \code{fit_file}
#'   must be provided.
#' @return a list with elements \itemize{\item{fit_files},
#'   \item{fit_names}, \item{surv_data_files}, \item{fit_metric}}.
#'
get_survival_input <- function(surv_ref) {
  
  if(! identical(names(surv_ref), c("data", "val")))
    stop(
      paste("surv_ref must have column names 'data' and 'val'",
            paste("(actual_names", paste(names(surv_ref),collapse = ", "),
                  ")"
            )
      )
    )
  surv_data_dir <- 
    surv_ref[surv_ref$data == "survivalDataDirectory", "val"]
  time_col_name <- "time"
  censor_col_name <- "status"
  treatment_col_name <- "treatment"
  
  dists <- c("exp", "weibull", "lnorm", "gamma", 
             "gompertz", "gengamma")
  
  time_col_index <- grep("time_col_name", surv_ref$data)
  censor_col_index <- grep("censor_col_name", surv_ref$data)
  treatment_col_index <- grep("treatment_col_name", surv_ref$data)
  dists_index <- grep("dists", surv_ref$data)
  
  if(length(time_col_index) > 1)
    stop("must have at most one time_col_name; default = 'time'")
  if(length(censor_col_index) > 1)
    stop("must have at most one censor_col_name; default = 'status'")
  if(length(treatment_col_index) > 1)
    stop("must have at most one treatment_col_name; default = 'treatment'")
  if(length(dists_index) > 1)
    stop(paste('must have at most one line specificying distributions;\n',
               'default = c("exp", "weibull", "lnorm", "gamma", 
               "gompertz", "gengamma")'))
  
  if(length(time_col_index) == 1)
    time_col_name <- surv_ref[time_col_index, "val"]
  if(length(censor_col_index) == 1)
    censor_col_name <- surv_ref[censor_col_index, "val"]
  if(length(treatment_col_index) == 1)
    treatment_col_name <- surv_ref[treatment_col_index, "val"]
  if(length(dists_index) ==1)
    dists <- surv_ref[dists_index, "val"]
  
  surv_data_indices <- grep("surv_data_file", surv_ref$data)
  fit_file_indices <- grep("fit_file", surv_ref$data)
  fit_name_indices <- grep("fit_name", surv_ref$data)
  
  surv_data_names <- surv_ref[surv_data_indices, "data"]
  fit_file_names <- surv_ref[fit_file_indices, "data"]
  fit_name_names <- surv_ref[fit_name_indices, "data"]
  
  surv_data_files = as.vector(surv_ref[surv_data_indices, "val"])
  fit_files = as.vector(surv_ref[fit_file_indices, "val"])
  fit_names = as.vector(surv_ref[fit_name_indices, "val"])
  
  fit_metric = surv_ref[surv_ref$data == "fit_metric", "val"]
  
  lff <- length(fit_file_names)
  lsdn <- length(surv_data_names)
  
  if(lff == 0 & lsdn == 0)
    stop(paste("survival reference file must define at least one of",
               "fit_file and surv_data_file for survival analysis"))
  if(lff != 0 & lsdn != 0 & (lff != lsdn))
    stop(paste("when both fit_file and surv_data_name elements are",
               "specified,\nshould have the same number of elements"))
  
  if(length(fit_names) != max(lff, lsdn))
    stop(paste("ref file should have the same number of fit_name", 
               "elements as fit_file or surv_data_file elements"))
  
  suffixes <- cbind(surv_data_names, fit_name_names, fit_file_names)
  
  
  suffixes <- gsub("fit_file", "", suffixes)
  suffixes <- gsub("fit_name", "", suffixes)
  suffixes <- gsub("surv_data_file", "", suffixes)
  
  
  for (i in 1:ncol(suffixes))
    suffixes[, i] <- sort(suffixes[, i])
  num_suffixes <- apply(suffixes, 1, function(x) {
    length(unique(x))
  })
  if (any(num_suffixes != 1)){
    print(table(suffixes))
    stop("suffixes do not match")
  }
  
  os_suffix_pos <- grep("os", suffixes[,1], ignore.case = TRUE)
  pfs_suffix_pos <- grep("pfs", suffixes[,1], ignore.case = TRUE)
  
  if(length(os_suffix_pos) != 1 | length(pfs_suffix_pos) != 1)
    stop("must have each suffix OS and PFS once (not case-sensitive)")
  if(os_suffix_pos == pfs_suffix_pos)
    stop("os and pfs suffixes must be distinct")
  
  
  fit_files <- fit_files[order(fit_file_names)]
  fit_names <- fit_names[order(fit_name_names)]
  surv_data_files <- surv_data_files[order(surv_data_names)]
  
  return(
    list(
      surv_data_dir = surv_data_dir, 
      fit_files = fit_files,
      fit_names = fit_names,
      surv_data_files = surv_data_files,
      fit_metric = fit_metric,
      time_col_name = time_col_name,
      censor_col_name = censor_col_name,
      treatment_col_name = treatment_col_name,
      dists = dists
    )
  )
  
}


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
#' @param dists the distributions to use to fit the survival function
#' @param use_envir An environment for the results to be saved in.
#'
#' @return A list with two elements:  \itemize{
#'    \item{\code{best_models}, 
#'    a list with the fits for each data file passed in; and} 
#'    \item{\code{envir}, 
#'    an environment containing the models so they can be referenced to 
#'    get probabilities.}
#'    }
#' @details By default, the function fits with six different distribution fucntions:
#' exponential,  Weibull,  lognormal, Gompertz, gamma, and generalized gamma.
#' 
#' If data_files is NULL and fit files exists, then fit_files
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
#' @export
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

