#' Create a partitioned survival model object from tabular input
#'
#' @param base_dir directory where the reference file is located
#' @param ref_file Name of the reference file.
#' @param ref data frame read from the reference file.
#' @param df_env an environment in which survival results will be stored
#' @param state_names names of states in the strategies
#' @param save_fits should the models be saved to disk?
#' @param just_load should we just load the models instead of fitting?
#' @param ... additional arguments to be passed to 
#'   [survival_fits_from_ref_struc()]
#' @export
#' @return a tibble of partitioned survival objects 
#'   (for `part_surv_from_tabular`)
#'   or a list (for `survival_fits_from_tabular`), 
#'   with each element being a similar
#'   tibble of flexsurvreg fits for either overall survival 
#'   or progression-free survival.
#'   There are columns to distinguish the different distributions used
#'   used in survival fitting, different treatments, and different subsets.
#' @examples 
#' fit_tibble <- partitioned_survival_from_tabular(system.file("tabular/surv",
#'                                                             package = "heemod"), 
#'                                   "example_oncSpecs.csv", new.env(), 
#'                                   c("ProgressionFree", "Progressive", 
#'                                   "Terminal", "Death"), 
#'                                   save_fits = FALSE,
#'                                   just_load = FALSE)
survival_fits_from_tabular <- function(base_dir, ref_file, ...) {
  ref <- read_file(file.path(base_dir, ref_file))
  ref$full_file <- file.path(base_dir, ref$file)
  survival_fits_from_ref_struc(ref, ...)
}

#' @rdname survival_fits_from_tabular
#' @export
survival_fits_from_ref_struc <- function(ref, df_env = new.env(),
                                         save_fits = FALSE, just_load = FALSE){  
  surv_ref_full_file <- ref[ref$data == "tm", "full_file"]
  surv_ref_file <- ref[ref$data == "tm", "file"]
  ## slightly roundabout way of getting the base location back
  location <- gsub(paste0(surv_ref_file, "$"),
                   "",
                   surv_ref_full_file)
  
  ## does some error checking

  survival_specs <- read_file(file.path(surv_ref_full_file))
  dists = c("exp", "weibull", "lnorm", "gamma", 
            "gompertz", "gengamma")
  survival_from_data(location,
                     survival_specs,
                     use_envir = df_env,
                     dists = dists,
                     save_fits = save_fits,
                     just_load = just_load)
  
  
}

#' @export
#' @rdname survival_fits_from_tabular
partitioned_survival_from_tabular <- 
  function(base_dir, ref_file, df_env,
           state_names,
           save_fits = FALSE, just_load = FALSE) {
  
  ref <- read_file(file.path(base_dir, ref_file))
  
  surv_inputs <- survival_fits_from_tabular(base_dir, ref_file, df_env, 
                             save_fits, just_load)
  part_survs_from_surv_inputs(surv_inputs[[1]], state_names)
}

#' @export
#' @rdname survival_fits_from_tabular
partitioned_survival_from_ref_struc <- function(ref, df_env,
                                              state_names,
                                              save_fits = FALSE, 
                                              just_load = FALSE) {
  surv_inputs <- survival_fits_from_ref_struc(ref, df_env, 
                                            save_fits, just_load)
  part_survs_from_surv_inputs(surv_inputs[[1]], state_names)
}

#' Title Get survival analysis curves from data
#'
#' @param location base directory for the analysis.
#' @param survival_specs a data.frame containing information
#'   on how to perform each fit - see @details.
#' @param dists the distributions to use to fit the survival function
#' @param use_envir An environment for the results to be saved in.
#' @param save_fits should fits be saved to disk?  Can be useful for testing.
#' @param just_load If TRUE, data files are ignored in favor of loading fits
#'    from `fit_files`
#' @param set_definitions definitions of different subsets to fit
#' @return A list with two elements:  \itemize{
#'    \item{`best_models`, 
#'    a list with the fits for each data file passed in; and} 
#'    \item{`envir`, 
#'    an environment containing the models so they can be referenced to 
#'    get probabilities.}
#'    }
#' @details By default, the function fits with six different distribution fucntions:
#' exponential,  Weibull,  lognormal, Gompertz, gamma, and generalized gamma.
#' 
#' survival_specs contains information about how to create the fits:
#'   the directory (or directories) in which
#'   files are located, the file names, the names of the columns
#'   that should be used for time, events, and censorship status,
#'   and where and under what names to save the fits.
#' If data_files is NULL and fit files exists, then fit_files
#'   should have the names of files in which survival models are kept.
#'   If data_files is not NULL, then survival models will be fit from
#'   the data in data_files (using the `flexsurvreg` package), and if fit_files
#'   is also not NULL, the survival models will be saved in fit_files.
#'   


survival_from_data <- 
  function(location,
           survival_specs,
           use_envir = df_env,
           dists = dists,
           save_fits = TRUE,
           just_load = FALSE,
           set_definitions = NULL){
    
    survival_specs <- check_survival_specs(survival_specs)
    
    fit_files <- file.path(location,
                           survival_specs$fit_directory,
                           survival_specs$fit_file)
    if(just_load){
      for(index in seq(along = fit_files)){
        this_fit_file <- fit_files[index]
        this_fit_name <- survival_specs$fit_name[index]
        load(paste(this_fit_file, ".RData", sep = ""))
        if(!is.null(use_envir))
          assign(this_fit_name, 
                 value = get(this_fit_name),
                 envir = use_envir)
      }
      surv_models <- mget(survival_specs$fit_name)
    }
    else{
      data_files <- file.path(location,
                              survival_specs$data_directory,
                              survival_specs$data_file)

      
     surv_models <- 
       lapply(seq(1:nrow(survival_specs)),
             function(this_row){
                this_treatment <- 
                  survival_specs[this_row, "treatment"]
                this_data <- 
                 read_file(data_files[this_row]) %>%
                  dplyr::filter(treatment == this_treatment)

                ## get set definitions, if there are any
                ## (if not, will return a data frame with no rows)
                set_definitions <- 
                  get_set_definitions(file.path(location, 
                                                survival_specs$data_directory[this_row]))
                if(!("time_subtract" %in% names(set_definitions)))
                  set_definitions <- dplyr::mutate(set_definitions, time_subtract = 0)
                
                set_definitions <- 
                  set_definitions %>% 
                    dplyr::mutate(time_subtract = ifelse(is.na(time_subtract), 0, time_subtract))
                neg_offset <- which(set_definitions$time_subtract < 0)
                if(length(neg_offset))
                  stop("bad offset in set_definitions line(s) ",
                       paste(neg_offset, collapse = ", ")
                  )
                
                class(this_data) <- c("survdata", class(this_data))
               
               these_sets <- 
                 dplyr::filter(set_definitions, 
                               treatment == this_treatment)
               if(nrow(these_sets) == 0) 
                 these_sets <- data.frame(set_name = "all",
                                          condition = "TRUE",
                                          time_subtract = 0,
                                          stringsAsFactors = FALSE)
               
               surv_fits <- 
                 lapply(1:nrow(these_sets), function(set_index){
                   subset_data <-
                     this_data %>% 
                     filter_(.dots = strsplit(these_sets[set_index, 
                                                         "condition"],
                                              ",")[[1]])
                   ## if requested, subtract off the appropriate time
                   ## (this is for fitting subsets that are defined by
                   ##    being after a certain time, and we want to
                   ##    fit as if the cutoff time is the new base time)
                   to_subtract <- these_sets[set_index, "time_subtract"]
                   if(length(to_subtract) > 0 && !is.na(to_subtract)){
                     this_time_col <- survival_specs[this_row, "time_col"]
                     subset_data[, this_time_col] <- 
                       subset_data[, this_time_col] - to_subtract
                   }
                   these_surv_fits <- 
                     f_fit_survival_models(subset_data, #this_data,
                                           dists = dists,
                                           time_col_name = survival_specs[this_row, "time_col"], 
                                           censor_col_name = survival_specs[this_row, "censor_col"], 
                                           treatment_col_name = survival_specs[this_row, "treatment_col"],
                                           covariate_col_names = NULL, 
                                           fit_indiv_groups = TRUE)
                   ## attr(these_surv_fits, "set_definition") <- these_sets[[set_index, "condition"]]
                   these_surv_fits$set_name <- these_sets[[set_index, "set_name"]]
                   these_surv_fits$set_def <- these_sets[[set_index, "condition"]]
                   these_surv_fits$time_subtract <- these_sets[[set_index, "time_subtract"]]
                   
                   ## modify the fits to take into account
                   ##   the time subtraction
                   new_fits <- 
                     these_surv_fits %>%
                        dplyr::rowwise() %>%
                          dplyr::do(fit = apply_shift(dist = .$fit, 
                                                      shift = .$time_subtract)) %>%
                            dplyr::ungroup()
                          
                   
                   these_surv_fits$fit <- new_fits$fit
  
                   these_surv_fits
                 }
                 )
               names(surv_fits) <- these_sets$set_name
               surv_fits_tib <- do.call("rbind", surv_fits)
               surv_fits_tib$type <- survival_specs[this_row, "type"]
               surv_fits_tib <- 
                 surv_fits_tib[, c("type", "treatment", "set_name", "dist",
                                   "fit", "set_def", "time_subtract"),
                               drop = FALSE]
               assign(survival_specs[this_row, "fit_name"],
                      surv_fits_tib)
               if(save_fits){
                 save(list = survival_specs[this_row, "fit_name"],
                      file = paste(fit_files[this_row], ".RData", sep = ""))
               }
                 
               surv_fits_tib
             }
      )
    }

    names(surv_models) <- survival_specs$fit_name
    if(!is.null(use_envir)){
      for(i in seq(along = surv_models)){
        assign(survival_specs$fit_name[i], 
             value = surv_models[[i]],
             envir = use_envir)
      }
    }
    list(do.call("rbind", surv_models), env = use_envir)
  }

get_set_definitions <- function(data_dir){
  set_definitions <- data.frame(treatment = character(0), type = character(0))
  set_definition_file_name <- 
    list.files(data_dir, pattern = "set_definitions", full.names = TRUE)
  if(length(set_definition_file_name) > 1)
    stop("can only have one file with the name 'set_definition'")
  if(length(set_definition_file_name) == 1)
    set_definitions <- read_file(set_definition_file_name)
  set_definitions
}

## make sure that survival fit specifications are 
##   properly formatted and have reasonable data
check_survival_specs <- 
  function(surv_specs){
    
    if(!is.data.frame(surv_specs) || nrow(surv_specs) == 0)
      stop("surv_specs must be a data frame with at least one row")
    
    surv_spec_col_names <- c("type", "treatment", "data_directory", "data_file",
                            "fit_directory", "fit_name", "fit_file",
                            "time_col", "treatment_col", "censor_col")
    if(! identical(names(surv_specs), surv_spec_col_names)){
      extra_names <- setdiff(names(surv_specs), surv_spec_col_names)
      missing_names <- setdiff(surv_spec_col_names, names(surv_specs))
      names_message <- paste("surv_ref must have column names:\n",
                       paste(surv_spec_col_names, collapse = ", "), 
                       sep = "")
      if(length(extra_names) > 0)
        names_message <- paste(names_message, "\n", "extra names: ",
                         paste(extra_names, collapse = ", "),
                         sep = "")
      if(length(missing_names) > 0)
        names_message <- paste(names_message, "\n", "missing names: ",
                         paste(missing_names, collapse = ", "),
                         sep = "")
      stop(names_message)
    }

    if(any(is.na(surv_specs) | surv_specs == ""))
      stop("all elements of surv_specs must be filled in")
    
    ## sort survival specs by treatment, then PFS and OS
    ## our checks will make sure that we have the right entries,
    ##   and that they are in the right order
    surv_specs <- 
      surv_specs %>% dplyr::arrange(treatment, desc(type))
    
    os_ind <- grep("os", surv_specs$type, ignore.case = TRUE)
    pfs_ind <- grep("pfs", surv_specs$type, ignore.case = TRUE)
    all_even_os <- identical(as.numeric(os_ind), 
                             seq(from = 2, 
                                 to = nrow(surv_specs),
                                 by = 2))
    all_odd_pfs <- identical(as.numeric(pfs_ind), 
                             seq(from = 1, 
                                 to = nrow(surv_specs),
                                 by = 2))
    same_treatments <- 
      identical(surv_specs$treatment[os_ind],
                surv_specs$treatment[pfs_ind])
    if(!all_even_os | !all_odd_pfs | !same_treatments)
      stop("each treatment must have exactly one PFS and one OS entry")

    if(any(dups <- duplicated(surv_specs[, c("type", "treatment")]))){
      print(surv_specs[dups,])
      stop("survival fit specification can only have one row for fitting ",
           "OS or PFS for a given treatment\n")
    }
    if(any(dups <- duplicated(surv_specs[, c("fit_directory", "fit_file", 
                                             "time_col", "censor_col")]))){
      print(surv_specs[dups,])
      stop("can only specify a given data file in a given directory with ",
           "the same time column and censoring column for one fit")
    }
    surv_specs
  }

dist_from_fits <- function(this_fit){
  attr(this_fit, "dist")
}

#' 
#' Fit survival models using different distributions
#' @export
#' @param survdata Survival data to be used.
#' @param dists Distributional forms to be considered in fitting using
#'     `flexsurvreg`.  By default, includes exponential, Weibull, 
#'     lognormal, gamma, gompertz, and generalized gamma.  Kaplan-Meier
#'     curves will also be stored automatically, with distribution
#'     name "km".
#' @param treatment_col_name Name of the column in survdata that holds the
#'     treatment group names (for example "control", "treatment", 
#'     "dose1", and so on).
#' @param time_col_name Name of the column in survdata with event times.
#' @param censor_col_name Name of the column in survdata with censorship
#'     indicators.   
#' @param covariate_col_names  Not yet implemented
#' @param fit_indiv_groups Should groups be fit individually?  Default TRUE.
#' @return a matrix (with dimnames) of flexsurvreg objects, 
#'     with rows corresponding to
#'     the distributions, and columns corresponding to groups.
#' @details 
#'  `survdata` should be a data frame of the form required for 
#'  [flexsurv::flexsurvreg()].   In the column specified
#'  by `censor_col_name` in the data itself,
#'  1 means that an event (frequently death or disease progression,
#'  depending on context) was observed, while 0
#'  means the event was censored.  So the Kaplan-Meier plot will have
#'  a drop anywhere the censor column contains a 1, and will not
#'  contain a drop when the censor column contains a 0.
#'  
f_fit_survival_models <- 
  function(survdata,
           dists = c("exp", "weibull", "lnorm", "llogis",
                     "gamma", "gompertz", "gengamma"),
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
    ##groups_list <- c(list(unique_groups))
    ##names(groups_list) <- c("all")
    ##if(fit_indiv_groups)
    ##{
    ##  if("all" %in% unique_groups)
    ##    stop("can't fit individual treatments if there is a treatment 'all'")
    ##  groups_list <- c(as.list(unique_groups), list(unique_groups))
    ##  names(groups_list) <- c(unique_groups, "all")
    groups_list <- as.list(unique_groups)
    names(groups_list) <- unique_groups
    ##}
    
    formula_base_string <- paste("survival::Surv(", time_col_name, ", ", 
                                 censor_col_name, ")",
                                 " ~", sep = "")
    
    ## we're going to add "km" to the fit distributions
    ##   and put a survfit object there
    
    dists <- c(dists, "km")
    
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
        this_formula <- stats::as.formula(this_formula)
        if(conditions[i, "dist"] == "km"){
          this_fit <- survival::survfit(this_formula,
                                        data = this_data) 
        }
        else{
          this_fit <- try(
            flexsurv::flexsurvreg(this_formula, 
                                  data = this_data,
                                  dist = conditions[i, "dist"])
        )
        }
      })
    all_res <- 
      f_add_surv_fit_metrics(all_res, metrics = c("BIC","m2LL"))

      tibble::tibble(dist = rep(dists, length(groups_list)),
                     treatment = rep(names(groups_list), each = length(dists)),
                     fit = all_res)
    
  }


#' Choose the best model out of a set based on a metric.
#'   
#' @param surv_fits A list object from `f.get.survival.probs` that gives
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
#' Calculate additional metrics to evaluate fit of survival model.
#' 
#' @param surv_fits A list object from [f_fit_survival_models()] that gives
#' 		a collection (list) of `flexsurvreg` parametric survival fit object. 
#' @param metrics Metrics to calculate.
#' @return
#'   A list object of parametric survival fits, containing additional fields for
#' 		the calculated fit metrics. 
#' @details Currently calculates only:
#' 		\itemize{\item Bayesian information criterion (BIC)
#' 		\item -2*log likelihood (-2LL)}  (Objects come with AIC already calculated.)

f_add_surv_fit_metrics <-
  function(surv_fits, metrics = c("BIC","m2LL")) {
    
    #argument checks
    stopifnot(length(surv_fits) > 0)
    stopifnot(all(metrics %in% c("BIC","m2LL")))
    
    #get current and previous time step in absolute (not Markov) units
    for(metric in metrics)
    {
      if(metric=="BIC") { surv_fits = add_BIC(surv_fits)}
      if(metric=="m2LL") { surv_fits = add_m2LL(surv_fits)}
    }
    
    #returns updated survival fits object
    surv_fits
  }


add_BIC <- function(surv_fits)
{
  out = surv_fits
  for(i in 1:length(out))
  {
    if(inherits(out[[i]], "flexsurvreg")){
      this_BIC = (-2*getElement(out[[i]], "loglik") + 
                    (log(as.numeric(getElement(out[[i]], "N")))*getElement(out[[i]], "npars")))
      out[[i]]$BIC <- this_BIC 
    }
  }
  out
}

add_m2LL <- function(surv_fits)
{
  out = surv_fits
  for(i in 1:length(out))
  {
    if(inherits(out[[i]], "flexsurvreg")){
      this_m2LL = -2*getElement(out[[i]], "loglik")
      out[[i]]$m2LL <- this_m2LL 
    }
  }
  out
}

#' Title
#'
#' @param fit_tib a tibble of fits, for example from
#'   [survival_fits_from_tabular()]
#' @param metric which metrics to add
#'
#' @return a tibble with the added columns
#' @export
#'
#' @examples
extract_surv_fit_metrics <- 
  function(fit_tib, metric = c("AIC", "BIC","m2LL")){
    ## metric <- match.arg(metric)
    fit_tib <- 
      fit_tib %>% dplyr::filter(dist != "km")
     extracted <- 
       fit_tib %>%
       dplyr::rowwise() %>%
       dplyr::do(data.frame(.$fit[metric]))
     tibble::as_tibble(cbind.data.frame(fit_tib, extracted))
  }
