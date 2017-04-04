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
#' fit_tibble <- partitioned_survival_from_tabular(system.file("tabular\\surv",
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
  ## si <- get_survival_input(read_file(surv_ref_full_file), location) 
  
  survival_specs <- read_file(file.path(surv_ref_full_file))
  dists = c("exp", "weibull", "lnorm", "gamma", 
            "gompertz", "gengamma")
  survival_from_data_2(location,
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


#' Convert saved fits to partitioned survival objects
#'
#' @param surv_inputs a list of matrices of `flexsurvreg` objects,
#'  for example the first element of the output of `survival_from_data`.
#' @param state_names names of states of the model
#'
#' @details `surv_inputs` must have one element with "OS" or
#'   "os" in the name, containing the matrix of fits for overall survival,
#'   and another element with "PFS" or "pfs" in the name, containing
#'   the matrix of fits for progression-free survival.
#' @return a matrix of partitioned survival objects.    The rows
#'   correspond to different distributions used in survival fitting,
#'   the columns to different subsets of the data.
#' @export
#'
part_survs_from_surv_inputs <-  function(surv_inputs, state_names){
    
  ## switch the survival information to be by model instead of by fit type,
  ##  and put into part_surv format; we'll end up with a matrix
  

  
  ## surv_inputs is a tibble with columns
  ##   type (PFS or OS), treatment, set_name (for data subsets),
  ##   dist (for survival distribution assumptions),
  ##   fit (for the fitted survival object) and set_def
  ##   (how the subset of data was defined, just to keep it around)

  ## the output will be a smaller tibble, with all the columns
  ##   except type and fit, and a new column part_surv
  
  make_part_surv_from_small_tibble <- function(st, state_names){
    pfs_row <- grep("pfs", st$type, ignore.case = TRUE)
    os_row <- grep("os", st$type, ignore.case = TRUE)
    stopifnot(length(pfs_row) == 1,
              length(os_row) == 1
    )
    define_part_surv(pfs = st[[pfs_row, "fit"]],
                     os = st[[os_row, "fit"]],
                     state_names = state_names)
      }
  
  
  surv_inputs %>%
    dplyr::group_by(treatment, set_name, dist, set_def) %>%
      dplyr::do(part_surv = make_part_surv_from_small_tibble(.,
                              state_names = state_names))

  # part_survs <-
  #   lapply(subsets, function(this_subset){
  #     
  #     this_subset_part_surv <- 
  #       lapply(pieces, function(this_piece) {
  #         define_part_surv(
  #           pfs = surv_inputs[[pfs_fit_ind]][[this_subset]][[this_piece]],
  #           os = surv_inputs[[os_fit_ind]][[this_subset]][[this_piece]],
  #           state_names = state_names
  #     )
  #   })
  #     dim(this_subset_part_surv) <- 
  #       dim(surv_inputs[[pfs_fit_ind]][[this_subset]])
  #     dimnames(this_subset_part_surv) <- 
  #       dimnames(surv_inputs[[pfs_fit_ind]][[this_subset]])
  #     class(this_subset_part_surv) <- c("part_surv_mat", "matrix")
  #     this_subset_part_surv
  #   })
  # names(part_survs) <- subsets
  # part_survs
}


#' Make sure survival inputs have the correct formats
#' @param location base directory for the analysis
#' @param surv_ref data frame with survival data information
#'
#' @details For survival analysis, we need, for each condition
#'   (frequently progression-free survival and overall survival)
#'   at least two, and possibly three, elements represented in
#'   the survival reference file.
#'   \itemize{
#'     \item{`surv_data_file`:  the name of the file 
#'        with the survival data}
#'     \item{`fit_file`: the name of a file with fits.  If
#'     `surv_data_file` is not provided, the fit will
#'     be read in from this file; if `surv_data_file`
#'     is provided, the fit will be written into this file.}
#'     \item{`fit_name`:  name to give the survival fit so
#'     that it can be referred to to extract probabilities.}
#'     }
#'     The names for the three fields are `fit_name_<label>`,
#'     `surv_data_file_<label>`, and `fit_file_<label>`, where
#'     `<label>` identifies that the fields go together for a single fit.
#'     The labels for the two conditions must be `pfs` and `os` 
#'     (or `PFS` and `OS`; they are not case sensitive).
                                   
#' At least one of `surv_data_file` and `fit_file`
#'   must be provided.
#' @return a list with elements \itemize{\item{fit_files},
#'   \item{fit_names}, \item{surv_data_files}, \item{fit_metric}}.
#'
# get_survival_input <- function(surv_ref, location = NULL) {
#   
#   surv_ref_col_names <- c("type", "treatment", "data_directory", "data_file",
#                           "fit_directory", "fit_name", "fit_fil",
#                           "time_col", "treatment_col", "censor_col")
#   if(! identical(names(surv_ref), surv_ref_col_names))
#     stop(
#       paste("surv_ref must have column names:\n",
#             paste(surv_ref_col_names, sep = ", "),
#             paste("\n(actual_names", paste(names(surv_ref),collapse = ", "),
#                   ")"
#             )
#       )
#     )
# 
#   dists <- c("exp", "weibull", "lnorm", "gamma", 
#              "gompertz", "gengamma")
#   
#   time_col_index <- grep("time_col_name", surv_ref$data)
#   censor_col_index <- grep("censor_col_name", surv_ref$data)
#   treatment_col_index <- grep("treatment_col_name", surv_ref$data)
#   dists_index <- grep("dists", surv_ref$data)
#   
#   if(length(time_col_index) > 1)
#     stop("must have at most one time_col_name; default = 'time'")
#   if(length(censor_col_index) > 1)
#     stop("must have at most one censor_col_name; default = 'status'")
#   if(length(treatment_col_index) > 1)
#     stop("must have at most one treatment_col_name; default = 'treatment'")
#   if(length(dists_index) > 1)
#     stop(paste('must have at most one line specificying distributions;\n',
#                'default = c("exp", "weibull", "lnorm", "gamma", 
#                "gompertz", "gengamma")'))
#   
#   if(length(time_col_index) == 1)
#     time_col_name <- surv_ref[time_col_index, "val"]
#   if(length(censor_col_index) == 1)
#     censor_col_name <- surv_ref[censor_col_index, "val"]
#   if(length(treatment_col_index) == 1)
#     treatment_col_name <- surv_ref[treatment_col_index, "val"]
#   if(length(dists_index) ==1)
#     dists <- surv_ref[dists_index, "val"]
#   
#   set_definitions <- NULL
#   sets_index <- grep("sets", surv_ref$data)
#   if(length(sets_index) > 1)
#     stop("can have only one entry containing 'sets' ",
#          "in the survival information file.")
#   if(length(sets_index) == 1){
#     set_definitions <- read_file(file.path(location,
#                                 surv_data_dir,
#                                 surv_ref[sets_index, "val"]
#                                 )
#     )
#   }
#   surv_data_indices <- grep("surv_data_file", surv_ref$data)
#   fit_file_indices <- grep("fit_file", surv_ref$data)
#   fit_name_indices <- grep("fit_name", surv_ref$data)
#   
#   surv_data_names <- surv_ref[surv_data_indices, "data"]
#   fit_file_names <- surv_ref[fit_file_indices, "data"]
#   fit_name_names <- surv_ref[fit_name_indices, "data"]
#   
#   surv_data_files = as.vector(surv_ref[surv_data_indices, "val"])
#   fit_files = as.vector(surv_ref[fit_file_indices, "val"])
#   fit_names = as.vector(surv_ref[fit_name_indices, "val"])
#   
#   fit_metric = surv_ref[surv_ref$data == "fit_metric", "val"]
#   
#   lff <- length(fit_file_names)
#   lsdn <- length(surv_data_names)
#   
#   if(lff == 0 & lsdn == 0)
#     stop(paste("survival reference file must define at least one of",
#                "fit_file and surv_data_file for survival analysis"))
#   if(lff != 0 & lsdn != 0 & (lff != lsdn))
#     stop(paste("when both fit_file and surv_data_name elements are",
#                "specified,\nshould have the same number of elements"))
#   
#   if(length(fit_names) != max(lff, lsdn))
#     stop(paste("ref file should have the same number of fit_name", 
#                "elements as fit_file or surv_data_file elements"))
#   
#   suffixes <- cbind(surv_data_names, fit_name_names, fit_file_names)
#   
#   
#   suffixes <- gsub("fit_file", "", suffixes)
#   suffixes <- gsub("fit_name", "", suffixes)
#   suffixes <- gsub("surv_data_file", "", suffixes)
#   
#   
#   for (i in 1:ncol(suffixes))
#     suffixes[, i] <- sort(suffixes[, i])
#   num_suffixes <- apply(suffixes, 1, function(x) {
#     length(unique(x))
#   })
#   if (any(num_suffixes != 1)){
#     print(table(suffixes))
#     stop("suffixes do not match")
#   }
#   
#   os_suffix_pos <- grep("os", suffixes[,1], ignore.case = TRUE)
#   pfs_suffix_pos <- grep("pfs", suffixes[,1], ignore.case = TRUE)
#   
#   if(length(os_suffix_pos) != 1 | length(pfs_suffix_pos) != 1)
#     stop("must have each suffix OS and PFS once (not case-sensitive)")
#   if(os_suffix_pos == pfs_suffix_pos)
#     stop("os and pfs suffixes must be distinct")
#   
#   
#   fit_files <- fit_files[order(fit_file_names)]
#   fit_names <- fit_names[order(fit_name_names)]
#   surv_data_files <- surv_data_files[order(surv_data_names)]
#   
#   return(
#     list(
#       surv_data_dir = surv_data_dir, 
#       fit_files = fit_files,
#       fit_names = fit_names,
#       surv_data_files = surv_data_files,
#       fit_metric = fit_metric,
#       time_col_name = time_col_name,
#       censor_col_name = censor_col_name,
#       treatment_col_name = treatment_col_name,
#       dists = dists,
#       set_definitions = set_definitions
#     )
#   )
#   
# }


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
#' If data_files is NULL and fit files exists, then fit_files
#'   should have the names of files in which survival models are kept.
#'   If data_files is not NULL, then survival models will be fit from
#'   the data in data_files (using the `flexsurvreg` package), and if fit_files
#'   is also not NULL, the survival models will be saved in fit_files.
#'   
#'   If `best_only = TRUE`, then `best_models` will be a vector
#'   of models, one for each group.   
#'   If `best_only = FALSE`, `best_models`
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
           use_envir = NULL,
           save_fits = TRUE,
           just_load = FALSE,
           set_definitions = NULL){
    
    if(length(data_files) == 0 & length(fit_files) == 0)
      stop("must specify at least one of data_files or fit_files")
    if(length(fit_files) > 0 & 
       length(fit_files) != length(fit_names))
      stop(paste("must specify the same number of fit_files and",
                 "fit_names if any fit_names are specified."))

    if(is.null(set_definitions)) 
      set_definitions <- data.frame(set_name = "all",
                         condition = "TRUE",
                         stringsAsFactors = FALSE)
    
    if(!is.null(base_dir))
      full_path <- file.path(base_dir, surv_dir)
    else
      full_path <- surv_dir
    
    if(length(data_files) > 0 & !just_load){
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
          surv_models <- 
            lapply(seq(along = set_definitions$set_name), function(this_set){
              subset_data <- 
                this_data %>% filter_(.dots = strsplit(set_definitions[this_set, "condition"],
                                                       ",")[[1]])
              these_surv_fits <- 
                f_fit_survival_models(subset_data, #this_data,
                                  dists = dists,
                                  time_col_name = time_col_name, 
                                  censor_col_name = censor_col_name, 
                                  treatment_col_name = treatment_col_name,
                                  covariate_col_names = NULL, 
                                  fit_indiv_groups = TRUE)
            
            if(best_only)
              res <- apply(these_surv_fits, 2, f_get_best_surv_model, 
                           metric = fit_metric)
            else
              res <- these_surv_fits
            attr(res, "set_definition") <- set_definitions[[this_set, "condition"]]
            res
              }
            )
          names(surv_models) <- set_definitions$set_name
          attr(surv_models, "set_definitions") <- set_definitions
          surv_models
        }
      }
      )
      ## best_dists <- lapply(best_models, dist_from_fits)
      if(length(fit_files) > 0 & save_fits == TRUE){
        ## eventually may want a different format for saving these
        ## currently dump for a readable version, save for a complete version
        for(i in seq(along = fit_names)){
          assign(fit_names[i], best_models[[i]])
          write_to_me <- paste(file.path(full_path, fit_files[i]), ".R", sep = "")
          suppressMessages(
            ## suppressMessages because we know we're going to get a warning
            ##   that it may not be possible to completely deparse an object
            ##   from flexsurv
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


survival_from_data_2 <- 
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

      ## get set definitions, if there are any
      ## (if not, will return a data frame with no rows)
      set_definitions <- 
        get_set_definitions(file.path(location, 
                                      survival_specs$data_directory))
      
     surv_models <- 
       lapply(seq(1:nrow(survival_specs)),
             function(this_row){
                this_treatment <- 
                  survival_specs[this_row, "treatment"]
                this_data <- 
                 read_file(data_files[this_row]) %>%
                 dplyr::filter(treatment == this_treatment)
               
               class(this_data) <- c("survdata", class(this_data))
               
               these_sets <- 
                 dplyr::filter(set_definitions, 
                               treatment == this_treatment)
               if(nrow(these_sets) == 0) 
                 these_sets <- data.frame(set_name = "all",
                                          condition = "TRUE",
                                          stringsAsFactors = FALSE)
               
               surv_fits <- 
                 lapply(1:nrow(these_sets), function(set_index){
                   subset_data <-
                     this_data %>% 
                     filter_(.dots = strsplit(these_sets[set_index, 
                                                         "condition"],
                                              ",")[[1]])
                   
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
                   these_surv_fits
                 }
                 )
               names(surv_fits) <- these_sets$set_name
               ##attr(surv_fits, "set_definitions") <- set_definitions
               surv_fits_tib <- do.call("rbind", surv_fits)
               surv_fits_tib$type <- survival_specs[this_row, "type"]
               surv_fits_tib <- 
                 surv_fits_tib[, c("type", "treatment", "set_name", "dist",
                                   "fit", "set_def"),
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
    
    if(any(dups <- duplicated(surv_specs[, c("type", "treatment")]))){
      print(surv_specs[dups,])
      stop("survival fit specification can only have one row for fitting ",
           "OS or PFS for a given treatment\n")
    }
    if(any(dups <- duplicated(surv_specs[, c("fit_directory", "fit_file", 
                                             "time_col", "censor_col")]))){
      print(surv_specs[dups,])
      stop("can only specify a given data file in a given directory with ",
           "the same time column and event column for one fit")
    }
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
    all_res <- f_add_surv_fit_metrics(all_res, metrics = c("BIC","m2LL"))

    # all_res_mat <- all_res
    # dim(all_res_mat) <- c(length(dists), length(names(groups_list)))
    # dimnames(all_res_mat) <- list(dists, names(groups_list))
    # all_res_mat
     
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


#' #' recombine elements from different part_surv ojects
#' #'
#' #' @param fit_matrix a list.   See details.  
#' #' @param choices a list specifying which fits to use; see below.
#' #' @param ... additional named arguments when using `combine_part_surv`;
#' #'   see below.
#' #' @param subset the subset of data for which you want fits
#' #' @details 
#' #' 
#' #'  `fit_matrix` is a list, with each element corresponding to a subset
#' #'  of the data that was fit.  
#' #'  Each element contains amatrix of partitioned survival 
#' #'   (`part_surv`) objects, generally
#' #'   from `partitioned_survival_from_tabular`.
#' #'   Each row corresponding to a distribution and each column
#' #'   to a group or condition for which a partitioned survival object
#' #'   was created.
#' #'   The `choices` argument in `choose_part_surv_` and the
#' #'   `...` argument in `choose_part_surv` specify which elements of which fit
#' #'   survival objects to combine.  
#' #'   `choices` is a list, where the name of each element should be the name of
#' #'   one of the strategies.   Each of those elements should be a list with elements 
#' #'   named pfs and os, for
#' #'   the two elements of a partial survival object, and each element
#' #'   must be a distribution type (a distribution used in the survival
#' #'   fit).   Each name should be one of the groups 
#' #'   or subgroups; that is, a column name of `fit_matrix`. 
#' #'
#' #' @return a list of partitioned survival objects
#' #' @export
#' #'
#' #' @examples
#' #' fit_matrix <- 
#' #'   partitioned_survival_from_tabular(base_dir = system.file("tabular\\surv", 
#' #'                                                 package = "heemod"), 
#' #'                                   ref_file = "example_oncSpecs.csv", 
#' #'                                   df_env = new.env(), 
#' #'                                   state_names = c("ProgressionFree", "Progressive", 
#' #'                                   "Terminal", "Death"), 
#' #'                                   save_fits = FALSE,
#' #'                                   just_load = FALSE)
#' #' ## create new partitioned survival objects.   For group A,
#' #' ## use the exponential fit for progression-free survival, and
#' #' ## the Weibull fit for overall survival; for B, use the exponential
#' #' ## and lognormal fits respectively.
#' #' combine_part_surv(fit_matrix, A = list(pfs = "exp", os = "weibull"), 
#' #'                              B = list(pfs = "exp", os = "lnorm"),
#' #'                              subset = "all")
#' #' combine_part_surv_(fit_matrix, 
#' #'                    choices = list(A = list(pfs = "exp", os = "weibull"), 
#' #'                                   B = list(pfs = "exp", os = "lnorm"),
#' #'                                   subset = "all))
#' combine_part_surv <- function(fit_matrix, ..., subset) {
#'   choices <- list(...)
#'   combine_part_surv_(fit_matrix, choices, subset)
#' }
#' 
#' #' @export
#' #' @rdname combine_part_surv
#' combine_part_surv_ <- function(fit_matrix, choices, subset){
#'   if(!(subset %in% names(fit_matrix)))
#'     stop(subset, " must be a name of fit_matrix")
#'   fit_matrix <- fit_matrix[[subset]]
#'   if(!all(names(choices) %in% colnames(fit_matrix)))
#'     stop("names of selections must be column names ",
#'          "of fit_matrix (names of strategies)")
#'   if(!all(unlist(choices) %in% rownames(fit_matrix)))
#'     stop("values of arguments other than fit_matrix must be
#'          row names of fit_matrix")
#'   
#'   if(!all(sapply(fit_matrix, function(this_obj){
#'     inherits(this_obj, "part_surv")}))
#'   )
#'     stop("the elements of fit_matrix must be of class 'part_surv'")
#'   
#'   choice_ind <-
#'     lapply(names(choices), function(this_name) {
#'       this_choice <- choices[[this_name]]
#'       if(!identical(sort(names(this_choice)),
#'                     c("os", "pfs"))){
#'         stop(paste("only names os and pfs allowed as elements of additional arguments"))
#'       }
#'       
#'       cbind(names(this_choice),
#'             unlist(this_choice),
#'             rep(this_name, length(names(this_choice))))
#'     })
#'   
#'   part_survs <-
#'     lapply(choice_ind, function(this_ind) {
#'       ## pieces <- choice_ind[this_ind[, -1]]
#'       pfs_ind <- this_ind[match("pfs", this_ind[, 1]), 2:3]
#'       os_ind <- this_ind[match("os", this_ind[, 1]), 2:3]
#'       
#'       pfs_piece <- fit_matrix[[pfs_ind[1], pfs_ind[2]]]
#'       os_piece <- fit_matrix[[os_ind[1], os_ind[2]]]
#'       if (!identical(get_state_names(pfs_piece),
#'                      get_state_names(os_piece)))
#'         stop("state names of pfs and os parts must be the same")
#'       
#'       if (!identical(pfs_piece$cycle_length, os_piece$cycle_length))
#'         stop("cycle_length elements of pfs_piece and os_piece must be identical")
#'       
#'       define_part_surv_(
#'         pfs = pfs_piece$pfs,
#'         os = os_piece$os,
#'         state_names = get_state_names(pfs_piece),
#'         cycle_length = pfs_piece$cycle_length
#'       )
#'     })
#'   names(part_survs) <- names(choices)
#'   part_survs
#' }
#' 
#' 
#' #' Create partial survival objects from fit files
#' #'
#' #' @param file_name loadable .RData file containing a fit matrix.
#' #' @param obj_name the name of the fit matrix inside the .RData file.
#' #' @param ... additional arguments for [combine_part_surv()].
#' #'
#' #' @inherit combine_part_surv return
#' #' @export
#' #'
#' combine_part_surv_from_fit_file <- 
#'   function(file_name, obj_name, ...){
#'     load(file_name)
#'     fit_matrix <- get(obj_name)
#'     part_surv_matrix <- part_survs_from_surv_inputs(fit_matrix)
#'     combine_part_surv(part_surv_matrix, ...)
#'   }


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
