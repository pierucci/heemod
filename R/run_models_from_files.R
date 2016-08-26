#' Run Analyses Specified in Files
#' 
#' This function runs a model from tabular input.
#' 
#' The reference file should have two columns, \code{data} 
#' and \code{file}. (Additional columns are allowed, but 
#' will be ignored.) "data" values must include "state", 
#' "tm", and "parameters", and also looks for "demographics"
#' and "data".  The corresponding file values give the names
#' of the files (still in base_dir) that contain the 
#' corresponding information - or, in the case of "data", 
#' the directory containing data frames to be loaded.
#' 
#' For more information see the vignette: 
#' \code{vignette("file-input", package = "heemod")}.
#' 
#' @param base_dir directory in which \code{ref-file} will 
#'   be found.
#' @param ref_file name of a file specifying where model 
#'   inputs will be found.
#' @param N.prob, cost, effect, init arguments to 
#'   \code{\link{run_models}} and 
#'   \code{\link{run_probabilistic}}
#' @param base_model which of the models should be the base 
#'   model
#' @param save_outputs Should the outputs be saved? 
#'   Only works if the reference file specifies an output 
#'   directory.
#' @param overwrite = FALSE should the outputs be 
#'   overwritten? Not relevant if save_outputs = FALSE.
#' @param ... additional arguments passed to 
#'   \code{\link{execute_models}}.
#'   
#' @return  an unevaluated models object
#'   
#'   

#' @return a list with outputs models and model_runs
#'   (always), and, if appropriate input is provided, dsa
#'   (deterministic sensitivity analysis), psa
#'   (probabilistic sensitivity analysis) and demographics 
#'   (results across different demographic groups).
#'   
#' @export
run_models_from_files <- 
  function(base_dir, ref_file, N.prob = 50, 
           cost, effect, init = NULL, base_model = NULL, 
           save_outputs = TRUE, overwrite = FALSE, ...) {
    
    inputs <- gather_model_info(base_dir, ref_file, base_model)
    
    outputs <- execute_models(inputs, base_dir,  N.prob = 50, 
                              cost = lazyeval::lazy(cost), 
                              effect = lazyeval::lazy(effect), 
                              init = init, base_model = base_model, ...)
    
    output_dir <- inputs$output_dir
    
    if(save_outputs & is.null(output_dir)) {
      warning("Output directory not defined in the specification file - the outputs will not be saved.")
    }
    
    if(save_outputs & ! is.null(output_dir)) {
      if(dir.exists(file.path(base_dir, output_dir)) & ! overwrite) {
        warning("Output directory exists and overwrite = FALSE - the outputs will not be saved.")
      }
    } else {
      save_outputs(outputs, base_dir = base_dir, 
                   output_dir = output_dir,
                   overwrite = overwrite, create_dir = TRUE)
    }
    
    invisible(outputs)
  }

#' Gather information for running a model
#' 
#' @param base_dir directory in which \code{ref-file} will
#'   be found.
#' @param ref_file name of a file specifying where model
#'   inputs will be found.
#' @return  an unevaluated models object
#' @param base_model which of the models should be the base
#'   model
#' @return a list with elements: \itemize{ \item models (of
#'   type \code{uneval_model}, created by 
#'   \code{\link{import_models_from_files}}) \item
#'   param_info \item use_envir an environment containing 
#'   data frames with information necessary for the models
#'   to run (see \code{\link{load_df_from_files}}) \item
#'   output_dir where to store output files, if specified }
#' @details For more information see the vignette: 
#' \code{vignette("file-input", package = "heemod")}.`

#'
gather_model_info <-
  function(base_dir, ref_file, base_model = NULL) {
    
    ref <- read_file(file.path(base_dir, ref_file))
    
    ref$full_file <- file.path(base_dir, ref$file)
    
    models <- import_models_from_files(
      base_dir = base_dir,
      ref = ref,
      base_model = base_model
    )
    
    
    use_envir <- new.env()
    if ("data" %in% ref$data) {
      ## load data frames with required data into an environment
      load_df_from_files(base_dir, ref[ref$data == "data", "file"],
                         use_envir)
    }
    if (length(ls(use_envir))) {
      attach(use_envir, pos = 2L,
             name = "heemod_temp_variables_envir_detach_me")
      on.exit(detach(name = "heemod_temp_variables_envir_detach_me"))
    }
    
    param_info <-
      f_parameters_from_tabular(ref[ref$data == "parameters", "full_file"],
                                output_file_name_base = NULL,
                                param_obj_name = "params")
    if("survivalDataDirectory" %in% ref$data){
      ## load or fit survival models into the environment
      
      ## TODO - currently assumes fit_files and fit_names
      ## specified separately, and in same order (that is,
      ## the first fit_name is for the models fit using the
      ## first data, and so on, or, alternately, that the
      ## first fit_file contains the model referred to by
      ## the first fit_name).  Eventually probably want to
      ## set this up in a way that's more error-proof.
      surv_inputs <- 
        survival_from_data(base_dir, ref[ref$data == "survivalDataDirectory", "file"], 
                           data_files = ref[ref$data == "surv_data_file", "file"], 
                           fit_files = as.vector(ref[ref$data == "fit_file", "file"]),
                           fit_names = as.vector(ref[ref$data == "fit_name", "file"]),
                           fit_metric = ref[ref$data == "fit_metric", "file"],
                           use_envir = use_envir)
    }
    
    
    output_dir <- NULL
    if("output" %in% ref$data) output_dir <- ref[ref$data == "output", "file"]
    
    demographic_file <- NULL
    if("demographics" %in% ref$data)
      demographic_file <- ref[ref$data == "demographics", "full_file"]
    
    list(models = models,
         param_info = param_info,
         use_envir = use_envir,
         output_dir = output_dir,
         demographic_file = demographic_file)
  }

#' Execute models
#' @description Execute a full set of analyses, possibly
#'   including discrete sensitivity analysis, probabilistic
#'   sensitivity analysis, and analyses across demographics.
#'   
#' @param inputs a list containing the unevaluated models
#'   ($models),    parameters for  the base model and
#'   sensitivity analyses ($param_info), and possibly an
#'   environment containing additional information necessary
#'   to run the model.
#' @param base_dir directory in which \code{ref-file} will
#'   be found.
#' @return  an unevaluated models object
#' @param N.prob,cost,effect,init arguments to 
#'   \code{\link{run_models}}
#' @param base_model which of the models should be the base
#'   model
#' @param run_demo,run_psa Useful for limiting computation,
#'   for example, when running a model interactively in
#'   shiny.
#' @param ... additional arguments
#' @details
#' 
#' Relies on the inputs collated by 
#' \code{\link{gather_model_info}}. These pieces were broken
#' apart to facilitate putting together a shiny interface.
#' 
#' For more information see the vignette: 
#' \code{vignette("file-input", package = "heemod")}.`

#' @return a list \itemize{ \item \code{model}s (always) 
#'   \item \code{model_runs} (always) \item \code{dsa}
#'   (deterministic sensitivity analysis) - if appropriate
#'   parameters provided \item \code{psa} (probabilistic
#'   sensitivity analysis) - if appropriate parameters
#'   provided \item \code{demographics} results across
#'   different demographic groups }
#'   

execute_models <-
  function(inputs,
           base_dir,
           N.prob = 50,
           cost,
           effect,
           init = NULL,
           base_model = NULL,
           run_demo = TRUE,
           run_psa = TRUE,
           ...) {
    if (!is.null(inputs$use_envir)) {
      attach(inputs$use_envir, pos = 2L,
             name = "heemod_temp_variables_envir_detach_me")
      on.exit(detach(name = "heemod_temp_variables_envir_detach_me"))
    }
    
    if (is.null(init)) {
      message("Initial state counts not explicitely defined. Using default setting.")
      init <-
        c(1000L, rep(0L, get_state_number(get_states(inputs$models[[1]])) - 1))
    }
    
    model_runs <- run_models_(
      inputs$models,
      parameters = inputs$param_info$params,
      init = init,
      cost = cost, #lazyeval::lazy(cost),
      effect = effect, #lazyeval::lazy(effect),
      base_model = base_model,
      ...
    )
    model_dsa <- NULL
    if (!is.null(inputs$param_info$dsa_params))
      model_dsa <-
      run_sensitivity(model_runs, inputs$param_info$dsa_params)
    
    model_psa <- NULL
    if (!is.null(inputs$param_info$psa_params) & run_psa)
      model_psa <-
      run_probabilistic(model_runs, inputs$param_info$psa_params, N = N.prob)
    
    demo_res <- NULL
    heterogeneity_res <- NULL
    if (!is.null(inputs$demographic_file) & run_demo){
      demographic_table <-
        f_read_demographic_table(inputs$demographic_file,
                                 weight_col_name = "weight",
                                 params = inputs$param_info$params)
      
      
      demo_res <-
        run_demographics(model_runs,
                         demographics = demographic_table)
      heterogeneity_res <-
        run_heterogeneity(model_runs,
                          newdata = demographic_table)
    }
    
    
    
    list(
      models = inputs$models,
      model_runs = model_runs,
      dsa = model_dsa,
      psa = model_psa,
      demographics = demo_res,
      heterogeneity = heterogeneity_res
    )
  }

#' Read Models Specified by Files
#' 
#' @param ref Imported reference file.
#' @param base_model Which of the models should be the base 
#'   model.
#'   
#' @return  an unevaluated models object
import_models_from_files <- function(ref, base_model) {
  
  state_info <- parse_multi_spec(
    ref$full_file[ref$data == "state"],
    group_vars = "state"
  )
  
  tm_info <- parse_multi_spec(
    ref$full_file[ref$data == "tm"],
    group_vars = c("from", "to")
  )
  
  if(any(sort(names(state_info)) != sort(names(tm_info)))) {
    stop("Mismatch between state names and transition matrix names.")
  }
  
  tm_info <- tm_info[names(state_info)]
  
  models <- lapply(
    seq_along(state_info),
    function(i) {
      create_model_from_tabular(state_info[[i]], tm_info[[i]])
    })
  
  names(models) <- names(state_info)
  
  which_base <- which(names(models) == base_model)
  models <- models[c(which_base, setdiff(seq_len(models), which_base))]
  
  models
}
