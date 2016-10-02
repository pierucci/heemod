#' Run Analyses From Files
#' 
#' This function runs a model from tabular input.
#' 
#' The reference file should have two columns, \code{data} 
#' and \code{file}. An optional \code{absolute_path} column 
#' can be added, having value \code{TRUE} where an absolute 
#' file path is provided. \code{data} values must include 
#' \code{state}, \code{tm}, and \code{parameters}, and can 
#' also include \code{options}, \code{demographics} and 
#' \code{data}.  The corresponding values in the \code{file}
#' column give the names of the files (located in 
#' \code{base_dir}) that contain the corresponding 
#' information - or, in the case of \code{data}, the 
#' directory containing the tables to be loaded.
#' 
#' @param location Directory where the files are located.
#' @param reference Name of the reference file.
#' @param run_psa Run PSA?.
#' @param run_demo Run demgraphic analysis?
#' @param save Should the outputs be saved?
#' @param overwrite Should the outputs be overwritten?
#'   
#' @return A list of evaluated models (always), and, if 
#'   appropriate input is provided, dsa (deterministic 
#'   sensitivity analysis), psa (probabilistic sensitivity 
#'   analysis) and demographics (results across different 
#'   demographic groups).
#'   
#' @export
run_models_tabular <- function(location, reference = "REFERENCE.csv",
                               run_psa = TRUE, run_demo = TRUE,
                               save = FALSE, overwrite = FALSE) {
  
  inputs <- gather_model_info(location, reference)
  outputs <- eval_models_from_tabular(inputs,
                                      run_psa = run_psa,
                                      run_demo = run_demo)
  
  output_dir <- inputs$output_dir
  
  if (save) {
    save_outputs(outputs, output_dir, overwrite)
  }
  
  outputs
}

#' Gather Information for Running a Model From Tabular Data
#' 
#' @param base_dir Directory where the files are located.
#' @param ref_file Name of the reference file.
#'   
#' @return A list with elements: \itemize{ \item models (of 
#'   type \code{uneval_model}, created by 
#'   \code{\link{create_model_list_from_tabular}}) \item 
#'   param_info  \item output_dir where to store output 
#'   files, if specified \item demographic_file a table for 
#'   demographic analysis \item model_options a list of 
#'   model options.}
#'   
#' @keywords internal
gather_model_info <- function(base_dir, ref_file) {
  
  if (options()$heemod.verbose) message("* Reading files...")
  if (options()$heemod.verbose) message("** Reading reference file...")
  ref <- read_file(file.path(base_dir, ref_file))
  
  if (any(pb <- duplicated(ref$data))) {
    stop(sprintf(
      "Duplicated values in reference file 'data' column: %s.",
      paste(ref$data[pb], collapse = ", ")
    ))
  }
  
  if (is.null(ref$absolute_path)) {
    ref$full_file <- file.path(base_dir, ref$file)
  } else {
    if (options()$heemod.verbose) message(sprintf(
      "** Using absolute path for %s.",
      paste(ref$data[ref$absolute_path & ! is.na(ref$absolute_path)],
            collapse = ", ")
    ))
    
    ref$full_file <- ifelse(
      ref$absolute_path & ! is.na(ref$absolute_path),
      ref$file,
      file.path(base_dir, ref$file)
    )
  }
  
  df_env <- new.env()
  
  if (options()$heemod.verbose) message("** Reading model list...")
  models <- create_model_list_from_tabular(
    ref = ref,
    df_env = df_env
  )
  
  model_options <- NULL
  if ("options" %in% ref$data) {
    if (options()$heemod.verbose) message("** Reading options...")
    model_options <- create_options_from_tabular(
      read_file(ref$full_file[ref$data == "options"])
    )
  }
  
  if ("data" %in% ref$data) {
    if (options()$heemod.verbose) message("** Reading external data...")
    create_df_from_tabular(
      ref$full_file[ref$data == "data"],
      df_env
    )
  }
  
  if (options()$heemod.verbose) message("** Reading parameters..")
  param_info <- create_parameters_from_tabular(
    read_file(ref$full_file[ref$data == "parameters"]),
    df_env
  )
  
  output_dir <- NULL
  if("output" %in% ref$data) {
    if (options()$heemod.verbose) message("** Reading path to output directory...")
    output_dir <- ref$full_file[ref$data == "output"]
  }
  
  demographic_file <- NULL
  if("demographics" %in% ref$data) {
    if (options()$heemod.verbose) message("** Reading demographic data...")
    demographic_file <- create_demographic_table(
      read_file(ref$full_file[ref$data == "demographics"]),
      params = param_info$params
    )
  }
  
  list(
    models = models,
    param_info = param_info,
    output_dir = output_dir,
    demographic_file = demographic_file,
    model_options = model_options
  )
}

#' Evaluate Models From a Tabular Source
#' 
#' Execute a full set of analyses, possibly including 
#' discrete sensitivity analysis, probabilistic sensitivity 
#' analysis, and analyses across demographics.
#' 
#' @param inputs Result from 
#'   \code{\link{gather_model_info}}.
#' @param run_psa Run PSA?
#' @param run_demo Run demographic analysis?
#'   
#' @return a list \itemize{ \item \code{models} (always) 
#'   unevaluated model. \item \code{model_runs} (always) 
#'   evaluated models \item \code{dsa} (deterministic 
#'   sensitivity analysis) - if appropriate parameters 
#'   provided \item \code{psa} (probabilistic sensitivity 
#'   analysis) - if appropriate parameters provided \item 
#'   \code{demographics} results across different 
#'   demographic groups - if appropriate parameters 
#'   provided}
#'   
#' @keywords internal
eval_models_from_tabular <- function(inputs,
                                     run_psa = TRUE,
                                     run_demo = TRUE) {
  
  if (options()$heemod.verbose) message("* Running files...")
  list_args <- c(
    inputs$models,
    list(
      parameters = inputs$param_info$params,
      init = inputs$model_options$init,
      cost = inputs$model_options$cost,
      effect = inputs$model_options$effect,
      base_model = inputs$model_options$base_model,
      method = inputs$model_options$method,
      cycles = inputs$model_options$cycles
    )
  )
  
  list_args <- Filter(
    function(x) ! is.null(x),
    list_args
  )
  
  if (options()$heemod.verbose) message("** Running models...")
  model_runs <- do.call(
    run_models,
    list_args
  )
  
  model_dsa <- NULL
  if (! is.null(inputs$param_info$dsa)) {
    if (options()$heemod.verbose) message("** Running DSA...")
    model_dsa <- run_sensitivity(
      model_runs,
      inputs$param_info$dsa_params
    )
  }
  
  model_psa <- NULL
  if (! is.null(inputs$param_info$psa_params) & run_psa) {
    if (options()$heemod.verbose) message("** Running PSA...")
    model_psa <- run_probabilistic(
      model_runs,
      resample = inputs$param_info$psa_params,
      N = inputs$model_options$n
    )
  }
  
  demo_res <- NULL
  if (! is.null(inputs$demographic_file) & run_demo) {
    if (options()$heemod.verbose) message("** Running demographic analysis...")
    demo_res <- stats::update(model_runs, inputs$demographic_file)
  }
  
  list(
    models = inputs$models,
    model_runs = model_runs,
    dsa = model_dsa,
    psa = model_psa,
    demographics = demo_res
  )
}

#' Read Models Specified by Files
#' 
#' @param ref Imported reference file.
#' @param df_env An environment containing external data.
#'   
#' @return A list of unevaluated models.
#'   
#' @keywords internal
create_model_list_from_tabular <- function(ref, df_env = globalenv()) {
  if(! inherits(ref, "data.frame")) stop("'ref' must be a data frame.")
  
  if (options()$heemod.verbose) message("*** Reading states...")
  state_info <- parse_multi_spec(
    read_file(ref$full_file[ref$data == "state"]),
    group_vars = ".state"
  )
  
  if (options()$heemod.verbose) message("*** Reading TM...")
  tm_info <- parse_multi_spec(
    read_file(ref$full_file[ref$data == "tm"]),
    group_vars = c("from", "to")
  )
  
  if (length(pb <- setdiff(names(state_info), names(tm_info)))) {
    stop(sprintf(
      "Mismatching model names between TM file and state file: %s.",
      paste(pb, collapse = ", ")
    ))
  }
  
  tm_info <- tm_info[names(state_info)]
  
  if (options()$heemod.verbose) message("*** Defining models...")
  models <- lapply(
    seq_along(state_info),
    function(i) {
      create_model_from_tabular(state_info[[i]], tm_info[[i]],
                                df_env = df_env)
    })
  
  names(models) <- names(state_info)
  
  models
}

#' Create State Definitions From Tabular Input
#' 
#' Transforms tabular input defining states into an 
#' \code{heemod} object.
#' 
#' Columns of state_info besides .model and state include 
#' costs and utilities we want to keep track of, with 
#' appropriate values (these may include parameters). For 
#' any cost or utility that should be discounted, an 
#' additional column with the name ".discount.<cost>" or 
#' ".discount.<effect>", for the appropriate cost or effect,
#' can be included. If no discounting is desired for a 
#' particular cost or effect, the corresponding column can 
#' be omitted.
#' 
#' A discount column can contain only a single value - a 
#' cost or benefit must be discounted by the same amount in 
#' each state. Discounts can be numbers or parameters (which
#' will then need to be defined like any other).
#' 
#' The input data frame is expected to contain state 
#' information for all the models you will use in an 
#' analysis. For more information see the vignette: 
#' \code{vignette("file-input", package = "heemod")}.
#' 
#' @param state_info Result for one model of 
#'   \code{\link{parse_multi_spec}}.
#' @param df_env An environment containing external data.
#'   
#' @return A state list.
#'   
#' @keywords internal
create_states_from_tabular <- function(state_info,
                                       df_env = globalenv()) {
  
  if(! inherits(state_info, "data.frame"))
    stop("'state_info' must be a data frame.")
  
  if(!(".state" %in% names(state_info)))
    stop("'.state' should be a column name.")
  
  if (any(duplicated(state_info$.state))) {
    stop(sprintf(
      "Duplicated state names: %s.",
      paste(unique(state_info$.state[duplicated(state_info$.state)]),
            sep = ", ")
    ))
  }
  
  state_names <- state_info$.state
  values <- setdiff(names(state_info), c(".model", ".state"))
  discounts <- values[grep("^\\.discount", values)]
  values <- setdiff(values, discounts)
  discounts_clean <- gsub("^\\.discount\\.(.+)", "\\1", discounts)
  
  if (! all(discounts_clean %in% values)) {
    stop(sprintf(
      "Discounting rates defined for non-existing values: %s.",
      paste(discounts[! discounts %in% values], collapse = ", ")
    ))
  }
  
  for (n in discounts) {
    if (all(is.na(state_info[[n]]))) {
      stop(sprintf(
        "No discount values found for '%s'.", n
      ))
      
    } else if (length(unique(stats::na.omit(state_info[[n]]))) > 1) {
      stop(sprintf(
        "Multiple discount values for '%s'.", n
      ))
      
    } else {
      state_info[[n]] <- stats::na.omit(state_info[[n]])[1]
    }
  }
  
  for (n in discounts_clean) {
    state_info[[n]] <- sprintf(
      "discount(%s, %s)",
      state_info[[n]],
      state_info[[paste0(".discount.", n)]]
    )
  }
  
  res <- define_state_list_(
    setNames(lapply(
      state_info$.state,
      function(state) {
        define_state_(
          lazyeval::as.lazy_dots(
            setNames(lapply(
              values,
              function(value) {
                state_info[[value]][state_info$.state == state]
              }
            ), values),
            env = df_env
          )
        )
      }
    ), state_info$.state)
  )
  if (options()$heemod.verbose) print(res)
  res
}

#' Create a Transition Matrix From Tabular Input
#' 
#' Transforms tabular input defining a transition matrix 
#' into an \code{heemod} object.
#' 
#' The data frame \code{trans_probs} should have columns 
#' \code{from}, \code{to}, and \code{prob}, where 
#' \code{prob} is the probability of a transition from the 
#' \code{from} state to the \code{to} state. Prob can be 
#' defined in terms of parameters, just as when using 
#' \code{define_matrix} at the keyboard. Probabilities of 0 
#' need not be specified - they will be automatically 
#' inserted.
#' 
#' All state names must be used in the \code{from} column of
#' the transition matrix (otherwise you can just get rid of 
#' the state). Absorbing states should have a transition 
#' from and to themselves with probability 1.
#' 
#' @param trans_probs  Result for one model of 
#'   \code{\link{parse_multi_spec}}.
#' @param state_names The names of the states used in the 
#'   transition matrix.
#' @param df_env An environment containing external data.
#'   
#' @return A transition matrix.
#'   
#' @keywords internal
create_matrix_from_tabular <- function(trans_probs, state_names,
                                       df_env = globalenv()) {
  if(! inherits(trans_probs, "data.frame"))
    stop("'trans_probs' must be a data frame.")
  
  stopifnot(
    all(c("from", "to", "prob") %in% names(trans_probs)),
    length(state_names) > 0
  )
  
  unique_states <- unique(c(trans_probs$from, trans_probs$to))
  
  ## we can have an initial state where people start but 
  ## can't get to, so we don't check trans_probs$to states 
  ## the way we check trans_probs$from states
  
  if (! all(trans_probs$to %in% trans_probs$from)) {
    stop(sprintf(
      "Some states do not have an exit probability: %s.",
      paste(
        unique(trans_probs$to)[! unique(trans_probs$to) %in% trans_probs$from],
        sep = ", "
      )
    ))
  }
  
  if(! all(unique_states %in% state_names)) {
    stop(sprintf(
      "Some states specified in the transition matrix differ from 'state_names': %s.",
      unique_states[! unique_states %in% state_names]
    ))
  }
  
  ## set up matrix of 0's, and then add the listed
  ## probabilities
  num_states <- length(unique_states)
  prob_mat <- matrix(0, nrow = num_states, ncol = num_states,
                     dimnames = list(state_names, state_names))
  prob_mat[as.matrix(trans_probs[, c("to", "from")])] <- trans_probs$prob
  
  res <- define_matrix_(
    lazyeval::as.lazy_dots(prob_mat, env = df_env),
    state_names = state_names
  )
  if (options()$heemod.verbose) print(res)
  res
}

#' Create a Parameter Definition From Tabular Input
#' 
#' If specified in the tabular file, DSA and PSA can also be
#' created.
#' 
#' The tabular parameter definition file can have the 
#' following columns: \code{parameter} (the parameter name, 
#' required), \code{value} (required), \code{low} and 
#' \code{high} (if both are present, a deterministic 
#' sensitivity analysis will be performed), and \code{psa} 
#' (a definition of a distribution to use in a probabilistic
#' sensitivity analysis. Other columns will be ignored.
#' 
#' @param param_defs A parameter definition file.
#' @param df_env An environment containing external data.
#' 
#' @return The parameter definition.
#'   
#' @keywords internal
create_parameters_from_tabular <- function(param_defs,
                                           df_env = globalenv()) {
  if(! inherits(param_defs, "data.frame"))
    stop("'param_defs' must be a data frame.")
  
  if (xor("low" %in% names(param_defs),
          "high" %in% names(param_defs))) {
    stop("Both 'low' and 'high' columns must be present in parameter file to define DSA.")
  }
  
  parameters <- define_parameters_(
    lazyeval::as.lazy_dots(
      setNames(
        lapply(param_defs$value, function(x) x),
        param_defs$parameter
      ),
      env = df_env
    )
  )
  
  dsa <- psa <- NULL
  
  if ("low" %in% names(param_defs) &&
      "high" %in% names(param_defs)) {
    
    if (! all(is.na(param_defs$low) ==
              is.na(param_defs$high))) {
      stop("'low' and 'high' must be both non missing in DSA tabular definition.")
    }
    
    if (all(is.na(param_defs$low))) {
      stop("Non non-missing values in columns 'low' and 'high'.")
    }
    
    param_sens <- param_defs$parameter[! is.na(param_defs$low)]
    low <- stats::na.omit(param_defs$low)
    high <- stats::na.omit(param_defs$high)
    
    dsa <- define_sensitivity_(
      par_names = param_sens,
      low_dots = lazyeval::as.lazy_dots(
        setNames(
          lapply(low, function(x) x),
          param_sens
        ),
        env = df_env
      ),
      high_dots = lazyeval::as.lazy_dots(
        setNames(
          lapply(high, function(x) x),
          param_sens
        ),
        env = df_env
      )
    )
  }
  
  if ("psa" %in% names(param_defs)) {
    
    if (all(is.na(param_defs$psa))) {
      stop("Non non-missing values in column 'psa'.")
    }
    
    param_psa <- param_defs$parameter[! is.na(param_defs$psa)]
    distrib_psa <- stats::na.omit(param_defs$psa)
    
    psa <- do.call(
      define_distrib,
      lapply(
        seq_along(param_psa),
        function(i) {
          substitute(
            rhs ~ lhs,
            list(
              rhs = as.name(param_psa[i]),
              lhs = parse(text = distrib_psa[i])[[1]]))
        }
      )
    )
    
  }
  
  list(
    params = parameters,
    dsa_params = dsa,
    psa_params = psa
  )
}


#' Create Model Options From a Tabular Input
#'
#' @param opt An option data frame.
#'
#' @return A list of model options.
#'   
#' @keywords internal
create_options_from_tabular <- function(opt) {
  
  allowed_opt <- c("cost", "effect", "init",
                   "method", "base", "cycles", "n")
  if(! inherits(opt, "data.frame"))
    stop("'opt' must be a data frame.")
  
  if (any(ukn_opt <- ! opt$option %in% allowed_opt)) {
    stop(sprintf(
      "Unkmown options: %s.",
      paste(opt$option[ukn_opt], collapse = ", ")
    ))
  }
  
  if (any(duplicated(opt$option))) {
    stop("Some option names are duplicated.")
  }
  
  res <- list()
  for (n in opt$option) {
    res <- c(res, list(opt$value[opt$option == n]))
  }
  names(res) <- opt$option
  
  if (! is.null(res$init)) {
    res$init <- as_numeric_safe(
      strsplit(res$init, ",")[[1]]
    )
  }
  
  if (! is.null(res$cycles)) {
    res$cycles <- as_integer_safe(res$cycles)
  }
  
  if (! is.null(res$n)) {
    res$n <- as_integer_safe(res$n)
  }
  
  if (! is.null(res$cost)) {
    res$cost <- parse(text = res$cost)[[1]]
  }
  
  if (! is.null(res$effect)) {
    res$effect <- parse(text = res$effect)[[1]]
  }
  if (options()$heemod.verbose) message(paste(
    names(res), unlist(res), sep = " = ", collapse = "\n"
  ))
  res
}

#' Create a \code{heemod} Model From Tabular Files Info
#' 
#' Calls \code{\link{create_states_from_tabular}} and
#' \code{\link{create_matrix_from_tabular}}.
#' 
#' @param state_file A state tabular file (file path or 
#'   parsed file).
#' @param tm_file A transition matrix tabular file (file 
#'   path or parsed file).
#' @param df_env An environment containing external data.
#' 
#' @return A \code{heemod} model as returned by 
#'   \code{\link{define_model}}.
#'   
#' @keywords internal
create_model_from_tabular <- function(state_file,
                                      tm_file,
                                      df_env = globalenv()) {
  if(! inherits(state_file, "data.frame"))
    stop("'state_file' must be a data frame.")
  if(! inherits(tm_file, "data.frame"))
    stop("'tm_file' must be a data frame.")
  
  if (options()$heemod.verbose) message("**** Defining state list...")
  states <- create_states_from_tabular(state_file,
                                       df_env = df_env)
  if (options()$heemod.verbose) message("**** Defining TM...")
  
  TM <- create_matrix_from_tabular(tm_file, get_state_names(states),
                                   df_env = df_env)
  
  define_model_(transition_matrix = TM, states = states)
}

#' Load Data From a Folder Into an Environment
#' 
#' Reads files containing data frames (in tabular format) 
#' from a directory, and loads them in an environment to be 
#' available during an analysis.
#' 
#' The files must be in .csv, .xls, or .xlsx format. A file 
#' my_df.csv (or my_df.xls, or my_df.xlsx) will be loaded as
#' a data frame my_df.
#' 
#' @param df_dir A directory containing the files.
#' @param df_envir An environment.
#' 
#' @return The environment with the data frames.
#'   
#' @keywords internal
create_df_from_tabular <- function(df_dir, df_envir) {
  if(! file.exists(df_dir))
    stop(paste(df_dir, "does not exist."))
  
  all_files <- list.files(df_dir, full.names = TRUE)
  obj_names <- all_files %>% 
    basename %>% 
    tools::file_path_sans_ext()
  
  if(any(duplicate_names <- duplicated(obj_names)))
    stop(paste("Duplicate data names:",
               paste(obj_names[duplicate_names], collapse = ", ")))
  
  ## do the assignments
  for(i in seq(along = all_files)){
    this_val <- read_file(all_files[i])
    assign(obj_names[i], this_val, envir = df_envir)
  }
  df_envir
}

#'Specify Inputs for Multiple Models From a Single File
#'
#'Parse a \code{data.frame} containing specifications for 
#'multiple models into a list of inputs required for each 
#'model.
#'
#'Each combination of values of the columns specified by 
#'\code{group_vars} should either be unique in the file (in 
#'which case it will be replicated for all values of 
#'\code{split_on}), or must be repeated as many times as 
#'unique values of \code{split_on}.
#'
#'\code{split_on} is usually the model name.
#'
#'\code{group_var} can be the state names, or from and to 
#'lines for a matrix definition...
#'
#'@param multi_spec \code{data frame}.
#'@param split_on \code{character} of length 1, with the 
#'  name of the variable in \code{multi_spec} to be split 
#'  on.
#'@param group_vars \code{character}, one or more variable 
#'  names from \code{multi_spec} that identify a line of 
#'  information.
#'  
#'@return A list of data frames, one for each value of 
#'  \code{split_on.}
#'   
#' @keywords internal
parse_multi_spec <- function(multi_spec,
                             split_on = ".model",
                             group_vars) {
  
  if(! inherits(multi_spec, "data.frame"))
    stop("'multi_spec' must be a data frame.")
  
  if(length(split_on) != 1) stop("'split_on' must have a length of exactly 1.")
  if(any(names(multi_spec) == "")) stop("'multi_spec' can't have empty names.")
  
  if(! all(c(split_on, group_vars) %in% names(multi_spec)))
    stop("'split_on' and 'group_vars' must be column names of the input 'multi_spec'.")
  
  remaining <- setdiff(names(multi_spec), c(split_on, group_vars))
  
  ## any line that exists for one split but not others
  ##   needs to be duplicated for all splits
  ## throw an error if a parameter exists for more than one,
  ##   but not all
  unique_splits <- unique(multi_spec[, split_on])
  num_splits <- length(unique_splits)
  
  occurences <- multi_spec %>% 
    dplyr::group_by_(.dots = group_vars) %>% 
    dplyr::summarize_(count = ~ n())
  
  orig_order <- unique(multi_spec[, group_vars, drop = FALSE])
  
  wrong_number_times <- 
    occurences$count > 1 & occurences$count < num_splits
  
  if(any(wrong_number_times)) {
    stop("'group_var' combinations must be specified either once for all splits or once for each split.")
  }
  
  just_once <- multi_spec %>% 
    dplyr::group_by_(.dots = group_vars) %>% 
    dplyr::filter_(~ n() == 1) %>%
    dplyr::select_(~ - dplyr::one_of(split_on))
  
  just_once <- data.frame(
    temp = rep(unique_splits, nrow(just_once)),
    just_once[rep(seq_len(nrow(just_once)), each = num_splits), ],
    stringsAsFactors = FALSE
  )
  
  names(just_once)[1] <- split_on
  
  more_than_once <- multi_spec %>% 
    dplyr::group_by_(.dots = group_vars) %>%
    dplyr::filter_(~ n() > 1)
  
  multi_spec <- rbind(just_once, as.data.frame(more_than_once))
  
  rownames(multi_spec) <- NULL
  list_spec <- split(multi_spec, multi_spec[, split_on])
  ## sort by order of appearance of split variables in multi_spec
  ##   (making first model mentioned the base model, similar to earlier system)
  
  orig_split_order <- unique(multi_spec[, split_on])
  list_spec <- list_spec[orig_split_order]
  ## want to make sure everything comes out sorted
  ##   by order of appearance of values of group_vars
  lapply(list_spec, function(x) {
    match_to_orig <-
      sapply(group_vars, function(this_var){
        match(x[, this_var], orig_order[, this_var])})
    x[do.call("order", as.data.frame(match_to_orig)),]
  })
}

#' Read a Demographic Table
#' 
#' This function mostly checks whether the parameters are
#' correct.
#' 
#' An optional \code{.weights} column can exist in the file.
#' 
#' @param newdata A data frame.
#' @param params Parameters of a model, to check that all
#'   the columns in the demographic table (other than the
#'   weight column) are in the model.
#' @param ... catches other, unwanted arguments.
#'   
#' @return  A data frame.
#'   
#' @keywords internal
create_demographic_table <- function(newdata,
                                     params) {
  weight_col <- which(names(newdata) == ".weights")
  if (length(weight_col)) {
    var_names <- names(newdata)[- weight_col]
  } else {
    var_names <- names(newdata)
  }
  valid_names <- var_names %in% names(params)
  
  if(! all(valid_names)) {
    invalid_names <- paste(var_names[!valid_names], collapse = ", ")
    stop(sprintf(
      "The following columns in the demographic table are not parameters of the model: %s.",
      invalid_names
    ))
  }
  
  newdata
}

#' Read the accepted file formats for tabular input
#'
#' Columns starting with '.comment' are ignored.
#' 
#' @param file_name File name.
#' 
#' @return A \code{data.frame}.
#'   
#' @keywords internal
read_file <- function(file_name) {
  
  have_xls <- is_xls(file_name)
  have_xlsx <- is_xlsx(file_name)
  have_csv <- is_csv(file_name)
  
  if(! have_csv & ! have_xls & ! have_xlsx) {
    stop("file names must be for csv, xls, or xlsx")
  }
  
  if(have_csv) {
    tab <- utils::read.csv(
      file_name, header = TRUE,
      stringsAsFactors = FALSE,
      strip.white = TRUE,
      na.strings = c(".", "NA", "")
    ) 
  } else if(have_xls | have_xlsx) {
    if (! requireNamespace("readxl", quietly = TRUE)) {
      stop("readxl packaged needed to read Excel files")
      
    } else {
      tab <- as.data.frame(
        readxl::read_excel(file_name)
      )
    }
  }
  
  ## get rid of "comment" columns, if any
  tab <- tab[! grepl("^\\.comment", names(tab))]
  
  ## get rid of NA rows that may have come from blank rows
  ## in file
  tab <- filter_blanks(tab)
  tab
}

#' Remove Blank Rows From Table
#' 
#' Remove rows were all values are \code{NA}.
#' 
#' Some rows can be left blanks in the input table for 
#' readability, this function ensures those rows are 
#' removed.
#' 
#' @param x A \code{data.frame}.
#'   
#' @return A \code{data.frame} without blank rows.
#'   
#' @keywords internal
filter_blanks <- function(x) {
  x[! apply(is.na(x), 1, all), , drop = FALSE]
}

#' Check File Type
#' 
#' @param x A file name.
#' @return Whether the file is (respectively)
#'  csv, xlsx, or xls.
#' @rdname file-checkers
#'   
#' @keywords internal
is_csv <- function(x) {
  tolower(tools::file_ext(x)) == "csv"
}

#' @rdname file-checkers
is_xlsx <- function(x) {
  tolower(tools::file_ext(x)) == "xlsx"
}

#' @rdname file-checkers
is_xls <- function(x) {
  tolower(tools::file_ext(x)) == "xls"
}

#' Save Model Outputs
#' 
#' @param outputs Result from
#'   \code{\link{run_models_tabular}}.
#' @param output_dir Subdirectory in which to write output.
#' @param overwrite Should the outputs be overwritten?
#'   
#' @return \code{NULL}. Used for its side effect of creating
#'   files in the output directory.
#'   
#' @keywords internal
save_outputs <- function(outputs, output_dir, overwrite) {
  if (options()$heemod.verbose) message("* Saving outputs...")
  if(is.null(output_dir)) {
    warning("Output directory not defined in the specification file - the outputs will not be saved.")
    return(NULL)
    
  } else if(dir.exists(output_dir) & ! overwrite) {
    warning("Output directory exists and overwrite is FALSE - the outputs will not be saved.")
    return(NULL)
    
  } else {
    delete_succes <- 0
    if(dir.exists(output_dir)) {
      delete_succes <- unlink(output_dir, recursive = TRUE)
    }
    
    if (delete_succes == 1) {
      warning("Failed to delete output directory.")
      return(NULL)
    }
    
    dir.create(output_dir)
  }
  
  ## plots about individual models
  model_names <- names(outputs$models)
  
  if (options()$heemod.verbose) message("** Generating plots for individual models...")
  for(this_model in model_names){
    this_plot <- plot(outputs$model_runs, model = this_model)
    this_file <- paste("state_count_plot", this_model, sep = "_")
    save_graph(this_plot, output_dir, this_file)
    
    this_plot <- plot(outputs$dsa, model = this_model)
    this_file <- paste("dsa", this_model, sep = "_")
    save_graph(this_plot, output_dir, this_file)
  }
  
  base_model <- get_base_model(outputs$model_runs)
  
  ## plots about differences between models
  if (options()$heemod.verbose) message("** Generating plots with model differences...")
  for(this_model in setdiff(model_names, base_model)){
    this_plot <- plot(outputs$dsa, type = "diff", model = this_model)
    this_file <- paste("dsa", this_model, "vs", base_model, sep = "_")
    save_graph(this_plot, output_dir, this_file)
    
    this_plot <- plot(outputs$psa, model = this_model)
    this_file <- paste("psa", this_model, "vs", base_model, sep = "_")
    save_graph(this_plot, output_dir, this_file)
  }
  
  ## acceptability curve
  if (options()$heemod.verbose) message("** Generating acceptability curve...")
  this_plot <- plot(outputs$psa, type = "ac")
  save_graph(this_plot,
             output_dir, "acceptability")
  
  if (options()$heemod.verbose) message("** Writing ICER by group to a file...")
  utils::write.csv(
    outputs$demographics,
    file = file.path(output_dir, "icer_by_group.csv"),
    row.names = FALSE
  )
  invisible(NULL)
}

save_graph <- function(plot, path, file_name) {
  full_file <- file.path(path, file_name)
  grDevices::png(filename = paste(full_file, "png", sep = "."))
  print(plot)
  grDevices::dev.off()
  
  grDevices::cairo_pdf(
    filename = paste(full_file, "pdf", sep = ".")
  )
  print(plot)
  grDevices::dev.off()
}
