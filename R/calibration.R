
#' Calibrate model parameters to match desired output
#' @param base_dir Directory where the files are located.
#' @param ref_file Name of the reference file.
#' @param run_model a run_model object
#' @param param_names names of the parameters to calibrate
#' @param matching_df List where each element specifies the function that
#'   will be used to calculate a value that is matched with the respective value
#'   in `matching.values`. See details.
#' @param target_values values we want the matching functions to match
#' @param method The optimization method from package `optimx` to use for the 
#'   calibration; defaults to "Nelder-Mead".
#' @param initial_values an optional matrix with each row providing a starting
#'   point for the optimization.  (If a data frame is passed in, it will be
#'   converted.) If not provided, the starting values from `run_model` will
#'   be used.
#' @param demographics As for `run_model_tabular`.
#' @param run_demo When running from a tabular model, should the model be run
#'   across demographic groups?
#'   
#' @param ... optional arguments to be passed to optimx; when running
#'   `calibrate_model_from_tabular`, will also include all arguments to
#'   `calibrate_model` other than run_model.
#' @export
#' 
#' @return A data frame in which each row has the calibrated values of
#'   parameters in `param_names` for the corresponding row of
#'   `initial_values`, along with the convergence code for each run.
#' @details `matching_df` is a data.frame with four columns:  group,
#' strategy_name, states_to_sum, and cycles_to_sum.  Each value of group defines
#' a set of counts that will be added together and compared to the corresponding
#' element of `target_values`.  (Target values must be given in the order
#' of sorted group values.)   For each value of group, strategy_name can have
#' only one value; having more than one will cause an error. For each value of
#' group, `cycles_to_sum` and `states_to_sum` will be used to index
#' the counts from the corresponding strategy, and the sum of those counts will
#' be returned as the value to match.  The calibration minimizes the sum of
#' squared differences between the calculated values and the target values.
#' 
#' Parameters not being optimized are unchanged from the values in the model run
#' passed to `calibrate_model`.  If `initial_values` is `NULL`,
#' initial values of the parameters being calibrated will also be taken from the
#' model run passed to `calibrate_model`.  Passing in initial values
#' allows (among other things) the user to check whether the calibration gets
#' the same results from different starting points.
#' 
#' Running calibrate_model does not change the model parameters - the user
#' must create a new model and run it if desired.
#' 
#' See also the vignette `k-calibration`.
#' @example inst/examples/example_calibration.R

calibrate_model <- function(run_model,
                              param_names,
                              matching_df,
                              target_values,
                              method = "Nelder-Mead",
                              initial_values = NULL,
                              demographics = NULL,
                              ...) {

  stopifnot(inherits(run_model, "run_model"))
  ## error checking for the matching function
  stopifnot(all(c("strategy_name", "cycles_to_sum", "states_to_sum", "group") %in%
                  names(matching_df)))  

  for(this_col in 1:ncol(matching_df))
    if(is.factor(matching_df[, this_col]))
      matching_df[, this_col] <- 
        levels(matching_df[,this_col])[matching_df[, this_col]]
    
  stopifnot(all(matching_df$states_to_sum %in% get_state_names(run_model)))
  stopifnot(all(matching_df$cycles_to_sum <= get_cycles(run_model)))
  stopifnot(all(matching_df$strategy_name %in% get_strategy_names(run_model)))
  
  ## if initial values were not supplied, pull them out of the model
  if (is.null(initial_values)) {
    init_param_values <-
      lapply(param_names,
             function(this_param) {
               try(lazy_eval(run_model$parameters[[this_param]]))
             })
    
    any_errors <- which(sapply(init_param_values,
                               function(x) {
                                 inherits(x, "try-error")
                               }))
    if (length(any_errors) !=  0) {
      stop(
        paste(
          "some parameters could not be evaluated, may not be numeric entries:",
          paste(param_names[any_errors], collapse = ", ")
        )
      )
    }
    
    initial_values <-
      structure(matrix(unlist(init_param_values),
                       nrow = 1),
                dimnames = list(NULL, param_names))
  }
  
  if (is.data.frame(initial_values))
    initial_values <- as.matrix(initial_values)
  stopifnot(identical(colnames(initial_values), param_names))
  model_params <- names(get_parameters(run_model))
  is_optimizable <- colnames(initial_values) %in% model_params
  if(!all(is_optimizable))
    stop(paste(paste0("parameter", plur(sum(!is_optimizable))),
              paste(colnames(initial_values)[!is_optimizable], collapse = ", "),
              "not in parameters of model")
         )
  
  
  ## now we've got the initial values squared away, and can finally optimize
  ##    optimize over the parameters
  
  if(length(param_names) > 1){
    if (!requireNamespace("optimx")) {
      stop("'optimx' package required for calibration.")
    }

  optim_start_each_row <-
    lapply(1:nrow(initial_values), function(i) {
      optim_output <- optimx::optimx(
        par = initial_values[i,],
        fn = fn_to_minimize,
        method = method,
        param_names = param_names,
        matching_fn_df = matching_df,
        model_runs_object = run_model,
        target_values = target_values,
        demographics = demographics,
        ...
      )
      optim_output[c(param_names, "value", "convcode")]
    })
  }
  else{
    ## optimx not recommended for 1-dimensional problems; using optimize
    optim_start_each_row <- 
      lapply(1:nrow(initial_values), function(i){
      optim_output <- optimize(
        f = fn_to_minimize,
        param_names = param_names,
        matching_fn_df = matching_df,
        model_runs_object = run_model,
        target_values = target_values,
        demographics = demographics,
        ...
      )
      optim_output
    })
  }
  res <- do.call("rbind", optim_start_each_row)
  
  if (any(res$convcode != 0))
    warning("not all optimizations converged")
  res
}

sum_updated_model_counts <- function(run_model.object,
                                   strategy_name,
                                   newdata,
                                   states_to_sum,
                                   cycles_to_sum,
                                   demographics) {
  # Update the run_model.model object with new parameters
  ##   suggested by the optimizer
  if(is.null(demographics)){
    updated_model <- eval_strategy_newdata(run_model.object,
                                                  strategy = strategy_name,
                                                  newdata)
  updated_counts <-
    as.matrix(get_counts(updated_model$.mod[[1]]))
  }
  else{
    
    mod_params <- modify_(get_parameters(run_model.object),
                          lazyeval::as.lazy_dots(as.list(newdata)))
    
    ## run the model with updated parameters
    updated_model <- run_model_(
      uneval_strategy_list = run_model.object$uneval_strategy_list,
      parameters = mod_params,
      init = get_init(run_model.object),
      cycles = get_cycles(run_model.object),
      method = get_method(run_model.object),
      cost = get_ce(run_model.object)$.cost,
      effect = get_ce(run_model.object)$.effect,
      state_cycle_limit = NULL,
      central_strategy = get_central_strategy(run_model.object),
      inflow = get_inflow(run_model.object)
    )
    
    ## and run the demographics
    updated_demo_model <- stats::update(updated_model, 
                                        demographics)
    updated_counts <- 
      get_counts(updated_demo_model) %>%
      dplyr::filter(., .strategy_names == strategy_name) %>%
      dplyr::select(., -dplyr::matches(".strategy_names")) %>%
      tidyr::spread_(., key_col = "state_names", value_col = "count")

    updated_counts <- as.matrix(updated_counts)
  }
  
  ## convert state names to corresponding column numbers and
  ##   combine with cycles to index matrix to get the desired counts
  ind <- cbind(cycles_to_sum,
               match(states_to_sum, colnames(updated_counts)))
  return(sum(updated_counts[ind]))
}

fn_to_minimize <- function(par,
                            param_names,
                            matching_fn_df,
                            model_runs_object,
                            target_values,
                           demographics = demographics,
                            ...) {

  # create a df with the 'new data' - this will be passed to eval_model_newdata:
  new.df <- as.data.frame(t(par))
  names(new.df) <- param_names
  unique_grps <- sort(unique(matching_fn_df$group))
  current_vals <- sapply(unique_grps, function(this_group) {
    this_count_set <- dplyr::filter(matching_fn_df, group == this_group)
    this_strategy_name <- unique(this_count_set$strategy_name)
    if(length(this_strategy_name) > 1)
      stop(paste("each group can have only one strategy_name; group",
                 this_group, "has", length(this_strategy_name)))
    sum_updated_model_counts(
      model_runs_object,
      this_strategy_name,
      newdata = new.df,
      states_to_sum = this_count_set$states_to_sum,
      cycles_to_sum = this_count_set$cycles_to_sum,
      demographics = demographics
    )
  })
  
  diffs.sq <- (current_vals - target_values) ^ 2
  return(sum(diffs.sq))
}

#' @rdname calibrate_model
#' @export
calibrate_model_from_tabular <-
  function(base_dir,
           ref_file,
           param_names,
           matching_df,
           target_values,
           method,
           run_demo,
           ...) {
    # Run initially... and get the run_model object
    initial.run <- run_model_tabular(
      location = base_dir,
      reference = ref_file,
      save = FALSE,
      overwrite = FALSE,
      run_psa = FALSE,
      run_demo = run_demo
    )
    use_demographics <- 
      ifelse(is.null(demographics), 
             NULL,
             data.frame(initial.run$demographics$newdata,
                        .weights = initial.run$demographics$weights))
    calibrate_model(initial.run$model_runs, 
                      demographics = use_demographics,
                      ...)
    
  }

convert_matching_function <- function(list_version) {
  pieces <-
    lapply(list_version, expand.grid, stringsAsFactors = FALSE)
  sizes <- sapply(pieces, nrow)
  res <- do.call("rbind", pieces)
  res$group <- rep(seq(along = pieces), sizes)
  res
}

