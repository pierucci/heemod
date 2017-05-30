#' Calibrate Model Parameters
#' 
#' Search for the appropriate value of unknown parameters to
#' obtain specific model results.
#' 
#' Parameters not being optimized are unchanged from the 
#' values in the model run. If `initial_values` is `NULL`, 
#' the initial parameter values will also be taken from the 
#' model run.
#' 
#' `initial_values` can be a vector or a table. In the 
#' second case each row corresponds to a set of initial 
#' parameter values: the calibration will be run once per 
#' set.
#' 
#' Passing in multiple initial values allows (among other 
#' things) the user to check whether the calibration gets 
#' the same results from different starting points.
#' 
#' Multi-dimensional problems are optimized with 
#' [optimx::optimx()], 1-dimensional problems with 
#' [stats::optimise()] (except when a `method` is given).
#' `convcode` is always `NA` with [stats::optimise()].
#' 
#' Running [calibrate_model()] does not change the model 
#' parameters; the user must create a new model and run it 
#' if desired.
#' 
#' See also `vignette("k-calibration")`.
#' 
#' @param x Result from [run_model()] or [update()].
#' @param parameter_names Names of the parameters to 
#'   calibrate.
#' @param fn_values Function applied to the model that 
#'   returns the values of interest as a numeric vector.
#' @param target_values Values to match, same length as the 
#'   output from `fn_values`.
#' @param initial_values Optional starting values. See 
#'   details.
#' @param method Optimisation method (`Nelder-Mead`,
#'   `BFGS`, or `L-BFGS-B`).
#' @param ... Optional arguments passed to 
#'   [optimx::optimx()].
#'   
#' @return A data frame in which each row has the calibrated
#'   values of parameters given in `parameter_names`, for
#'   the corresponding row of `initial_values`, along with
#'   the convergence code for each run.
#'   
#' @export
#' @example inst/examples/example_calibration.R
calibrate_model <- function(x, parameter_names,
                            fn_values, target_values,
                            initial_values = NULL,
                            method = c("Nelder-Mead", "BFGS",
                                       "L-BFGS-B"),
                            ...) {
  method <- match.arg(method)
  
  # if initial values were not supplied,
  # extract them from the model
  if (is.null(initial_values)) {
    init_param_values <- get_parameter_values(x, parameter_names)
    
    initial_values <- matrix(
      unlist(init_param_values),
      nrow = 1,
      dimnames = list(NULL, parameter_names)
    )
  }
  
  if (is.data.frame(initial_values)) {
    initial_values <- as.matrix(initial_values)
  }
  
  if (! is.numeric(initial_values)) {
    stop("Some of the initial parameter values are not numeric.")
  }
  
  if (is.null(dim(initial_values))) {
    if (is.null(names(initial_values))) {
      names(initial_values) <- parameter_names
    }
    initial_values <- t(as.matrix(initial_values))
  }
  
  if (is.null(colnames(initial_values))) {
    colnames(initial_values) <- parameter_names
  }
  
  stopifnot(
    is.matrix(initial_values),
    identical(colnames(initial_values), parameter_names)
  )
  
  model_parameters <- get_parameter_names(x)
  is_optimizable <- colnames(initial_values) %in% model_parameters
  
  if(! all(is_optimizable)) {
    stop(sprintf(
      "Parameter%s %s not present in model parameters.",
      plur(sum(!is_optimizable)),
      paste(colnames(initial_values)[!is_optimizable], collapse = ", ")
    ))
  }
  
  if (length(parameter_names) > 1 ||
      "method" %in% names(list(...))) {
    if (! requireNamespace("optimx")) {
      stop("'optimx' package required for calibration over more than 1 parameter.")
    }
    
    reserved_names <- c("value", "fevals", "gevals",
                        "niter", "convcode",
                        "kkt1", "kkt2", "xtimes")
    
    if (any(pb <- reserved_names %in% parameter_names)) {
      stop(sprintf(
        "Parameter name%s not allowed for calibration over more than 1 parameter: %s.",
        plur(sum(pb)),
        paste(reserved_names[pb], collapse = ", ")
      ))
    }
    
    rowwise_optim <- lapply(
      seq_len(nrow(initial_values)),
      function(i) {
        if (nrow(initial_values) > 1) {
          message(sprintf(
            "Running calibration with initial values #%i.", i
          ))
        }
        
        optim_output <- optimx::optimx(
          par = initial_values[i, ],
          fn = fn_calibrate_wrapper,
          method = method,
          
          model = x,
          parameter_names = parameter_names,
          target_values = target_values,
          fn_values = fn_values,
          ...
        )
        
        if (optim_output$convcode != 0 &&
            ! is.null(m <- unlist(attr(optim_output, "details")[, "message"]))) {
          message(sprintf("Message: %s.", m))
        }
        
        optim_output[c(parameter_names, "value", "convcode")]
      })
    
  } else {
    # optimx not recommended for 1-dimensional problems
    # using optim
    
    rowwise_optim <- lapply(
      seq_len(nrow(initial_values)),
      function(i) {
        if (nrow(initial_values) > 1) {
          message(sprintf(
            "Running calibration with initial values #%i.", i
          ))
        }
        
        optim_output <- stats::optimise(
          f = fn_calibrate_wrapper,
          
          model = x,
          parameter_names = parameter_names,
          target_values = target_values,
          fn_values = fn_values,
          ...
        )
        
        res <- data.frame(
          par = optim_output$minimum,
          value = optim_output$objective,
          convcode = NA
        )
        names(res)[1] <- parameter_names
        res
      })
  }
  
  res <- do.call("rbind", rowwise_optim)
  rownames(res) <- NULL
  
  if (any(res$convcode != 0, na.rm = TRUE)) {
    warning("Not all optimizations converged.")
  }
  
  res
}

fn_calibrate_wrapper <- function(par, model, ...) {
  fn_calibrate(x = model, par = par, ...)
}

fn_calibrate <- function(x, ...) {
  UseMethod("fn_calibrate")
}

fn_calibrate.run_model <- function(x, parameter_names,
                                   target_values, par, fn_values) {
  names(par) <- parameter_names
  parameters <- modify_(
    get_parameters(x),
    .dots = to_dots(par)
  )
  
  new_model <- run_model_(
    parameters = parameters,
    uneval_strategy_list = get_uneval_strategy_list(x),
    init = get_uneval_init(x),
    cycles = get_cycles(x),
    method = get_method(x),
    cost = get_ce_cost(x),
    effect = get_ce_effect(x),
    state_time_limit = get_state_time_limit(x),
    central_strategy = get_central_strategy(x),
    inflow = get_inflow(x)
  )
  
  values <- fn_values(new_model)
  
  sum((values - target_values) ^ 2)
}

fn_calibrate.updated_model <- function(x, parameter_names,
                                       target_values, par, fn_values) {
  updated_mod <- x
  x <- get_model(x)
  
  names(par) <- parameter_names
  parameters <- modify_(
    get_parameters(x),
    .dots = to_dots(par)
  )
  
  new_model <- run_model_(
    parameters = parameters,
    uneval_strategy_list = get_uneval_strategy_list(x),
    init = get_uneval_init(x),
    cycles = get_cycles(x),
    method = get_method(x),
    cost = get_ce_cost(x),
    effect = get_ce_effect(x),
    state_time_limit = get_state_time_limit(x),
    central_strategy = get_central_strategy(x),
    inflow = get_inflow(x)
  )
  suppressMessages({
    new_updated <- update(new_model,
                          newdata = get_newdata(updated_mod))
  })
  values <- fn_values(new_updated)
  
  sum((values - target_values) ^ 2)
}

#' Define Calibration Function
#' 
#' Define a function to be passed to the `fn_values`
#' argument of [calibrate_model()].
#' 
#' @param type Type of model values (`count` or `value`).
#' @param strategy_names Names of strategies.
#' @param element_names Names of states (for counts) or of
#'   state values (for values).
#' @param cycles Cycles of interest.
#' @param groups Optional grouping of values (values in a
#'   same group have the same `groups`).
#' @param aggreg_fn A function to aggregate values in a same
#'   group.
#'   
#' @return A numeric vector.
#' @export
#' 
#' @example inst/examples/example_define_calibration_fn.R
define_calibration_fn <- function(type, strategy_names,
                                  element_names, cycles,
                                  groups = NULL, aggreg_fn = sum) {
  if (length(type) == 1) {
    type <- rep(type, length(element_names))
  }
  
  stopifnot(
    all(type %in% c("count", "value")),
    length(type) == length(element_names),
    length(type) == length(strategy_names),
    length(element_names) == length(cycles),
    if (! is.null(groups)) length(element_names) == length(groups) else TRUE
  )
  
  f_list <- lapply(
    type,
    function(type_n) {
      if (type_n == "count") get_counts else get_values
    })
  
  ex_list <- lapply(
    seq_along(element_names),
    function(i) {
      if (type[i] == "count") {
        lazyeval::interp(
          ~ state_names == state & markov_cycle == cycle &
            .strategy_names == strat,
          state = element_names[i], cycle = cycles[i],
          strat = strategy_names[i]
        )
      } else {
        lazyeval::interp(
          ~ value_names == value & markov_cycle == cycle &
            .strategy_names == strat,
          value = element_names[i], cycle = cycles[i],
          strat = strategy_names[i]
        )
      }
    })
  
  function(x) {
    res <- unlist(lapply(
      seq_along(ex_list),
      function(i) {
        (f_list[[i]](x) %>% 
           dplyr::filter_(
             .dots = ex_list[[i]]
           ))[[type[i]]]
      })
    )
    
    if (! is.null(groups)) {
      res <- tapply(res, groups, aggreg_fn)
    }
    
    if (! is.numeric(res)) {
      stop(sprintf(
        "Model values are not numeric. Class: %s",
        paste(class(res), collapse = ", ")
      ))
    }
    
    return(res)
  }
}
