#' Iteratively Evaluate a Markov Model With New Parameter 
#' Values
#' 
#' Given a data.frame with on set of new parameters values 
#' per row, iteratively evaluate the model over the set of 
#' new values.
#' 
#' New parameters with a missing value (\code{NA}) do not 
#' replace existing parameters.
#' 
#' @param x Result from \code{\link{run_model}}.
#' @param model Name or index of model to recompute.
#' @param newdata a data.frame whose names match parameters 
#'   names. \code{model} will be evaluated iteratively, 
#'   taking successive values from each row.
#'   
#' @return A data.frame containing the values of 
#'   \code{newdata} and each Markov Model evaluation in 
#'   \code{res}.
#'   
#' @example inst/examples/example_eval_strategy_newdata.R
#'   
#' @keywords internal
eval_strategy_newdata <- function(x, strategy = 1, newdata) {
  check_strategy_index(x = x, i = strategy)
  
  cycles <- get_cycles(x)
  init <- get_init(x)
  inflow <- get_inflow(x)
  method <- get_method(x)
  old_parameters <- get_parameters(x)
  uneval_strategy <- x$uneval_strategy_list[[strategy]]
  
  if (status_cluster(verbose = FALSE)) {
    cl <- get_cluster()
    
    num_cores <- length(cl)
    
    message(paste("Using a cluster with", num_cores, "cores."))
    
    split_vec <- rep(1:num_cores, each = nrow(newdata) %/% num_cores)
    split_vec <- c(split_vec, rep(num_cores, nrow(newdata) %% num_cores))
    
    pnewdata <- split(newdata, split_vec)
    parallel::clusterExport(
      cl, 
      c("uneval_strategy", "old_parameters", "pnewdata", 
        "cycles", "init", "method"),
      envir = environment()
    )
    
    pieces <- parallel::parLapply(cl, pnewdata, function(newdata) {
      
      newdata %>% 
        dplyr::rowwise() %>% 
        dplyr::do_(
          .mod = ~ eval_newdata(
            .,
            strategy = uneval_strategy,
            old_parameters = old_parameters,
            cycles = cycles,
            init = init,
            inflow = inflow,
            method = method
          )
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::bind_cols(
          newdata
        )
    })
    res <- do.call("rbind", pieces)
    rownames(res) <- NULL
    
  } else {
    res <- newdata %>% 
      dplyr::rowwise() %>% 
      dplyr::do_(
        .mod = ~ eval_newdata(
          .,
          strategy = uneval_strategy,
          old_parameters = old_parameters,
          cycles = cycles,
          init = init,
          method = method,
          inflow = inflow
        )
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::bind_cols(
        newdata
      )
    
  }
  res
}

eval_newdata <- function(new_parameters, strategy, old_parameters,
                         cycles, init, method, inflow) {
  
  new_parameters <- Filter(
    function(x) all(! is.na(x)),
    new_parameters
  )
  
  lazy_new_param <- lazyeval::as.lazy_dots(new_parameters)
  
  parameters <- utils::modifyList(
    old_parameters,
    lazy_new_param
  )
  
  eval_strategy(
    strategy = strategy,
    parameters = parameters,
    cycles = cycles,
    init = init,
    method = method,
    inflow = inflow
  )
}
