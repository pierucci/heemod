#' Run Sensitivity Analysis
#' 
#' @param model An evaluated Markov model.
#' @param dsa An object returned by 
#'   [define_dsa()].
#' @return A `data.frame` with one row per model and 
#'   parameter value.
#' @export
#' 
#' @example inst/examples/example_run_dsa.R
run_dsa <- function(model, dsa) {
  
  if (! all(c(".cost", ".effect") %in% names(get_model_results(model)))) {
    stop("No cost and/or effect defined, sensitivity analysis unavailable.")
  }
  
  init <- get_init(model)
  cycles <- get_cycles(model)
  method <- get_method(model)
  strategy_names <- get_strategy_names(model)
  
  list_res <- list()
  resolved_newdata <- list()
  for (n in strategy_names) {
    message(sprintf(
      "Running DSA on strategy '%s'...", n
    ))
    tab <- eval_strategy_newdata(
      model,
      strategy = n,
      newdata = dsa$dsa
    )
      this_resolved_newdata <-
        tab[, names(tab) %in% dsa$variables] %>%
        dplyr::rowwise() %>%
        dplyr::do_(., x = 

        ~ eval_newdata(new_parameters = .,
                     strategy = model$uneval_strategy_list[[n]],
                     old_parameters = get_parameters(model),
                     cycles = 1,
                     init = get_init(model),
                     method = get_method(model),
                     inflow = get_inflow(model),
                     strategy_name = n,
                     expand_limit = get_expand_limit(model, n))$parameters
        ) %>%
        dplyr::ungroup() 

     res <- tab
    list_res <- c(
      list_res,
      list(res)
    )
    resolved_newdata <- c(
      resolved_newdata,
      list(this_resolved_newdata)
    )
    names(resolved_newdata)[length(resolved_newdata)] <- n
  }
  
  for (i in seq_along(strategy_names)) {
    list_res[[i]]$.strategy_names <- strategy_names[i]
  }
  
  res <- Reduce(dplyr::bind_rows, list_res) %>% 
    tidyr::gather_(".par_names", ".par_value",
                   dsa$variables, na.rm = TRUE)
  res <- res %>% 
    dplyr::rowwise() %>% 
    dplyr::do_(~ get_total_state_values(.$.mod)) %>% 
    dplyr::bind_cols(res %>% dplyr::select_(~ - .mod)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate_(.dots = get_ce(model))
  
  structure(
    list(
      dsa = res,
      variables = dsa$variables,
      model = model,
      resolved_newdata = resolved_newdata
    ),
    class = c("dsa", class(res))
  )
}

get_model.dsa <- function(x) {
  x$model
}
