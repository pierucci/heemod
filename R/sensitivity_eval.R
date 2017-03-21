#' Run Sensitivity Analysis
#' 
#' @param model An evaluated Markov model.
#' @param dsa An object returned by 
#'   [define_dsa()].
#' @param resolve_labels logical. should expressions that will eventually be
#'   used as labels when the dsa is plotted be resolved to values?
#'   
#' @return A `data.frame` with one row per model and 
#'   parameter value.
#' @export
#' 
#' @example inst/examples/example_run_dsa.R
run_dsa <- function(model, dsa, resolve_labels = FALSE) {
  
  if (! all(c(".cost", ".effect") %in% names(get_model_results(model)))) {
    stop("No cost and/or effect defined, sensitivity analysis unavailable.")
  }
  
  init <- get_init(model)
  cycles <- get_cycles(model)
  method <- get_method(model)
  strategy_names <- get_strategy_names(model)
  
  list_res <- list()
  for (n in strategy_names) {
    message(sprintf(
      "Running DSA on strategy '%s'...", n
    ))
    tab <- eval_strategy_newdata(
      model,
      strategy = n,
      newdata = dsa$dsa
    )
    if(resolve_labels){
      resolved_newdata <-
        eval_newdata(new_parameters = tab[, names(tab) %in% dsa$variables],
                     strategy = model$uneval_strategy_list[[n]],
                     old_parameters = get_parameters(model),
                     cycles = 1,
                     init = get_init(model),
                     method = get_method(model),
                     inflow = get_inflow(model),
                     strategy_name = n,
                     expand_limit = get_expand_limit(model, n))$parameters

      for(this_col in dsa$variables){
        tab[, this_col][[1]] <-
          lapply(tab[, this_col][[1]],
                 function(x){
                   if(inherits(x, "lazy")){
                      x <- list(expr = lazy_eval(x, resolved_newdata))
                   }
                 x
                 }
          )

      }
    }
     res <- tab %>%
      dplyr::mutate_if(
        names(tab) %in% dsa$variables,
        dplyr::funs(to_text_dots),
        name = FALSE
      )
    list_res <- c(
      list_res,
      list(res)
    )
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
      model = model
    ),
    class = c("dsa", class(res))
  )
}

get_model.dsa <- function(x) {
  x$model
}
