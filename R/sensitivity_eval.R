#' Run Sensitivity Analysis
#' 
#' @param model An evaluated Markov model.
#' @param dsa An object returned by 
#'   \code{\link{define_dsa}}.
#' @param sensitivity Deprecated, use \code{dsa} instead.
#'   
#' @return A \code{data.frame} with one row per model and 
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
  
  list_res <- lapply(
    strategy_names,
    function(n) {
      tab <- eval_strategy_newdata(
        model,
        strategy = n,
        newdata = dsa$dsa
      ) 
      tab %>% 
        dplyr::mutate_if(
          names(tab) %in% dsa$variables,
          dplyr::funs(to_text_dots),
          name = FALSE
        )
    }
  )
  
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