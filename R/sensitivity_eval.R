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
  
  init <- get_uneval_init(model)
  cycles <- get_cycles(model)
  method <- get_method(model)
  strategy_names <- get_strategy_names(model)
  
  n_par <- length(dsa$variables)
  pos_par <- cumsum(c(1, rep(c(n_par, n_par+1), n_par)))
  pos_par <- pos_par[-length(pos_par)]
  
  list_res <- list()
  e_newdata <- list()
  for (n in strategy_names) {
    message(sprintf(
      "Running DSA on strategy '%s'...", n
    ))
    tab <- eval_strategy_newdata(
      model,
      strategy = n,
      newdata = dsa$dsa
    )
    
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
    
    e_newdata <- c(
      e_newdata,
      list(unlist(lapply(
        tab$.mod,
        function(x) x$parameters[1, dsa$variables]))[pos_par]))
    
    names(e_newdata)[length(e_newdata)] <- n
  }
  
  for (i in seq_along(strategy_names)) {
    list_res[[i]]$.strategy_names <- strategy_names[i]
  }
  
  res <- 
    dplyr::bind_rows(list_res) %>%
    tidyr::gather_(
      ".par_names", ".par_value",
      dsa$variables, na.rm = TRUE) %>% 
    dplyr::rowwise()
  
  res <- res %>% 
    dplyr::do_(~ get_total_state_values(.$.mod)) %>% 
    dplyr::bind_cols(res %>% dplyr::select_(~ - .mod)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      .par_value_eval = unlist(e_newdata)) %>% 
    dplyr::mutate_(
      .dots = get_ce(model))
  
  structure(
    list(
      dsa = res,
      variables = dsa$variables,
      model = model
    ),
    class = c("dsa", "list")
  )
}

get_model.dsa <- function(x) {
  x$model
}

digits_at_diff <- function(x, y, addl_digits = 1){
  stopifnot(length(x) == length(y))
  diff <- abs(x - y)
  num_digits <- -floor(log(diff, 10)) + addl_digits
  round_x <- 
    sapply(seq(along = x), 
           function(i){round(x[i], num_digits[i])})
  round_y <- 
    sapply(seq(along = y), 
           function(i){round(y[i], num_digits[i])})
  list(x = round_x, y = round_y, nd = num_digits)
}
