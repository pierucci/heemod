#' Run Probabilistic Uncertainty Analysis
#' 
#' @param model The result of \code{\link{run_model}}.
#' @param resample Resampling distribution for parameters 
#'   defined by \code{\link{define_psa}}.
#' @param N > 0. Number of simulation to run.
#' 
#' @return A list with one \code{data.frame} per model.
#' @export
#' 
#' @example inst/examples/example_run_psa.R
#'   
run_psa <- function(model, resample, N) {
  stopifnot(
    N > 0,
    ! is.null(N)
  )
  
  if (! all(c(".cost", ".effect") %in% names(get_model_results(model)))) {
    stop("No cost and/or effect defined, probabilistic analysis unavailable.")
  }
  
  newdata <- eval_resample(resample, N)
  
  list_res <- list()
  
  for (n in get_strategy_names(model)) {
    message(sprintf("Resampling strategy '%s'...", n))
    list_res <- c(
      list_res,
      list(
        eval_strategy_newdata(
          x = model,
          strategy = n,
          newdata = newdata
        ) %>% 
          dplyr::rowwise() %>% 
          dplyr::do_(~ get_total_state_values(.$.mod)) %>% 
          dplyr::bind_cols(newdata) %>% 
          dplyr::ungroup()
      )
    )
  }
  
  names(list_res) <- get_strategy_names(model)
  index <- seq_len(N)
  
  for (n in names(list_res)) {
    list_res[[n]]$.strategy_names <- n
    list_res[[n]]$.index <- index
  }
  
  res <- Reduce(dplyr::bind_rows, list_res)
  
  res <- dplyr::mutate_(res, .dots = get_ce(model))
  
  run_model <- res %>% 
    dplyr::select_(~ - .index) %>% 
    dplyr::group_by_(".strategy_names") %>%
    dplyr::summarise_all(mean) %>% 
    as.data.frame()
  
  structure(
    list(
      psa = res,
      run_model = run_model[! names(run_model) %in% names(newdata)],
      model = model,
      N = N,
      resamp_par = names(newdata)
    ),
    class = c("psa", class(res))
  )
}

get_model <- function(x) {
  UseMethod("get_model")
}

get_model.psa <- function(x) {
  x$model
}

get_model_results.psa <- function(x) {
  x$run_model
}

get_cycles.psa <- function(x) {
  get_cycles(get_model(x))
}

get_init.psa <- function(x) {
  get_init(get_model(x))
}

get_method.psa <- function(x) {
  get_method(get_model(x))
}

get_central_strategy.psa <- function(x, ...) {
  get_central_strategy(get_model(x))
}

get_noncomparable_strategy.psa <- function(x, ...) {
  get_noncomparable_strategy(summary(x, ...)$res_comp)
}

get_root_strategy.psa <- function(x, ...) {
  get_root_strategy(summary(x, ...)$res_comp)
}

eval_correlation <- function(x, var_names) {
  res <- diag(length(var_names))
  colnames(res) <- var_names
  rownames(res) <- var_names
  
  for (i in seq_len(nrow(x))) {
    res[x$v1[i], x$v2[i]] <- x$cor[i]
    res[x$v2[i], x$v1[i]] <- x$cor[i]
  }
  res
}

#' Evaluate Resampling Definition
#' 
#' @param resample A \code{\link{define_psa}} object.
#' @param N > 0. Number of simulation to run.
#'   
#' @return A \code{data.frame} of resampled values with on 
#'   column per parameter and \code{N} rows.
#'   
#' @keywords internal
eval_resample <- function(resample, N) {
  
  mat_p <- stats::pnorm(
    mvnfast::rmvn(
      n = N,
      mu = rep(0, length(resample$list_qdist)),
      sigma = resample$correlation
    )
  )
  
  list_res <- mapply(
    function(i, f) f(mat_p[, i]),
    seq_len(ncol(mat_p)),
    resample$list_qdist
  )
  
  if (length(dim(list_res)) < 2) {
    list_res <- matrix(list_res, ncol = length(list_res))
  }
  
  colnames(list_res) <- names(resample$list_qdist)
  res <- as.data.frame(list_res)
  
  for (f in resample$multinom) {
    res <- f(res)
  }
  res
}
