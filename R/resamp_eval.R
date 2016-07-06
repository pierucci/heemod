#' Run Probabilistic Uncertainty Analysis
#' 
#' @param model The result of \code{\link{run_models}}.
#' @param resample Resampling distribution for parameters 
#'   defined by \code{\link{define_distrib}}.
#' @param N > 0. Number of simulation to run.
#'   
#' @return A list with one \code{data.frame} per model.
#' @export
#' 
#' @example inst/examples/example_run_probabilistic.R
#'   
run_probabilistic <- function(model, resample, N) {
  
  stopifnot(
    N > 0
  )
  
  if (! all(c(".cost", ".effect") %in% names(model))) {
    stop("No cost and/or effect defined, probabilistic analysis unavailable.")
  }
  
  newdata <- eval_resample(resample, N)
  
  init <- attr(model, "init")
  cycles <- attr(model, "cycles")
  method <- attr(model, "method")
  list_models <- attr(model, "uneval_model_list")
  
  list_res <- list()
  
  for (i in seq_along(list_models)) {
    message(sprintf("Running model '%s'...", names(list_models)[i]))
    list_res <- c(
      list_res,
      list(
        eval_model_newdata(
          model = list_models[[i]], method = method,
          old_parameters = get_parameters(model),
          init = init, cycles = cycles, newdata = newdata
        )
      )
    )
  }
  names(list_res) <- names(list_models)
  index <- seq_len(N)
  
  for (n in names(list_res)) {
    list_res[[n]]$.model_names <- n
    list_res[[n]]$.index <- index
  }
  
  res <- Reduce(dplyr::bind_rows, list_res)
  
  res <- dplyr::mutate_(res, .dots = attr(model, "ce"))
  
  structure(
    res, 
    class = c("probabilistic", class(res)),
    model = model
  )
}

get_base_model.probabilistic <- function(x, ...) {
  get_base_model(attr(x, "model"))
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
#' @param resample A \code{\link{define_distrib}} object.
#' @param N > 0. Number of simulation to run.
#'   
#' @return A \code{data.frame} of resampled values with on
#' column per parameter and \code{N} rows.
#' 
#' 
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
  
  for (f in attr(resample, "multinom")) {
    res <- f(res)
  }
  res
}
