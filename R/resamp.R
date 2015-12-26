#' Define Parameters Distribution for Probabilistc Analysis
#' 
#' Define the properties of parameter distributions and 
#' their correlation structure for probabilistic incertitude
#' analysis of Markov models.
#' 
#' If no correlation matrix is specified parameters are 
#' assumed to be independant.
#' 
#' @param ... Name-value pairs of expressions defining 
#'   parameter distributions.
#' @param mat_cor A correlation matrix for parameters.
#'   
#' @return An object of class \code{resamp_definition}. 
#'   Contains \code{list_qdist}, a list of quantile
#'   functions and \code{mat_cor} a correlation matrix.
#' @export
#' 
#' @example R/example_define_resample.example
#' 
define_resample <- function(...,
                            mat_cor = diag(length(list(...)))) {
  list_qdist <- list(...)
  
  stopifnot(
    length(unique(names(list_qdist))) == length(list_qdist),
    nrow(mat_cor) == ncol(mat_cor),
    nrow(mat_cor) == length(list_qdist),
    all(mat_cor >= -1) & all(mat_cor <= 1),
    isTRUE(all.equal(diag(mat_cor), rep(1, ncol(mat_cor))))
  )
  
  structure(
    list(
      list_qdist = list_qdist,
      mat_cor = mat_cor
    ),
    class = "resamp_definition"
  )
}

#' Convenience Probability Density Functions for 
#' Probabilistic Analysis
#' 
#' @param mean Distribution mean.
#' @param sd Distribution standard deviation.
#'   
#' @return A function taking a probability as argument and
#'   returning the quantile from the specified ditribution.
#' @export
#' 
r_norm <- function(mean, sd) {
  function(x) qnorm(p = x, mean = mean, sd = sd)
}

#' Run Probabilistic Incertitude Analysis
#'
#' @param model The result of \code{\code{run_model}}.
#' @param resample Resampling distribution for parameters
#' defined by \code{\link{define_resample}}.
#' @param N > 0. Number of simulation to run.
#'
#' @return A list with one data.frame per model.
#' @export
#'
#' @example R/example_run_probabilistic.example
#' 
run_probabilistic <- function(model, resample, N) {
  
  stopifnot(
    N > 0
  )
  
  newdata <- eval_resample(resample, N)
  
  init <- attr(model, "init")
  cycles <- attr(model, "cycles")
  list_models <- attr(model, "uneval_model_list")
  
  res <- lapply(list_models, eval_model_newdata,
                init = init, cycles = cycles, newdata = newdata)
  return(res)
}

#' Evaluate Resampling Definition
#' 
#' @param resample A \code{\link{define_resample}} object.
#' @param N > 0. Number of simulation to run.
#'   
#' @return A \code{data.frame} of resampled values with on
#' column per parameter and \code{N} rows.
#' 
#' 
eval_resample <- function(resample, N) {
  mat_p <- pnorm(
    mvnfast::rmvn(
      n = N,
      mu = rep(0, length(resample$list_qdist)),
      sigma = resample$mat_cor
    )
  )
  
  list_res <- mapply(
    function(i, f) f(mat_p[, i]),
    seq_len(ncol(mat_p)),
    resample$list_qdist
  )
  
  colnames(list_res) <- names(resample$list_qdist)
  as.data.frame(list_res)
}
