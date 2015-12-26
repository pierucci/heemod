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
                            correlation = diag(length(list(...)))) {
  list_qdist <- list(...)
  
  stopifnot(
    length(unique(names(list_qdist))) == length(list_qdist)
  )
  
  if (class(correlation) == "correlation_matrix") {
    correlation <- eval_correlation(correlation, names(list_qdist))
  }
  
  stopifnot(
    nrow(correlation) == ncol(correlation),
    nrow(correlation) == length(list_qdist),
    all(correlation >= -1) & all(correlation <= 1),
    isTRUE(all.equal(diag(correlation), rep(1, ncol(correlation))))
  )
  
  structure(
    list(
      list_qdist = list_qdist,
      correlation = correlation
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

#' Define a Correlation Structure for Probabilistic 
#' Incertitude Analysis
#' 
#' Not all correlation need to be specified for all variable
#' combinations, unspecified correlations are assumed to be
#' 0.
#' 
#' @param ... A list of parameter names and correlation 
#'   coeficients of the form \code{var1, var2, cor(var1, 
#'   var2), var3, var4, cor(var3, var4), ...}
#'   
#' @return An object of class \code{correlation_matrix}.
#' @export
#' 
#' @examples
#' 
#' cm <- define_correlation(
#'     var1, var2, .4,
#'     var1, var3, -.2,
#'     var2, var3, .1
#'   )
#' 
define_correlation <- function(...) {
  .dots <- lazyeval::lazy_dots(...)
  
  define_cor_mat_(.dots)
}

define_correlation_ <- function(.dots) {
  stopifnot(
    length(.dots) %% 3 == 0
  )
  
  f <- function(i) {
    if (i %% 3 == 0) {
      lazyeval::lazy_eval(.dots[[i]])
    } else {
      deparse(.dots[[i]]$expr)
    }
  }
  
  list_res <- lapply(seq_along(.dots), f)
  
  res <- dplyr::data_frame(
    v1 = unlist(list_res[seq(from = 1, to = length(list_res), by = 3)]),
    v2 = unlist(list_res[seq(from = 2, to = length(list_res), by = 3)]),
    cor = unlist(list_res[seq(from = 3, to = length(list_res), by = 3)])
  )
  
  stopifnot(
    ! any(duplicated(
      mapply(
        function(x, y) paste(sort(c(x, y)), collapse = ""),
        res$v1, res$v2
      )))
  )
  structure(res, class = "correlation_matrix")
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
      sigma = resample$correlation
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
