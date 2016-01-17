#' Define Parameters Distribution for Probabilistc Analysis
#' 
#' Define the properties of parameter distributions and 
#' their correlation structure for probabilistic incertitude
#' analysis of Markov models.
#' 
#' If no correlation matrix is specified parameters are 
#' assumed to be independant.
#' 
#' The correlation patrix need only be specified for 
#' correlated parameters.
#' 
#' @param ... Formulas defining parameter distributions.
#' @param correlation A correlation matrix for parameters or
#'   the output of \code{\link{define_correlation}}.
#'   
#' @return An object of class \code{resamp_definition}. 
#'   Contains \code{list_qdist}, a list of quantile 
#'   functions and \code{correlation} a correlation matrix.
#' @export
#' 
#' @example inst/examples/example_define_resample.R
#'   
define_resample <- function(...,
                            correlation) {
  .dots <- list(...)
  
  list_input <- lapply(
    .dots,
    function(x) eval(attr(terms(x), "variables")[[3]])
  )
  
  list_qdist <- unlist(
    list_input,
    recursive = FALSE
  )
  names(list_qdist) <- unlist(
    lapply(
      .dots,
      function(x) all.vars(x)
    )
  )
  
  list_multi <- lapply(
    .dots[unlist(lapply(list_input,
                        function(x) "multinom_param" %in% class(x)))],
    function(x) define_multinom(force(all.vars(x)))
  )
  
  if (missing(correlation)){
    correlation <- diag(length(list_qdist))
  }
  
  define_resample_(list_qdist, list_multi, correlation)
}

define_resample_ <- function(list_qdist, list_multi, correlation) {
  
  stopifnot(
    length(unique(names(list_qdist))) == length(list_qdist),
    all(unlist(lapply(list_multi,
                      function(x) "multinom" %in% class(x))))
  )
  
  # additional checks
  # all parameters in list_multi are r_multi ou r_binom parameters
  # all r_multi or r_binom are present in 1 and only 1 multi
  
  if ("correlation_matrix" %in% class(correlation)) {
    correlation <- eval_correlation(correlation, names(list_qdist))
  }
  
  stopifnot(
    nrow(correlation) == ncol(correlation),
    nrow(correlation) == length(list_qdist),
    all(correlation >= -1) & all(correlation <= 1),
    isTRUE(all.equal(as.vector(diag(correlation)),
                     rep(1, ncol(correlation))))
  )
  
  structure(
    list(
      list_qdist = list_qdist,
      correlation = correlation
    ),
    class = "resamp_definition",
    multinom = list_multi
  )
}

#' Define That Parameters Belong to the Same Multinomial
#' Distribution
#' 
#' @param x A vector of parameter names.
#'   
#' @return An object of class \code{multinomial}.
#' 
define_multinom <- function(x) {
  char_var <- x
  
  # ugly piece of shit code
  expr_denom <- parse(text = paste(char_var, collapse = "+"))
  
  res <- function(x) {
    
    # ugly ugly baaaaad
    # creates copies everywhere
    # replace this with a nice mutate_() or something...
    
    denom <- eval(expr_denom, x)
    
    for (var in char_var) {
      x[[var]] <- x[[var]] / denom
    }
    x
  }
  
  structure(
    res,
    class = c("function", "multinom")
  )
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
  
  define_correlation_(.dots)
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
  structure(res, class = c("correlation_matrix", class(res)))
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
#' @param model The result of \code{\link{run_model}}.
#' @param resample Resampling distribution for parameters 
#'   defined by \code{\link{define_resample}}.
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
          init = init, cycles = cycles, newdata = newdata
        )
      )
    )
  }
  names(list_res) <- names(list_models)
  index <- seq_len(N)
  
  for (n in names(list_res)) {
    list_res[[n]]$.model_name <- n
    list_res[[n]]$.index <- index
  }
  
  res <- Reduce(dplyr::bind_rows, list_res)
  
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
