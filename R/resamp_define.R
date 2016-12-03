#' Define Parameters Distribution for Probabilistic Analysis
#' 
#' Define the properties of parameter distributions and 
#' their correlation structure for probabilistic uncertainty
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
#' @param list_qdist List of resampling functions.
#' @param list_multi List of multinomial parameters.
#'   
#' @return An object of class \code{resamp_definition}. 
#'   Contains \code{list_qdist}, a list of quantile 
#'   functions and \code{correlation} a correlation matrix.
#' @export
#' 
#' @example inst/examples/example_define_resample.R
#'   
define_psa <- function(...,
                       correlation) {
  .dots <- list(...)
  
  list_input <- lapply(
    .dots,
    function(x) eval(attr(stats::terms(x), "variables")[[3]])
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
  
  define_psa_(list_qdist, list_multi, correlation)
}

#' @rdname define_psa
define_psa_ <- function(list_qdist, list_multi, correlation) {
  
  if (any(duplicated(names(list_qdist)))) {
    stop("Some parameter names are duplicated.")
  }
  
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
      correlation = correlation,
      multinom = list_multi
    ),
    class = "resamp_definition"
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
#' Uncertainty Analysis
#' 
#' Not all correlation need to be specified for all variable
#' combinations, unspecified correlations are assumed to be 
#' 0.
#' 
#' @param ... A list of parameter names and correlation 
#'   coeficients of the form \code{var1, var2, cor(var1, 
#'   var2), var3, var4, cor(var3, var4), ...}.
#' @param .dots Used to work around non-standard evaluation.
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

#' @rdname define_correlation
define_correlation_ <- function(.dots) {
  if (! length(.dots) %% 3 == 0) {
    stop("Incorrect number of elements in correlation definition, the correct form is A, B, cor(A, B)...")
  }
  
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
  
  if (any(res$cor > 1) | any(res$cor < -1)) {
    stop("Correlation values must be between -1 and 1.")
  }
  
  if (any(duplicated(
    mapply(
      function(x, y) paste(sort(c(x, y)), collapse = ""),
      res$v1, res$v2
    )))) {
    stop("A correlation is defined more than once.")
  }
  
  structure(res, class = c("correlation_matrix", class(res)))
}

#' @export
print.correlation_matrix <- function(x, ...) {
  var_names <- unique(c(x$v1, x$v2))
  res <- diag(length(var_names))
  colnames(res) <- var_names
  rownames(res) <- var_names
  
  for (i in seq_len(nrow(x))) {
    res[x$v1[i], x$v2[i]] <- x$cor[i]
    res[x$v2[i], x$v1[i]] <- x$cor[i]
  }
  print(as.table(res), zero.print = "-", ...)
}

#' @export
print.resamp_definition <- function(x, ...) {
  cat(sprintf(
    "A PSA definition:\n\n%i parameter%s resampled, %i multinomial group%s.\n",
    length(x$list_qdist),
    plur(length(x$list_qdist)),
    length(x$multinom),
    plur(length(x$multinom))
    ))
}
