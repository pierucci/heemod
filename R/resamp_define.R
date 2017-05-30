#' Define Parameters Distribution for Probabilistic Analysis
#' 
#' Define the properties of parameter distributions and 
#' their correlation structure for probabilistic uncertainty
#' analysis of Markov models.
#' 
#' The distributions must be defined within `heemod` 
#' (see [distributions]), or defined with
#' [define_distribution()].
#' 
#' If no correlation matrix is specified parameters are 
#' assumed to be independant.
#' 
#' The correlation patrix need only be specified for 
#' correlated parameters.
#' 
#' @param ... Formulas defining parameter distributions.
#' @param correlation A correlation matrix for parameters or
#'   the output of [define_correlation()].
#' @param .dots Pair/values of expressions coercible to lazy objects.
#'   
#' @return An object of class `resamp_definition`. 
#'   Contains `list_qdist`, a list of quantile 
#'   functions and `correlation` a correlation matrix.
#' @export
#' 
#' @example inst/examples/example_define_resample.R
#'   
define_psa <- function(...,
                       correlation) {
  .dots <- lazyeval::lazy_dots(...)
  define_psa_(.dots, correlation)
}
  
#' @rdname define_psa
define_psa_ <- function(.dots = list(), correlation) {
  eval_dots <- lazyeval::lazy_eval(.dots)
  lapply(
    eval_dots,
    function(x) {
      if (! inherits(x, "formula")) {
        stop("Parameter distributions must be written as formulas.")
      }
      if (is_one_sided(x)) {
        stop("Parameter names must be on the left hand of the formula.")
      }
    }
  )
  
  list_input <- lapply(
    eval_dots,
    function(x) {
      eval(rhs(x), envir = asNamespace("heemod"))
    })
  
  list_qdist <- unlist(
    list_input,
    recursive = FALSE
  )
  lapply(list_qdist, function(x) {
    if (! inherits(x, "function")) {
      stop("Distributions must be defined as functions.")
    }
  })
  
  n_par <- unlist(lapply(list_input, length))
  
  names(list_qdist) <- unlist(
    lapply(
      eval_dots,
      function(x) all.vars(lhs(x))
    )
  )
  
  is_multinom <- unlist(lapply(
    list_input,
    inherits, "multinom_param"))
  
  list_multi <- lapply(
    eval_dots[is_multinom],
    function(x) all.vars(lhs(x), unique = FALSE)
  )
  
  if (any(pb <- duplicated(unlist(list_multi)))) {
    stop(sprintf(
      "Some multinomial parameters are duplicated: %s.",
      paste(unique(unlist(list_multi)[pb]), collapse = ", ")
    ))
  }
  
  if (any(pb <- n_par[is_multinom] != unlist(lapply(list_multi, length)))) {
    stop(sprintf(
      "Number of multinomial distribution paramter does not correspond to number of variable: %s.",
      paste(unlist(lapply(list_multi[pb], paste, collapse = " ")), collapse = ", ")
    ))
  }
  
  if (missing(correlation)){
    correlation <- diag(length(list_qdist))
  }
  
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
#' @return An object of class `correlation_matrix`.
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
