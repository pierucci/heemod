vswitch <- function(x, ...)
  UseMethod("vswitch")

vswitch.factor <- function(x, ...) {
  x <- levels(x)[x]
  
  vswitch(x, ...)
}

vswitch.character <- function(x, ...) {
  listRes <- list(...)
  nRes <- names(listRes)
  if (is.null(nRes))
    stop("'...' should be named when 'x' is a character vector.")
  if (any(!is.na(x) & ! x %in% nRes))
    stop("Some values of 'x' do not correspond to any name in '...'.")
  x <- match(x, nRes)
  names(listRes) <- NULL
  
  do.call(vswitch, c(list(x = x), listRes))
}

vswitch.default <- function(x, ...) {
  listRes <- lapply(list(...),
                    function(x) rep(x, length.out = length(x)))
  if (! is.null(names(listRes)))
    warning("Named '...' with non character/factor 'x'. Names will be ignored.")
  if (any(!is.na(x) & (x > length(listRes) | x < 1)))
    stop("Some values of 'x' are out of range.")
  
  tabRes <- as.data.frame(listRes)
  iRows <- seq(length.out = nrow(tabRes))
  
  as.matrix(tabRes)[cbind(iRows, x)]
}

#' Dispatch Values According to Strategy
#' 
#' Returns different values depending on the strategy.
#' 
#' @param .strategy Optional strategy name. If not specified
#'   it is implicitely added.
#' @param ... Values of the parameter named depending on the
#'   strategy.
#'   
#' @return A vector of values.
#' @export
#' 
#' @examples
#' 
#' define_parameters(
#'   val = 456,
#'   x = dispatch_strategy(
#'     strat_1 = 1234,
#'     strat_2 = 9876,
#'     strat_3 = val * 2 + markov_cycle
#'   )
#' )
dispatch_strategy <- function(.strategy, ...) {
  .dots <- lazyeval::lazy_dots(...)
  if (is.null(names(.dots)) || any(is.na(names(.dots)))) {
    stop("All arguments to 'dispatch_strategy()' must be named.")
  }
  vswitch(.strategy, ...)
}

#' Hack to Automate Use of Strategy Name
#' 
#' This function is a hack to automate the definition of the
#' argument \code{.strategy} in
#' \code{\link{dispatch_strategy}}.
#' 
#' The hack consists in replacing calls to 
#' \code{dispatch_strategy(...)} by 
#' \code{dispatch_strategy(.strategy = strategy, ...)} if
#' \code{.strategy_name} is not already defined.
#' 
#' @param .dots A \code{lazy_dots} object.
#'   
#' @return A modified \code{lazy_dots} object.
#'   
#' @keywords internal
dispatch_strategy_hack <- function(.dots) {
  f <- function (x, env) {
    if (is.atomic(x) || is.name(x)) {
      x
    } else if (is.call(x)) {
      if (dispatch_strategy_check(x[[1]], env)) {
        x <- pryr::standardise_call(x)
        if (is.null(x$.strategy)) {
          x$.strategy <- substitute(strategy)
        }
      }
      as.call(lapply(x, f, env = env))
    } else if (is.pairlist(x)) {
      as.pairlist(lapply(x, f, env = env))
    } else {
      stop(sprintf(
        "Don't know how to handle type %s.",
        typeof(x)))
    }
  }
  
  do.call(
    structure,
    c(list(
      .Data = lapply(
        .dots,
        function(x) {
          x$expr <- f(x$expr, env = x$env)
          x
        }
      )),
      attributes(.dots)
    )
  )
}

# Ensure only heemod version of dispatch_strategy gets used
dispatch_strategy_check <- function(x, env) {
  if (identical(x, quote(dispatch_strategy))) {
    if (identical(environment(eval(x, envir = env)),
                  asNamespace("heemod"))) {
      TRUE
    } else {
      warning("A version of 'dispatch_strategy()' that is not defined by heemod was found.")
      FALSE
    }
  } else {
    FALSE
  }
}
