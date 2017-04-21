#' Determine whether to dose during certain cycles.
#'
#' @param N cycles or periods to check.
#' @param init Non-repeating dosing indicator at beginning.
#' @param pattern Repeating dosing pattern after initial.
#' @param first if \code{init} is not specified, how many periods
#'   at the beginning should be dosed.  Must be non-negative.
#' @param then_every if \code{pattern} is not specified, make it
#'   (then_every - 1) zeroes followed by a 1.   If \code{then_every}
#'   has any negative value, there is no dosing after the initial
#'   period (equivalent to setting pattern = 0).
#' @param cap largest dosing period
#'
#' @return A logical vector indicating whether or not a patient
#'   is dosed during the relevant period (markov cycle).
#' @export
#' @details \code{init} takes precedence over \code{first}; that is,
#'   if \code{init} is defined, then \code{first} is ignored.  Similarly,
#'   \code{pattern} takes precedence over \code{then_every}.
#' @examples{
#'   is_dosing_period(N = 1:13, first = 4, then_every = 3, cap = 40)
#'   is_dosing_period(N = 37:46, first = 4, then_every = 3, cap = 40)
#'   is_dosing_period(N = 1:100, init = c(1,0,1,0,1,0,1,1), pattern = c(1, 0, 1, 1, 0), cap = 120)
#'   ## stop after initial period
#'   is_dosing_period(N = 1:8, first = 4, pattern = 0, cap = 40)
#'   is_dosing_period(N = 1:8, first = 4, then_every = -1, cap = 40)
#'   ## demonstrating argument precedence rules
#'   is_dosing_period(N = 1:10, init = c(1,0,1), first = 3, then_every = 5)
#'   is_dosing_period(N = 1:10, init = numeric(0), pattern = c(1, 1, 0, 1, 0), then_every = 2)
#' }
#' 
is_dosing_period <- function(N, init, pattern, first, then_every, cap = Inf){
  if(missing(init)){
    if(missing(first)) stop("must specify either init or first")
    if(first < 0) stop("first must be 0 or positive")
    init <- rep(1, first)
  }
  if(missing(pattern)){
    if(missing(then_every)) stop("must specify either pattern or then_every")
    if(then_every < 0) pattern <- 0
    else pattern <- c(rep(0, then_every - 1), 1)
  }
  if(!all(c(init, pattern) %in% c(0,1, TRUE, FALSE)))
      stop("all elements of init and pattern must be FALSE or 0 or TRUE or 1")
  linit <- length(init)
  cond1 <- N <= length(init) & as.logical(init[N])
  cond2 <- N > length(init) & N <= cap
  cond3 <- as.logical(pattern[(N - length(init) - 1) %% length(pattern) + 1])
  cond1 | (cond2 & cond3)
  }


