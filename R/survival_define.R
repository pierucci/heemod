#' Define a Survival Distribution
#' 
#' Define a parametric survival distribution.
#' 
#' @param distribution A parametric survival distribution.
#' @param ... Additional distribution parameters (see 
#'   respective distribution help pages).
#'   
#' @return A `surv_dist` object.
#' @export
#' 
#' @examples
#' 
#' define_survival(distribution = "exp", rate = .5)
#' define_survival(distribution = "gompertz", rate = .5, shape = 1)
#' 
define_survival <- function(distribution = c("exp", "weibull",
                                             "weibullPH",
                                             "lnorm", "llogis",
                                             "gamma", "gompertz",
                                             "gengamma",
                                             "gengamma.orig",
                                             "genf", "genf.orig"),
                            ...) {
  
  distribution <- match.arg(distribution)
  
  list_arg <- list(...)
  
  if (distribution %in% c("exp", "weibull",
                          "llogis", "lnorm", "gamma")) {
    env_f <- asNamespace("stats")
  } else {
    if (! requireNamespace("flexsurv")) {
      stop("'flexsurv' package required.")
    }
    env_f <- asNamespace("flexsurv")
  }
  
  pf <- get(paste0("p", distribution),
            envir = env_f)
  
  names_fun <- setdiff(names(list_arg), "distribution")
  names_par <- setdiff(names(formals(pf)), "q")
  
  correct_names <- names_fun %in% names_par
  
  if (! all(correct_names)) {
    stop(sprintf(
      "Incorrect argument%s: %s.",
      plur(sum(! correct_names)),
      paste(names_fun[! correct_names], collapse = ", ")))
  }
  
  structure(
    list(
      distribution = distribution,
      ...
    ),
    class = c("surv_object", "surv_dist")
  )
}

#' Define a Restricted Cubic Spline Survival Distribution
#' 
#' Define a restricted cubic spline parametric survival
#' distribution.
#' 
#' @param scale "hazard", "odds", or "normal", as described
#'   in flexsurvspline. With the default of no knots in
#'   addition to the boundaries, these models reduce to the
#'   Weibull, log-logistic and log-normal respectively. The
#'   scale must be common to all times.
#' @param ... Additional distribution parameters (see 
#'   respective distribution help pages).
#'   
#' @return A \code{surv_dist} object.
#'   
#' @examples
#' 
#' define_spline_survival(
#'   scale = "hazard", 
#'   gamma = c(-18.3122, 2.7511, 0.2292), 
#'   knots=c(4.276666, 6.470800, 7.806289)
#' )
#' define_spline_survival(
#'   scale = "odds", 
#'   gamma = c(-18.5809, 2.7973, 0.2035), 
#'   knots=c(4.276666, 6.470800, 7.806289)
#' )
#' 
#' @export
define_spline_survival <- function(scale = c("hazard", "odds", 
                                             "normal"),
                                   ...) {
  
  scale <- match.arg(scale)
  
  list_arg <- list(...)
  
  if (! requireNamespace("flexsurv")) {
    stop("'flexsurv' package required.")
  }
  
  pf <- flexsurv::psurvspline
  
  names_fun <- setdiff(names(list_arg), "scale")
  names_par <- setdiff(names(formals(pf)), "q")
  
  correct_names <- names_fun %in% names_par
  
  if (! all(correct_names)) {
    stop(sprintf(
      "Incorrect argument%s: %s.",
      plur(sum(! correct_names)),
      paste(names_fun[! correct_names], collapse = ", ")))
  }
  
  structure(
    list(
      distribution = "survspline",
      scale = scale,
      ...
    ),
    class = c("surv_object", "surv_dist")
  )
}
