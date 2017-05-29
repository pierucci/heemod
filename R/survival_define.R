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
                          "lnorm", "gamma")) {
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

#' Define a survival distribution based on explicit survival probabilities
#'
#' @param x a data frame with columns `time` and `survival` 
#'
#' @return a `registry` object, which can be used with [compute_surv()].
#' @export
#'
#' @examples
#'  x <- data.frame(time = c(0, 1, 5, 10), survival = c(1, 0.9, 0.7, 0.5))
#'  define_registry_survival(x)
#'  
define_registry_survival <- function(x){
  UseMethod("define_registry_survival")
}

#' @rdname define_registry_survival
#' @export
define_registry_survival.data.frame <- function(x){
  required_names <- c("time", "survival")
  names_present <- required_names %in% names(x)
  if(any(!names_present)){
    stop("missing column",
         plur(sum(!names_present)),
         " in registry object: ",
         paste(required_names[!names_present], collapse = ", ")
    )
  }
  x$time <- as.numeric(x$time)
  x <- x[order(x$time),]
  dup_time <- duplicated(x$time)
  if(any(dup_time))
    stop("any time can appear only once in explicit survival data. ",
         "Duplicated time",
         plur(sum(dup_time)),
         ": ",
         paste(x$time[dup_time], collapse = ", ")
    )
  
  if(x$time[1] != 0 | x$survival[1] != 1)
    stop("registry data must start with time 0 and survival 1")
  
  increasing_survival <- diff(x$survival) > 0
  if(any(increasing_survival)){
    problem_times <- matrix(x$time[which(increasing_survival) + c(0,1)],
                            ncol = 2, byrow = TRUE)
    stop("survival cannot increase over time; see times:\n",
         paste("(", 
               problem_times[,1],
               ", ",
               problem_times[,2],
               ")",
               sep = "", collapse = ", ")
    )
  }
  class(x) <- c("registry", "surv_object", "data.frame")
  x
}
#' @rdname define_registry_survival
#' @export
define_registry_survival.character <- function(x){
  define_registry_survival(read_file(x))
}
