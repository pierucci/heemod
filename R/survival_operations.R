#' Project Beyond a Survival Distribution with Another
#' 
#' Project survival from a survival distribution using one or survival
#' distributions using the specified cut points.
#' 
#' @param ... Survival distributions to be used in the projection.
#' @param at A vector of times corresponding to the cut point(s) to be used.
#'   
#' @return A `surv_projection` object.
#' @export
#' 
#' @examples
#' 
#' dist1 = define_survival(distribution = "exp", rate = .5)
#' dist2 = define_survival(distribution = "gompertz", rate = .5, shape = 1)
#' proj_dist = project(dist1, dist2, at=20)
project <- function(..., at) {
  dots <- list(...)
  
  stopifnot(
    all(at > 0),
    all(is.finite(at)),
    !is.unsorted(at, strictly=T),
    length(at) == length(dots) - 1
  )

  # Restructure so that first distribution is "alone"
  # and subsequent distributions are put in a list along
  # with their cut point.
  dist_list <- list()
  for(i in seq_along(dots)) {
    if(i==1) dist_list[[i]] <- dots[[i]]
    else dist_list[[i]] <- list(
      dist = dots[[i]],
      at = at[i-1]
    )
  }
  
  # Use recursion to deal with distributions in pairs
  Reduce(projectFn, dist_list)
}

#' Project Beyond a Survival Distribution with Another (pairwise)
#' 
#' Project survival from a survival distribution using another survival
#' distribution at the specified cutpoint.  Used by project to reduce
#' the list of distributions.
#' 
#' @param dist1 Survival distribution to project from.
#' @param dist2_list A list containing distribution to project with and the
#' time at which projection begins.
#' @return A `surv_projection` object.
#' @keywords internal
projectFn = function(dist1, dist2_list) {
  structure(
    list(
      dist1 = dist1,
      dist2 = dist2_list$dist,
      at = dist2_list$at
    ),
    class = "surv_projection"
  )
}

#' Pool Two or More Survival Distributions
#' 
#' Pool a set of survival distributions using the specified weights.
#' 
#' @param ... Survival distributions to be used in the projection.
#' @param weights A vector of weights used in pooling.
#'   
#' @return A `surv_pooled` object.
#' @export
#' 
#' @examples
#' 
#' dist1 = define_survival(distribution = "exp", rate = .5)
#' dist2 = define_survival(distribution = "gompertz", rate = .5, shape = 1)
#' pooled_dist = pool(dist1, dist2, weights = c(0.25, 0.75))
pool <- function(..., weights=1) {
  
  dots <- list(...)
  
  stopifnot(
    all(weights > 0),
    all(is.finite(weights)),
    length(weights) == length(dots)
  )
  
  structure(
    list(
      dists = dots,
      weights = weights
    ),
    class = "surv_pooled"
  )
}

#' Apply a Hazard Ratio
#' 
#' Proportional reduce or increase the hazard rate of a distribution.
#' 
#' @param dist A survival distribution.
#' @param hr A hazard ratio to be applied.
#' @param log.hr If true, the hazard ratio is exponentiated before being
#' applied.
#'   
#' @return A `surv_ph` object.
#' @export
#' 
#' @examples
#' 
#' dist1 = define_survival(distribution = "exp", rate = .25)
#' ph_dist = apply_hr(dist1, 0.5)
apply_hr = function(dist, hr, log.hr = F) {
  
  stopifnot(
    length(hr) == 1,
    is.finite(hr),
    log.hr | hr > 0
  )
  
  structure(
    list(
      dist = dist,
      hr = ifelse(log.hr, exp(hr), hr)
    ),
    class = "surv_ph"
  )
}

#' Apply an Acceleration Factor
#' 
#' Proportionally increase or reduce the time to event of a survival
#' distribution.
#' 
#' @param dist A survival distribution.
#' @param af An acceleration factor to be applied.
#' @param log.af If true, the accleration factor is exponentiated before being
#' applied.
#'   
#' @return A `surv_aft` object.
#' @export
#' 
#' @examples
#' 
#' dist1 = define_survival(distribution = "exp", rate = .25)
#' aft_dist = apply_af(dist1, 1.5)
apply_af = function(dist, af, log.af = F) {
  
  stopifnot(
    length(af) == 1,
    is.finite(af),
    log.af | af > 0
  )
  
  structure(
    list(
      dist = dist,
      af = ifelse(log.af, exp(af), af)
    ),
    class = "surv_aft"
  )
}

#' Apply an Odds Ratio
#' 
#' Proportionally increase or reduce the odds of an event of a survival
#' distribution.
#' 
#' @param dist A survival distribution.
#' @param or An odds ratio to be applied.
#' @param log.or If true, the odds ratio is exponentiated before being applied.
#'   
#' @return A `surv_po` object.
#' @export
#' 
#' @examples
#' 
#' dist1 = define_survival(distribution = "exp", rate = .25)
#' po_dist = apply_or(dist1, 1.2)
apply_or = function(dist, or, log.or = F) {
  
  stopifnot(
    length(or) == 1,
    is.finite(or),
    log.or | or > 0
  )
  
  structure(
    list(
      dist = dist,
      or = ifelse(log.or, exp(or), or)
    ),
    class = "surv_po"
  )
}

#' Add Hazards
#' 
#' Get a survival distribution reflecting the independent hazards from two or
#' more survival distributions.
#' 
#' @param ... Survival distributions to be used in the projection.
#'   
#' @return A `surv_add_haz` object.
#' @export
#' 
#' @examples
#' 
#' dist1 = define_survival(distribution = "exp", rate = .125)
#' dist2 = define_survival(distribution = "weibull", shape = 1.2, scale = 50)
#' combined_dist = add_hazards(dist1, dist2)
add_hazards = function(...) {
  
  dots <- list(...)
  
  structure(
    list(
      dists = dots
    ),
    class = "surv_add_haz"
  )
}

#' Set Covariates of a Survival Distribution
#' 
#' Set the covariate levels of a survival model to be represented in survival
#' projections.
#' 
#' @param dist a survfit or flexsurvreg object
#' @param ... Covariate values representing the group for which survival
#' probabilities will be generated when evaluated.
#' @param data A an optional data frame repersenting multiple sets of covariate
#' values for which survival probabilities will be generated.  Can be used to
#' generate aggregate survival for a heterogenous set of subjects.
#'   
#' @return A `surv_model` object.
#' @export
#' 
#' @examples
#' 
#' fs1 = flexsurv::flexsurvreg(
#'   survival::Surv(rectime, censrec)~group,
#'   data=flexsurv::bc,
#'   dist = "llogis"
#' )
#' good_model = set_covariates(fs1, group = "Good")
#' cohort = data.frame(group=c("Good", "Good", "Medium", "Poor"))
#' mixed_model = set_covariates(fs1, data = cohort)
set_covariates = function(dist, ..., data = NULL) {
  
  data = rbind(
    data.frame(...),
    data
  )
  
  structure(
    list(
      dist = dist,
      covar = data
    ),
    class = "surv_model"
  )
}
