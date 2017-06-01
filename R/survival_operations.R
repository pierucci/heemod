#' Join Beyond a Survival Distribution with Another
#' 
#' Join survival from a survival distribution using one
#' or more survival distributions using the specified cut points.
#' 
#' @param ... Survival distributions to be used in the
#'   junction.
#' @param .dots Used to work around non-standard evaluation.
#' @param at A vector of times corresponding to the cut
#'   point(s) to be used.
#'   
#' @return A `surv_projection` object.
#' @export
#' 
#' @examples
#' 
#' dist1 <- define_survival(distribution = "exp", rate = .5)
#' dist2 <- define_survival(distribution = "gompertz", rate = .5, shape = 1)
#' join_dist <- join(dist1, dist2, at=20)
join <- function(..., at) {
  dots <- list(...)
  
  join_(dots, at)
}

#' @export
#' @rdname join
project <- function(...) {
  warning("'project() is deprecated, use 'join()' instead.")
  join(...)
}

#' @export
#' @rdname join
project_ <- function(...) {
  warning("'project_() is deprecated, use 'join_()' instead.")
  join_(...)
}

#' @export
#' @rdname join
join_ <- function(.dots, at) {
  
  stopifnot(
    all(at > 0),
    all(is.finite(at)),
    ! is.unsorted(at, strictly = TRUE),
    length(at) == length(.dots) - 1
  )
  
  # Restructure so that first distribution is "alone"
  # and subsequent distributions are put in a list along
  # with their cut point.
  dist_list <- list()
  for (i in seq_along(.dots)) {
    if (i == 1) {
      dist_list[[i]] <- .dots[[i]]
    } else {
      dist_list[[i]] <- list(
        dist = .dots[[i]],
        at = at[i-1]
      )
    }
  }
  
  # Use recursion to deal with distributions in pairs
  Reduce(project_fn, dist_list)
}

#' Project Beyond a Survival Distribution with Another
#' (pairwise)
#' 
#' Project survival from a survival distribution using
#' another survival distribution at the specified cutpoint. 
#' Used by project to reduce the list of distributions.
#' 
#' @param dist1 Survival distribution to project from.
#' @param dist2_list A list containing distribution to
#'   project with and the time at which projection begins.
#' @return A `surv_projection` object.
#' @keywords internal
project_fn <- function(dist1, dist2_list) {
  structure(
    list(
      dist1 = dist1,
      dist2 = dist2_list$dist,
      at = dist2_list$at
    ),
    class = c("surv_object", "surv_projection")
  )
}

#' Mix Two or More Survival Distributions
#' 
#' Mix a set of survival distributions using the specified
#' weights.
#' 
#' @param ... Survival distributions to be used in the
#'   mixing.
#' @param dots Used to work around non-standard evaluation.
#' @param weights A vector of weights used in mixing.
#'   
#' @return A `surv_pooled` object.
#' @export
#' 
#' @examples
#' 
#' dist1 <- define_survival(distribution = "exp", rate = .5)
#' dist2 <- define_survival(distribution = "gompertz", rate = .5, shape = 1)
#' pooled_dist <- mix(dist1, dist2, weights = c(0.25, 0.75))
#' 
mix <- function(..., weights = 1) {
  
  dots <- list(...)
  mix_(dots, weights)
}

#' @export
#' @rdname mix
mix_ <- function(dots, weights = 1) {
  
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
    class = c("surv_object", "surv_pooled")
  )
}

#' @export
#' @rdname mix
pool <- function(...) {
  warning("'pool() is deprecated, use 'mix()' instead.")
  mix(...)
}

#' @export
#' @rdname mix
pool_ <- function(...) {
  warning("'pool_() is deprecated, use 'mix_()' instead.")
  mix_(...)
}

#' Apply a Hazard Ratio
#' 
#' Proportional reduce or increase the hazard rate of a
#' distribution.
#' 
#' @param dist A survival distribution.
#' @param hr A hazard ratio to be applied.
#' @param log_hr If `TRUE`, the hazard ratio is exponentiated
#'   before being applied.
#'   
#' @return A `surv_ph` object.
#' @export
#' 
#' @examples
#' 
#' dist1 <- define_survival(distribution = "exp", rate = .25)
#' ph_dist <- apply_hr(dist1, 0.5)
#' 
apply_hr <- function(dist, hr, log_hr = FALSE) {
  
  stopifnot(
    length(hr) == 1,
    is.finite(hr),
    log_hr | hr > 0
  )
  
  structure(
    list(
      dist = dist,
      hr = ifelse(log_hr, exp(hr), hr)
    ),
    class = c("surv_object", "surv_ph")
  )
}

#' Apply an Acceleration Factor
#' 
#' Proportionally increase or reduce the time to event of a
#' survival distribution.
#' 
#' @param dist A survival distribution.
#' @param af An acceleration factor to be applied.
#' @param log_af If `TRUE`, the accleration factor is
#'   exponentiated before being applied.
#'   
#' @return A `surv_aft` object.
#' @export
#' 
#' @examples
#' 
#' dist1 <- define_survival(distribution = "exp", rate = .25)
#' aft_dist <- apply_af(dist1, 1.5)
apply_af <- function(dist, af, log_af = FALSE) {
  
  stopifnot(
    length(af) == 1,
    is.finite(af),
    log_af | af > 0
  )
  
  structure(
    list(
      dist = dist,
      af = ifelse(log_af, exp(af), af)
    ),
    class = c("surv_object", "surv_aft")
  )
}

#' Apply an Odds Ratio
#' 
#' Proportionally increase or reduce the odds of an event of
#' a survival distribution.
#' 
#' @param dist A survival distribution.
#' @param or An odds ratio to be applied.
#' @param log_or If `TRUE`, the odds ratio is exponentiated
#'   before being applied.
#'   
#' @return A `surv_po` object.
#' @export
#' 
#' @examples
#' 
#' dist1 <- define_survival(distribution = "exp", rate = .25)
#' po_dist <- apply_or(dist1, 1.2)
apply_or = function(dist, or, log_or = FALSE) {
  
  stopifnot(
    length(or) == 1,
    is.finite(or),
    log_or | or > 0
  )
  
  structure(
    list(
      dist = dist,
      or = ifelse(log_or, exp(or), or)
    ),
    class = c("surv_object", "surv_po")
  )
}

#' Add Hazards
#' 
#' Get a survival distribution reflecting the independent
#' hazards from two or more survival distributions.
#' 
#' @param ... Survival distributions to be used in the
#'   projection.
#' @param dots Used to work around non-standard evaluation.
#'   
#' @return A `surv_add_haz` object.
#' @export
#' 
#' @examples
#' 
#' dist1 <- define_survival(distribution = "exp", rate = .125)
#' dist2 <- define_survival(distribution = "weibull", shape = 1.2, scale = 50)
#' combined_dist <- add_hazards(dist1, dist2)
#' 
add_hazards <- function(...) {
  
  dots <- list(...)
  
  add_hazards_(dots)
}

#' @export
#' @rdname add_hazards
add_hazards_ <- function(dots) {
  
  structure(
    list(
      dists = dots
    ),
    class = c("surv_object", "surv_add_haz")
  )
}

#' Set Covariates of a Survival Distribution
#' 
#' Set the covariate levels of a survival model to be 
#' represented in survival projections.
#' 
#' @param dist a survfit or flexsurvreg object
#' @param ... Covariate values representing the group for 
#'   which survival probabilities will be generated when 
#'   evaluated.
#' @param covariates Used to work around non-standard
#'   evaluation.
#' @param data A an optional data frame representing 
#'   multiple sets of covariate values for which survival 
#'   probabilities will be generated. Can be used to 
#'   generate aggregate survival for a heterogenous set of 
#'   subjects.
#'   
#' @return A `surv_model` object.
#' @export
#' 
#' @examples
#' 
#' fs1 <- flexsurv::flexsurvreg(
#'   survival::Surv(rectime, censrec)~group,
#'   data=flexsurv::bc,
#'   dist = "llogis"
#' )
#' good_model <- set_covariates(fs1, group = "Good")
#' cohort <- data.frame(group=c("Good", "Good", "Medium", "Poor"))
#' mixed_model <- set_covariates(fs1, data = cohort)
#' 
set_covariates <- function(dist, ..., data = NULL) {
  covariates <- data.frame(...)
  
  set_covariates_(dist, covariates, data)
}

#' @export
#' @rdname set_covariates
set_covariates_ <- function(dist, covariates, data = NULL) {
  
  data <- rbind(
    covariates,
    data
  )
  
  structure(
    list(
      dist = dist,
      covar = data
    ),
    class = c("surv_object", "surv_model")
  )
}

#' Plot general survival models
#' 
#' @param x A survival object of class `surv_object`.
#' @param time Times for which to predict.
#' @param type Either `prob`, for transition probabilities,
#'   or `surv`, for survival.
#' @param ... Additional arguments to pass to [compute_surv()].
#'   
#' @details The function currently only highlights join
#'   points that are at the top level; that is, for objects
#'   with class `surv_projection`.
#'   
#'   To avoid plotting the join points, set join_size to a
#'   negative number.
#'   
#' @return A [ggplot2::ggplot()] object.
#' @export
#' 
plot.surv_object <- function(x, time, type = c("surv", "prob"), ...) {
  type <- match.arg(type)
  y_ax_label <- c(surv = "Survival", prob = "Probability")[type]
  tab_res <- data.frame(
    time = time,
    y = compute_surv(x, time = time, type = type, ...))
  
  ggplot2::ggplot(
    tab_res, ggplot2::aes_string(x = "time", y = "y")) + 
    ggplot2::geom_line() + 
    ggplot2::scale_x_continuous(name = "Time") + 
    ggplot2::scale_y_continuous(name = y_ax_label)
}
