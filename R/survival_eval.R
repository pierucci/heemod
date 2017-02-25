#' Check Cycle and Time Inputs
#' 
#' Performs checks on the cycle and time inputs to `eval_surv`
#' 
#' @param cycle The `markov_cycle` or 
#'   `state_time` for which to predict.
#' @param cycle_length The value of a Markov cycle in 
#'   absolute time units.
#'   
#' @keywords internal
#' 
check_cycle_inputs <- function(cycle, cycle_length) {
    
  stopifnot(
    all(cycle == seq(from = min(cycle), to = max(cycle), by = 1)),
    all(round(cycle, 0) == cycle),
    length(cycle) >= 1,
    !any(cycle < 0),
    !any(is.infinite(cycle_length)),
    !any(is.na(cycle))
  )
  
}

#' Extract Evaluated Parameters
#' 
#' Extracts the covariate-adjusted parameters from a flexsurv object.
#' 
#' @param obj A flexsurv object.
#' @param data An optional dataset of covariate values to generate parameters
#' for.  Defaults to the original data to which the model was fit.
#'   
#' @return A tidy data frame of curve parameters for each covariate level.
#'   
#' @keywords internal
#' 
extractParams = function(obj, data = NULL) {
  
  # Use data from object if not given
  if (is.null(data)) data <- obj$data$m %>%
      dplyr::select(-1, -ncol(obj$data$m))
  else {
    # Apply factor levels of original data
    for(i in colnames(data)) {
      if (is.character(data[[i]]) | is.factor(data[[i]])) {
        data[[i]] <- factor(data[[i]], levels = levels(obj$data$m[[i]]))
      }
    }
  }
  
  # Grab parameter estimates
  coef <- obj$coefficients
  nCoef <- length(coef)
  
  if (obj$ncovs == 0) {
    # Null model, extract parameter estimates
    outParams <- obj$res %>%
      as.data.frame %>%
      t %>%
      as.data.frame %>%
      head(1)
    rownames(outParams) <- NULL
  }
  else {
    # Get parameters of distribution
    parNames <- obj$dlist$pars
    names(parNames) <- parNames
    nPars <- length(parNames)
    # Replicate matrix of coefficents, row = obs, col = param
    nObs <- nrow(data)
    coefMat <- coef %>% 
      rep(nObs) %>%
      matrix(ncol <- nCoef, nrow = nObs, byrow = TRUE)
    names(coefMat) <- parNames
    # Preallocate a matrix to hold calculated parameters
    parMat <- matrix(ncol = nPars, nrow = nObs)
    # Loop to compute covariate-adjusted parmaeters
    for(i in seq_len(nPars)) {
      # Extract inverse transformation
      invTrans <- obj$dlist$inv.transforms[[i]]
      # Subset coefficients relevant to parameter
      coefSelector <- c(i, obj$mx[[parNames[i]]] + nPars)
      nParCoefs <- length(coefSelector)
      parCoefMat <- coefMat[ , coefSelector]
      if (nParCoefs > 1) {
        # Assemble model matrix
        form <- obj$all.formulae[[parNames[i]]][-2] %>%
          formula
        mm <- stats::model.matrix(form, data = data)
        # Multiply cells of model matrix by cells of coefficient matrix and get row sums
        parMat[ , i] <- (mm * parCoefMat) %>%
          rowSums %>%
          invTrans
      }else {
        parMat[ , i] <- parCoefMat %>% invTrans
      }
    }
    outParams <- parMat  %>%
      as.data.frame()
    colnames(outParams) <- parNames
  }
  return(outParams)
}

#' Extract Product-Limit Table for a Stratum
#' 
#' Extracts the product-limit table from a survfit object for a given stratum.
#' Only `survfit` and unstratified `survfit.coxph` objects are supported.
#' 
#' @param sf A survit object.
#' @param index The index number of the strata to extract.
#'   
#' @return A data.frame of the product-limit table for the given stratum.
#'   
#' @keywords internal
#' 
extractStratum = function(sf, index) {
  if(is.null(sf$strata)) {
    # If there is no stratification, get the full table
    selector <- seq_len(length(sf$time))
    values <- list()
  }
  else{
    # If there are strata, create a selector which selects only the rows
    # corresponding to the given index
    endIndex <- sum(sf$strata[seq_len(index)])
    startIndex = 1 + endIndex - sf$strata[index]
    selector = seq(from = startIndex, to = endIndex)
    
    # Extract the variable names and values corresponding to the stratum
    split_strata = strsplit(names(sf$strata[index]),"(=|, )")[[1]]
    len = length(split_strata) / 2
    keys = split_strata[seq_len(len) * 2 - 1]
    values = split_strata[seq_len(len) * 2]
    names(values) = keys
  }
  
  # Return the stratum's product-limit table
  argList = as.list(values) %>% append(
    list(
      time = c(0, sf$time[selector]),
      n = sum(sf$n.censor[selector] + sf$n.event[selector]),
      nrisk = c(sum(sf$n.censor[selector] + sf$n.event[selector]), sf$n.risk[selector]),
      ncensor = c(0, sf$n.censor[selector]),
      nevent = c(0, sf$n.event[selector]),
      surv = c(1, sf$surv[selector]),
      lower = c(1, sf$lower[selector]),
      upper = c(1, sf$upper[selector])
    )
  )
  return(do.call(data_frame, argList))
}

#' Extract Product-Limit Tables
#' 
#' Extracts the product-limit table from a survfit object for all strata.
#' Only `survfit` and unstratified `survfit.coxph` objects are supported.
#' 
#' @param sf A survit object.
#'   
#' @return A tidy data.frame of the product-limit tables for all strata.
#'   
#' @keywords internal
#' 
extractStrata = function(sf) {
  if(is.null(sf$strata)) extractStratum(sf, 1)
  else plyr::ldply(
    seq_len(length(sf$strata)),
    function(i) extractStratum(sf, i)
  )
}

#' Calculate Probability of Event
#' 
#' Calculates the per-cycle event probabilities from a vector of survival
#' probabilities.
#' 
#' @param x A vector of conditional event probabilities.
#'   
#' @return The per-cycle event probabilities.
#'   
#' @keywords internal
#' 
calc_prob_from_surv = function(x) - diff(x) / x[-length(x)]

#' Calculate Probability of Survival
#' 
#' Calculates the probability of survival from a vector of event
#' probabilities
#' 
#' @param x A vector of per-cycle event probabilities.
#'   
#' @return The survival probabilities.
#'   
#' @keywords internal
#' 
calc_surv_from_prob = function(x) cumprod(x[1] - x[-1])


#' Evaluate Survival Distributions
#' 
#' Generate either survival probabilities or conditional probabilities of event
#' for each model cycle.
#' 
#' The results of `eval_surv` are memoised for
#' `options("heemod.memotime")` (default: 1 hour) to
#' increase resampling performance.
#' 
#' @name eval_surv
#' @param x A survival distribution object
#' @param cycle The `markov_cycle` or 
#'   `state_time` for which to predict.
#' @param cycle_length The value of a Markov cycle in 
#'   absolute time units.
#' @param type either `prob`, for transition 
#'   probabilities, or `surv`, for survival
#' @param ... arguments passed to methods.
#'   
#' @return Returns either the survival probalities or conditional probabilities
#' of event for each cycle.
#'   
#' @export
eval_surv_ <- function(x, cycle, cycle_length=1,
                       type=c("prob","surv"), ...) {
  UseMethod("eval_surv_")
}

#' @rdname eval_surv
#' @export
eval_surv <- memoise::memoise(
  eval_surv_,
  ~ memoise::timeout(options()$heemod.memotime)
)

#' @rdname eval_surv
#' @export
eval_surv_.survfit <- function(x, cycle, cycle_length = 1,
                               type = c("prob","surv"), ...) {
  
  dots = list(...)
  
  type <- match.arg(type)
  
  if(type == "prob") {
    cycle_ = c(cycle[1] - 1, cycle)
  }else {
    cycle_ = cycle
  }
  
  check_cycle_inputs(cycle_, cycle_length)
  
  t <- cycle_length * cycle_
  
  # Extract the product-limit tables for all strata
  pl_table <- extractStrata(x)
  
  # Identify the terms which separate groups (if any)
  terms <- setdiff(
    colnames(pl_table),
    c("time", "n", "nrisk", "ncensor", "nevent", "surv", "lower", "upper")
  )
  
  # Generate predicted survival for each group
  surv_df = plyr::ddply(
    pl_table,
    terms,
    function(d) {
      maxtime <- max(d$time)
      selector <- (t > maxtime)
      # Use stepfun to look up survival probabilities
      value <- stats::stepfun(d$time[-1], d$surv)(t)
      # Use NA when time > max time
      value[selector] <- as.numeric(NA)
      data_frame(t = t, value = value, n = d$n[1])
    }
  )
  
  if(is.null(dots$covar)) {
    if(length(terms) > 0) {
      warning("No covariates provided, returning aggregate survial across all subjects.")
    }
    # If covariates are not provided, do weighted average for each time.
    agg_df = surv_df %>%
      dplyr::group_by_("t") %>%
      dplyr::summarize_(value = "sum(value * n) / sum(n)")
  }else {
    # If covariates are provided, join the predictions to them and then
    # do simple average for each time.
    agg_df = left_join(dots$covar, surv_df, by = terms) %>%
      dplyr::group_by_("t") %>%
      dplyr::summarize_(value = "mean(value)")
  }
  
  # Get the vector of predictions
  ret = agg_df$value
  
  if (type == "prob") {
    # Calculate per-cycle failure prob
    ret <- calc_prob_from_surv(ret)
  }
  
  return(ret)
}

#' @rdname eval_surv
#' @export
eval_surv_.flexsurvreg <- function(x, cycle, cycle_length = 1,
                                   type = c("prob", "surv"), ...) {
  
  dots = list(...)
  
  type <- match.arg(type)
  
  if(type == "prob") {
    cycle_ = c(cycle[1] - 1, cycle)
  }else {
    cycle_ = cycle
  }
  
  check_cycle_inputs(cycle_, cycle_length)
  
  t <- cycle_length * cycle_
  
  # Extract parameter estimates
  coef <- x$coefficients
  
  nCoef <- length(coef)
  nT <- length(t)
  
  if(x$ncovs > 0 && is.null(dots$covar)) {
    warning("No covariates provided, returning aggregate survial across all subjects.")
  }
  
  # For efficiency, survival probabilities are only calculated
  # for each distinct set of covariates, then merged back onto
  # the full dataset (data_full).
  if (is.null(dots$covar)) {
    # if covar is not provided, use the
    # original model.frame
    data_full = x$data$m %>%
      dplyr::select(-1, -ncol(x$data$m))
    data = dplyr::distinct(data_full)
  }
  else {
    # Use covar if provided
    data_full <- dots$covar
    data <- dplyr::distinct(dots$covar)
  }
  
  # If there is no data, make an empty df
  if (ncol(data) == 0) data <- data.frame(value = numeric(nT))
  
  # Get a data frame of parameter values for each observation
  paramDf <- extractParams(x, data = data)
  nObs <- nrow(paramDf)
  
  # Repeat rows of parameter df to match number of time points
  paramDf <- paramDf %>%
    dplyr::slice(rep(seq_len(nObs), each = nT))
  
  # Assumble arguments to p<dist> function
  fncall <- list(rep(t, nObs), lower.tail = FALSE) %>%
    append(x$aux) %>%
    append(paramDf)
  
  # Calculate survival probabilities for each distinct level/time,
  surv_df <- data %>%
    dplyr::slice(rep(seq_len(nObs), each = nT)) %>%
    dplyr::mutate(t = rep(t, nObs), value = do.call(x$dfns$p, fncall))
  
  # Join to the full data, then summarize over times.
  if(x$ncovs > 0) {
    surv_df <- surv_df %>%
      dplyr::left_join(data_full, by = colnames(data)) %>%
      dplyr::group_by_("t") %>%
      dplyr::summarize_(value = "mean(value)")
  }

  
  # Just get the results column
  ret <- surv_df$value
  
  if (type == "prob") {
    # Calculate per-cycle failure prob
    ret <- calc_prob_from_surv(ret)
  }
  
  return(ret)
}

#' @rdname eval_surv
#' @export
eval_surv_.surv_model <- function(x, cycle, cycle_length = 1,
                                  type = c("prob", "surv"), ...) {
  eval_surv_(
    x$dist,
    cycle = cycle,
    cycle_length = cycle_length,
    covar = x$covar,
    type = type,
    ...
  )
  
}

#' @rdname eval_surv
#' @export
eval_surv_.surv_projection <- function(x, cycle,
                                       cycle_length = 1,
                                       type = c("prob", "surv"), ...) {
  
  type <- match.arg(type)
  
  if(type == "prob") {
    cycle_ = c(cycle[1] - 1, cycle)
  }else {
    cycle_ = cycle
  }
  
  check_cycle_inputs(cycle_, cycle_length)
  
  t <- cycle_length * cycle_
  
  ret <- numeric(length(cycle_))
  
  surv1 <- eval_surv_(
    x$dist1,
    cycle = cycle_,
    cycle_length = cycle_length,
    type = "surv"
  )
  surv2 <- eval_surv_(
    x$dist2,
    cycle=cycle_,
    cycle_length=cycle_length,
    type="surv"
  )
  
  ind_s1 <- t < x$at
  ind_s2 <- t >= x$at
  
  surv1_p_at <- eval_surv_(x$dist1, cycle = 1, cycle_length = x$at, type = "surv", .internal = TRUE)
  surv2_p_at <- eval_surv_(x$dist2, cycle = 1, cycle_length = x$at, type = "surv", .internal = TRUE)
  
  ret[ind_s1] <- surv1[ind_s1]
  ret[ind_s2] <- (surv2 * surv1_p_at / surv2_p_at)[ind_s2]
  
  if(type == "prob") {
    ret <- calc_prob_from_surv(ret)
  }
  
  ret
}

#' @rdname eval_surv
#' @export
eval_surv_.surv_pooled <- function(x, cycle,
                                       cycle_length = 1,
                                       type = c("prob", "surv"), ...) {
  
  type <- match.arg(type)
  
  if(type == "prob") {
    cycle_ = c(cycle[1] - 1, cycle)
  }else {
    cycle_ = cycle
  }
  
  check_cycle_inputs(cycle_, cycle_length)
  
  # Determine dimensions of matrix and initialize
  n_cycle <- length(cycle_)
  n_dist <- length(x$dists)
  surv_mat <- matrix(nrow = n_cycle, ncol = n_dist)

  # Evaluate and weight component distributions into columns
  # of matrix
  for(i in seq_len(n_dist)) {
    surv_mat[ ,i] <- x$weights[i] / sum(x$weights) * eval_surv_(
      x$dists[[i]],
      cycle=cycle_,
      cycle_length=cycle_length,
      type="surv"
    )
  }
  
  # Calculate weighted average as the row sums
  ret <- rowSums(surv_mat)
  
  if(type == "prob") {
    ret <- calc_prob_from_surv(ret)
  }
  
  ret
}

#' @rdname eval_surv
#' @export
eval_surv_.surv_ph <- function(x, cycle,
                                 cycle_length = 1,
                                 type = c("prob", "surv"), ...) {
  
  type <- match.arg(type)
  
  if(type == "prob") {
    cycle_ = c(cycle[1] - 1, cycle)
  }else {
    cycle_ = cycle
  }
  
  check_cycle_inputs(cycle_, cycle_length)
  
  ret <- eval_surv_(
    x$dist,
    cycle = cycle_,
    cycle_length = cycle_length,
    type = "surv"
  ) ^ x$hr
  
  if(type == "prob") {
    ret <- calc_prob_from_surv(ret)
  }
  
  ret
}

#' @rdname eval_surv
#' @export
eval_surv_.surv_aft <- function(x, cycle,
                               cycle_length = 1,
                               type = c("prob", "surv"), ...) {
  
  type <- match.arg(type)
  
  if(type == "prob") {
    cycle_ = c(cycle[1] - 1, cycle)
  }else {
    cycle_ = cycle
  }
  
  check_cycle_inputs(cycle_, cycle_length)
  
  ret <- eval_surv_(
    x$dist,
    cycle = cycle_,
    cycle_length = cycle_length/x$af,
    type = "surv"
  )
  
  if(type == "prob") {
    ret <- calc_prob_from_surv(ret)
  }
  
  ret
}

#' @rdname eval_surv
#' @export
eval_surv_.surv_po <- function(x, cycle,
                                cycle_length = 1,
                                type = c("prob", "surv"), ...) {
  
  dots = list(...)
  
  type <- match.arg(type)
  
  if(type == "prob") {
    cycle_ = c(cycle[1] - 1, cycle)
  }else {
    cycle_ = cycle
  }
  
  check_cycle_inputs(cycle_, cycle_length)
  
  p <- eval_surv_(
    x$dist,
    cycle = cycle_,
    cycle_length = cycle_length,
    type = "surv"
  )
  
  ret <- 1 / ((((1 - p) / p) * x$or) + 1)
  
  if(type == "prob") {
    ret <- calc_prob_from_surv(ret)
  }
  
  ret
}

#' @rdname eval_surv
#' @export
eval_surv_.surv_add_haz <- function(x, cycle,
                                    cycle_length = 1,
                                    type = c("prob", "surv"), ...) {
  
  type <- match.arg(type)
  
  if(type == "prob") {
    cycle_ = c(cycle[1] - 1, cycle)
  }else {
    cycle_ = cycle
  }
  
  check_cycle_inputs(cycle_, cycle_length)
  
  # Determine dimensions of matrix and initialize
  n_cycle <- length(cycle_)
  n_dist <- length(x$dists)
  surv_mat <- matrix(nrow = n_cycle, ncol = n_dist)
  
  # Evaluate and weight component distributions into columns
  # of matrix
  for(i in seq_len(n_dist)) {
    surv_mat[ ,i] <- eval_surv_(
      x$dists[[i]],
      cycle = cycle_,
      cycle_length = cycle_length,
      type = "surv"
    )
  }
  
  # Apply independent risks
  ret <- apply(surv_mat, 1, function(z) prod(z))
  
  if(type == "prob") {
    ret <- calc_prob_from_surv(ret)
  }
  
  ret
}

#' @rdname eval_surv
#' @export
eval_surv_.surv_dist <- function(x, cycle,
                                 cycle_length = 1,
                                 type = c("prob", "surv"), ...) {
  type <- match.arg(type)
  
  if(type == "prob") {
    cycle_ = c(cycle[1] - 1, cycle)
  }else {
    cycle_ = cycle
  }
  
  check_cycle_inputs(cycle_, cycle_length)
  
  times_surv <- cycle_length * cycle_
  
  if (! requireNamespace("flexsurv")) {
    stop("'flexsurv' package required.")
  }
  
  pf <- get(paste0("p", x$distribution),
            envir = asNamespace("flexsurv"))
  
  args <- x[- match("distribution", names(x))]
  args[["q"]] <- times_surv
  args[["lower.tail"]] <- F
  ret <- do.call(pf, args)
  
  if(type == "prob") {
    ret <- calc_prob_from_surv(ret)
  }
  
  ret
}



