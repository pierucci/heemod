#' Calculate utilities based on time before death
#' @param x an object of of type \code{cycle_counts}, 
#'   \code{eval_model}, or \code{run_models}.  x is either
#'   a matrix of counts for from a model.
#'   (of type \code{cycle_counts}) or an \code{eval_model} or
#'   a \code{run_models} list.
#' @param m name or number of a model, only if \code{x} is
#'   of type \code{run_models}.
#' @param death_state the name of the state representing death.
#' @param util_before_death a matrix with two columns, \code{through_cycle} 
#'   and \code{util}.  From \code{through_cycle[i - 1]} (or 1, for i = 1)
#'   to \code{through_cycle[i+1]} before death, 
#'   an individual will accrue \code{util[i]} units of
#'   utility.  
#' @param util_long_before_death utility more than 
#'   \code{max(util_before_death[, through_cycle])} cycles before death.
#' @param discount_rate discount rate to be applied to utilities.   
#' @param ... other arguments to be passed through

#'
#' @return  a vector of utilities, of length \code{nrow(counts)}.
#' @export
#'
#' @examples
#' ProgFree <- round(1000 * exp(-0.2 * 0:24))
#' Progressive <- round((1000 - ProgFree) * exp(-0.1 * 0:24))
#' Death <- 1000 - ProgFree - Progressive
#' state_names <- rep(c("ProgFree", "Progressive", "Death"), each = 25)
#' 
#' counts <- data.frame(.strategy = rep("s1", 25),
#'                      markov_cycle = 0:24,
#'                      state_names = state_names,
#'                      count = c(ProgFree, Progressive, Death)
#' )
#' class(counts) <- c("cycle_counts", class(counts))
#' aa1 <- data.frame(until_lag = c(1,2,3), util = c(0.1, 0.3, 0.5))
#'  utility_by_time_from_death(counts, util_before_death = aa1, 
#'    util_long_before_death = 1)

utility_by_time_from_death <- 
  function(x, ...){
    UseMethod("utility_by_time_from_death")
  }

#' @export
#' @rdname utility_by_time_from_death
utility_by_time_from_death.cycle_counts <- 
  function(x, util_before_death,
           util_long_before_death, death_state = "Death",
           discount_rate = 0, ...){
    counts <- x
    stopifnot(discount_rate >= 0)
    stopifnot(is.character(death_state))
    if(! death_state %in% counts$state_names)
      stop("death_state must be the name of one of the states")
    if(!all(c("until_lag", "util") %in% names(util_before_death)))
      stop("util_before_death must have columns until_lag and util")
    distinct_num_subjects <- 
      dplyr::group_by(counts, markov_cycle) %>%
        dplyr::summarize(., num_subjects = sum(count)) %>%
          dplyr::summarize(., unique_subjects = dplyr::n_distinct(round(num_subjects, 6)))
    stopifnot(distinct_num_subjects ==  1)
    if(any(util_before_death[, "until_lag"] <= 0))
      stop("problem with util_before_death: can't specify values for Markov cycles <= 0")
    if(max(util_before_death[, "until_lag"]) < 1)
      stop("problem with util_before_death:  must specify utility for some cycle > 1")
    
    death_counts <- subset(counts, state_names == death_state)$count
    alive_counts_by_cycle <- 
      dplyr::filter(counts, state_names != death_state) %>%
        dplyr::group_by(., markov_cycle) %>%
          dplyr::summarize(., alive_counts = sum(count))
    
    alive_counts <- alive_counts_by_cycle$alive_counts
    new_deaths <- c(0, diff(death_counts))
    discounted_new_deaths <- 
      discount(new_deaths, discount_rate, first = FALSE) 
    
    util_df <- 
      data.frame(cycles = c(0, util_before_death[, "until_lag"] + 1e-5),
                 util = c(util_before_death[, "util"], 0))
  
    ## utilities associated with different times before death
    util_vec <- look_up(
      util_df,
      cycles = 1:nrow(counts) - 1,
      bin = "cycles",
      value = "util"
    )
    ## filter vector to just count up the people who are
    ##   the specified times before death
    near_death_vec <- rep(1, max(util_before_death[, "until_lag"]) + 1)
    util_vec <- util_vec[1:length(near_death_vec)]
    ## adjust for lag
    util_vec[1] <- near_death_vec[1] <- 0        
    
    
    padding <- rep(0, length(near_death_vec))
    drop_padding <- -c(1:length(near_death_vec))
    num_near_death <- 
      stats::filter(c(padding, rev(new_deaths)), 
                        near_death_vec, sides = 1)
    num_near_death <- rev(num_near_death[drop_padding])

    utils_before_death <- 
      stats::filter(c(padding, rev(discounted_new_deaths)), 
                        util_vec, sides = 1)
    utils_before_death <- 
      rev(utils_before_death[drop_padding])
    earlier_utils <- (alive_counts - num_near_death) * util_long_before_death
    
    utils_before_death + earlier_utils

  }

#' @rdname utility_by_time_from_death
#' @export
utility_by_time_from_death.eval_strategy <- 
  function(x, ...){
    utility_by_time_from_death.cycle_counts(get_counts(x), ...)
  }

#' @rdname utility_by_time_from_death
#' @export
utility_by_time_from_death.run_model <- 
  function(x, m, ...){
    temp <- dplyr::filter(get_counts(x), .strategy_names == m)
    utility_by_time_from_death.cycle_counts(temp, ...)
  }
