#' Allow assignment of a cost at a particular cycle or age
#'
#' @param cost cost, in units when incurred
#' @param age age at a given cycle
#' @param cycle the cycle
#' @param at_age at what age should the cost be incurred
#' @param at_cycle in what cycle should cost be incurred?
#' @param use_age should age be used? (If not, cycle is used.)
#'
#' @return  0 except when the age or cycle condition is met; 
#'   \code{cost} when the condition is met.
#' @export
#'
#' @details intended for use in parameters object 
#'   for \code{heemod} models.
#' 
#' @examples
#' cost_at_right_time(500, age = 1:20, at_age = 10, use_age = TRUE)
#' cost_at_right_time(500, cycle = 1:20, at_cycle = 10, use_age = FALSE)
cost_at_right_time <-
  function(cost, age, cycle, at_age, at_cycle, use_age){
    
    if(!use_age){
      if(missing(cycle) | missing(at_cycle))
        stop("when use_age = FALSE, must supply cycle and at_cycle")
      return(cost * (cycle == at_cycle))
    }
    if(use_age){
      if(missing(age) | missing(at_age))
        stop("when use_age = TRUE, must supply age and at_age")
      
      return(cost * (age == at_age))
    }
  }


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


#' Model piecewise linear decline in vaccine efficacy
#' or other variables
#'
#' @param marked_cycles cycles at which effectiveness
#'   fractions are specified.
#' @param marked_values effectiveness fractions
#'   at the corresponding cycles.
#' @param init_value a point at (0, init_value) will be
#'  added
#' @param change_per_cycle can be specified instead of
#'   \code{marked_cycles} and \code{marked_times}.   If both
#'   are specified, \code{marked_cycles} and \code{marked_values} 
#'   take precedence.
#' @param max_change_cycles used with change_per_cycle
#' @param age_start_change_per_cycle at what age should the decline in
#'   efficacy start (if not specified, there's no threshold).
#' @param cycle_length used to calculate age from cycles if necessary
#' @param cycles The cycles for which values should be returned.  
#'   If \code{NULL}, the default, a function is returned.
#' @param age_at_cycle_0 used in conjunction with
#'   \code{age_start_change_per_cycle}.
#' @param min_val minimum value the interpolation can take on.
#'
#' @return a function that calculates the decline in efficacy
#'   based on cycle and, if specified, patient age at the
#'   start of a simulation.
#' @details intended for use in parameters object 
#'   for \code{heemod} models.
#' @export
#'
#' @examples
#' linear_interpolation(c(1, 10, 20), c(1, .5, .25))
#' linear_interpolation(change_per_cycle = -0.02, max_change_cycles = 50)
#' linear_interpolation(change_per_cycle = -0.02, max_change_cycles = 100,
#'    min_val = -0.4)
#' linear_interpolation(age_start_change_per_cycle = 65, age_at_cycle_0 = 60,
#'   change_per_cycle = -0.05, cycles = 1:15, max_change_cycles = 5)
#' 
linear_interpolation <- function(marked_cycles,
                                 marked_values,
                                 init_value = 1,
                                 change_per_cycle,
                                 max_change_cycles,
                                 age_start_change_per_cycle = NULL,
                                 cycle_length = 1,
                                 cycles = NULL,
                                 age_at_cycle_0,
                                 min_val = -Inf){
  
  if(missing(marked_cycles) & missing(marked_values)){
    if(missing(change_per_cycle))
      stop(paste("must specify either marked_cycles and marked_values",
                 "or change_per_cycle"))
    else{
      marked_cycles <- max_change_cycles
      marked_values <- init_value + max_change_cycles * change_per_cycle
    }
  }
  
  if(is.character(marked_cycles))
    marked_cycles <- eval(parse(text = marked_cycles))
  if(is.character(marked_values))
    marked_values <- eval(parse(text = marked_values))
  
  if(length(marked_cycles) != length(marked_values))
    stop(paste("marked_cycles and marked_values",
               "must have the same length"))
  
  marked_cycles <- c(0, marked_cycles)
  marked_values <- c(init_value, marked_values)
  
  
  
  decline_fn <- stats::approxfun(x = marked_cycles, 
                          y = marked_values, 
                          rule= c(2,2))
  
  if(is.null(age_start_change_per_cycle))
    final_fn <- function(cycle, start_age = NULL) {pmax(decline_fn(cycle), min_val)}
  else
    final_fn <- function(cycle, start_age){
      age_at_time <- cycle * cycle_length + start_age
      ## print(cbind(age_at_time, (age_at_time - age_start_change_per_cycle - cycle_length)/cycle_length))
      pmax(ifelse(age_at_time < age_start_change_per_cycle, 1, 
                  decline_fn((age_at_time - age_start_change_per_cycle - cycle_length)/cycle_length)),
           min_val)
    }
  
  if(is.null(cycles)) return(final_fn)
  else return(final_fn(cycles, age_at_cycle_0))
}

#' Find the total dose, given a scaling parameter
#' (e.g. find total dose for a 70kg patient for a 2mg/kg drug dosing)
#' 
#' @param doses a vector of doses (e.g. 2, 5, 10)
#' @param dosing_units a vector of units corresponding to the doses
#'   (e.g. "mg/kg", "mg/m2", "mg")
#' @param scaling a vector of scaling numbers (e.g. 70, 85, 1)
#' @param scaling_units a vector of units for the scaling numbers
#'   (e.g. "kg", "m2", "mg")
#' 
#' @return
#' Returns a vector of scale-adjusted doses (e.g. 140mg, 425mg, 10mg)
find_scaled_doses <- function(doses, dosing_units, scaling, scaling_units) {
  
  ##argument checks and exception handling
  if(length(doses)!=length(dosing_units))
    stop("length of doses vector does not match that of dosing_units")
  if(length(scaling)!=length(scaling_units))
    stop("length of scaling vector does not match that of scaling_units")
  if(length(doses)!=length(scaling))
    stop("length of doses vectors does not match that of scaling")
  
  #check if units match
  if(!all(scaling_units==sapply(strsplit(as.character(dosing_units),"/"), 
                                utils::tail, 1)))
    stop("scaling_units does not match denominator of dosing_units")
  
  #return results
  res = doses*scaling
  res
}

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

#' Find lowest-cost way to assemble units of different sizes to a certain total
#'
#' @param totals the sum required.
#' @param available_units available sizes and costs.
#' @param max_num_units largest number of vials to use.
#' @param max_excess = 0 by how much we can exceed the requested total.
#' @param only_least_cost = TRUE return only the least-cost solution?
#' @param subset_col optionally, a column to select on.
#' @param subset_val if subset_col is provided, the value to select on in that column.
#' @param max_combinations_to_check allows you to override the limit on number
#'   of combinations, at the risk of taking up lots of memory and computation time.
#'
#' @return a list, with one element for each total.  Each element contains
#'   a matrix describing combinations that give the desired total
#'   (or totals in the desired range, if max_excess > 0), along with
#'   their costs.  The matrix will be sorted by cost.   If only a single
#'   solution is returned, for example when \code{only_least_cost = TRUE},
#'    empty units will be trimmed.
#'   In either case, the final column ("cost") will give the cost,
#'   and others will give the constituent sizes.
#' @export
#'
#' @examples
#' units <- data.frame(size = c(1000, 250, 200, 100, 50),
#'     cost = c(40, 11.5, 8.5, 5.5, 4.4))
#' find_least_cost_partition(450, available_units = units, max_num_units = 6,
#'   max_excess = 0, only_least_cost = FALSE)
#' find_least_cost_partition(450, available_units = units, max_num_units = 6,
#'   max_excess = 0, only_least_cost = TRUE)
#' temp <- find_least_cost_partition(sample(250:450, 10, replace = TRUE), 
#'    available_units = units, max_num_units = 6,
#'    max_excess = 0, only_least_cost = TRUE)
#' least_cost(temp)
find_least_cost_partition <-
  function(totals,
           available_units,
           max_num_units,
           max_excess = totals - 1,
           only_least_cost = TRUE,
           subset_col = NULL,
           subset_val = NULL,
           max_combinations_to_check = 10 ^ 5) {
    ## input checking
    if (is.null(subset_col) + is.null(subset_val) == 1)
      stop("subset_col and subset_val should either both be NULL, or both be set")
    if (!is.null(subset_col) & !is.null(subset_val)) {
      if (length(subset_col) != 1)
        stop("must specify exactly one column to select on")
      if (length(subset_val) != 1)
        stop("must specify exactly one value to select")
      if (!(subset_col %in% names(available_units)))
        stop(paste(subset_col, "is not a column of available_units"))
      if (!(subset_val %in% available_units[, subset_col]))
        stop(paste(
          "column",
          subset_col,
          "does not contain the value",
          subset_val
        ))
      available_units <-
        available_units[available_units[[subset_col]] == subset_val, , drop = FALSE]
    }
    
    available_units <- available_units[, c("size", "cost")]
    available_units <- rbind(available_units,
                             data.frame(size = 0, cost = 0))
    arg_list <- vector("list", length = max_num_units)
    for (i in 1:max_num_units)
      arg_list[[i]] <- 1:nrow(available_units)
    if (nrow(available_units) ^ max_num_units > max_combinations_to_check)
      stop(
        "too many combinations to check; change argument max_combinations_to_check to override"
      )
    combinations <- as.matrix(do.call("expand.grid", arg_list))
    
    possibilities <-
      matrix(available_units[as.vector(combinations), "size"],
             nrow = nrow(combinations),
             ncol = ncol(combinations))
    
    
    if (length(max_excess) < length(totals))
      max_excess <- rep(max_excess, length.out = length(totals))
    
    ## do the calculation for unique values; 
    ##   later we'll reconstitute
    orig_totals <- totals
    which_unique <- !duplicated(totals)
    totals <- totals[which_unique]
    max_excess <- max_excess[which_unique]
    
    combinations_by_total <-
      lapply(seq(along = totals), function(i) {
        ## take only combinations that add up to the right total
        ##   (or range)
        this_total <- totals[i]
        this_max_excess <- max_excess[i]
        
        these_sums <- rowSums(possibilities)
        if (this_max_excess == 0)
          size_cond <- these_sums == this_total
        else{
          size_cond <- (these_sums >= this_total) &
            (these_sums <= this_total + this_max_excess)
        }
        subset(combinations, size_cond)
      })
    
    combinations_by_total <-
      lapply(combinations_by_total, function(combinations) {
        ## because the initial set doesn't distinguish order,
        ##  we end up with duplicates.  Eliminate them.
        combinations <-
          t(apply(combinations, 1, sort, decreasing = TRUE))
        combinations <-
          combinations[!duplicated(combinations), , drop = FALSE]
        
        pieces <-
          matrix(
            available_units[as.vector(combinations), "size"],
            nrow = nrow(combinations),
            ncol = ncol(combinations)
          )
        if (nrow(pieces) > 1)
          pieces <- t(apply(pieces, 1, sort, decreasing = TRUE))
        costs <-
          rowSums(matrix(
            available_units[as.vector(combinations), "cost"],
            nrow = nrow(combinations),
            ncol = ncol(combinations)
          ))
        
        res <- data.frame(pieces, cost = costs)
        res <- res[order(res$cost), , drop = FALSE]
        if (only_least_cost)
          res <- res[1, , drop = FALSE]
        
        if (nrow(res) == 1 & ncol(res) > 1)
          res <- res[, res != 0, drop = FALSE]
        rownames(res) <- NULL
        res
      })
    names(combinations_by_total) <- totals
    
    ## now reconstitute for original totals (if there were duplicates)
    indices <- match(orig_totals, totals)
    combinations_by_total[indices]
  }

#' Get the least cost for the requested doses
#'
#' @param x a set of combinations from \code{\link{find_least_cost_partition}}.
#'
#' @return a data frame with columns \code{total} and \code{cost}.
#' @export
#'
#' @examples
#' ## see the examples for find_least_cost_partition.
least_cost <- function(x) {
  data.frame(total = as.numeric(names(x)),
             cost = sapply(x, function(x) {
               x[1, "cost"]
             }))
}


#' Cost of administration for an intravenous treatment
#'
#' @param iv_time The infusion time, in units compatible with the costs
#' @param cost_first_unit cost for the first time unit
#' @param cost_addl_units cost for the second time unit
#'
#' @return the cost
#'
#' @examples
#' cost_iv_administration(0.5, 100, 20) # = 50
#' cost_iv_administration(1.5, 100, 20) # = 110
cost_iv_administration <- 
  function(iv_time, cost_first_unit, cost_addl_units){
    pmin(iv_time, 1) * cost_first_unit + 
      pmax(iv_time - 1, 0) * cost_addl_units
    
  }


#' Cost of administration for an intravenous treatment
#'
#' @param data_table a data frame; see details
#' @param compound the name of the compound
#'
#' @details `data_table` must have columns `compound`, `param`,
#'   and `value`.   The required values are found in `data_table`
#'   using [look_up()].
#' @return  the cost
#' @export
#'
#' @examples
#' exampleParams <- 
#'   data.frame(compound = "X", 
#'   param = c("cost_admin_first_hr", "cost_admin_addl_hr",
#'             "iv_time_hr"),
#'   value = c(100, 20, 1.5))
#'   cost_iv_compound_administration(exampleParams, "X")
cost_iv_compound_administration <- 
  function(data_table, compound,
           time_col = "iv_time_hr",
           first_cost_col = "cost_admin_first_hr",
           addl_cost_col = "cost_admin_addl_hr")
    {
    stopifnot(all(c("compound", "param", "value") %in%
                names(data_table)))
    if(!(compound %in% unique(data_table$compound)))
      stop(compound, 
           " not present in 'compound' column of data_table")
    all_cols <- c(time_col, first_cost_col, addl_cost_col)
    cols_present <- all_cols %in% unique(data_table$param)
    if(!all(cols_present))
      stop("columns", 
           paste(all_cols[!cols_present], sep = ","),
           "are not in data_table"
      )
  iv_time <- 
    look_up(data_table, compound = compound, 
            param = time_col, value = "value"
            )
  cost_first_unit <- 
    look_up(data_table, compound = compound, 
            param = first_cost_col, value = "value"
            )
  cost_addl_units <- 
    look_up(data_table, compound = compound, 
            param = addl_cost_col, value = "value"
            )
  cost_iv_administration(iv_time, cost_first_unit, cost_addl_units)
}

