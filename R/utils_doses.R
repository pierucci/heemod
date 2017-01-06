#' Find lowest-cost way to assemble units of different sizes
#' to a certain total
#' 
#' @param totals A vector of total.
#' @param available_units available sizes and costs.
#' @param max_num_units largest number of vials to use.
#' @param max_excess = 0 by how much we can exceed the
#'   requested total.
#' @param only_least_cost = TRUE return only the least-cost
#'   solution?
#' @param subset_col optionally, a column to select on.
#' @param subset_val if subset_col is provided, the value to
#'   select on in that column.
#' @param max_combinations_to_check allows you to override
#'   the limit on number of combinations, at the risk of
#'   taking up lots of memory and computation time.
#'   
#' @return a list, with one element for each total.  Each
#'   element contains a matrix describing combinations that
#'   give the desired total (or totals in the desired range,
#'   if max_excess > 0), along with their costs.  The matrix
#'   will be sorted by cost.   If only a single solution is
#'   returned, for example when \code{only_least_cost =
#'   TRUE}, empty units will be trimmed. In either case, the
#'   final column ("cost") will give the cost, and others
#'   will give the constituent sizes.
#' @export
#' 
#' @examples
#' units <- data.frame(size = c(1000, 250, 200, 100, 50),
#'     cost = c(40, 11.5, 8.5, 5.5, 4.4))
#' compute_cost_partition(450, available_units = units, max_num_units = 6,
#'   max_excess = 0, only_least_cost = FALSE)
#' compute_cost_partition(450, available_units = units, max_num_units = 6,
#'   max_excess = 0, only_least_cost = TRUE)
#' temp <- compute_cost_partition(sample(250:450, 10, replace = TRUE), 
#'    available_units = units, max_num_units = 6,
#'    max_excess = 0, only_least_cost = TRUE)
#' least_cost(temp)
compute_cost_partition <-
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

#' Title Find the total dose, given a scaling parameter 
#' (e.g. find total dose for a 70kg patient for a 2mg/kg
#' drug dosing)
#' 
#' @param doses a vector of doses (e.g. 2, 5, 10)
#' @param dosing_units a vector of units corresponding to
#'   the doses (e.g. "mg/kg", "mg/m2", "mg")
#' @param scaling a vector of scaling numbers (e.g. 70, 85,
#'   1)
#' @param scaling_units a vector of units for the scaling
#'   numbers (e.g. "kg", "m2", "mg")
#'   
#' @return Returns a vector of scale-adjusted doses (e.g.
#' 140mg, 425mg, 10mg)
compute_scaled_doses <- function(doses, dosing_units, scaling, scaling_units) {
  
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
#' @param first if \code{init} is not specified, how many
#'   periods at the beginning should be dosed.  Must be
#'   non-negative.
#' @param then_every if \code{pattern} is not specified,
#'   make it (then_every - 1) zeroes followed by a 1.   If
#'   \code{then_every} has any negative value, there is no
#'   dosing after the initial period (equivalent to setting
#'   pattern = 0).
#' @param cap largest dosing period
#'   
#' @return A logical vector indicating whether or not a
#'   patient is dosed during the relevant period (markov
#'   cycle).
#' @export
#' @details \code{init} takes precedence over \code{first};
#'   that is, if \code{init} is defined, then \code{first}
#'   is ignored.  Similarly, \code{pattern} takes precedence
#'   over \code{then_every}.
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
