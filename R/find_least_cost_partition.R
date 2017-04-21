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


