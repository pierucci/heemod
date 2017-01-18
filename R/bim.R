


#' Calculate the total cost for a strategy
#'
#' @param trt_weights vector, length of treatments
#' @param trt_costs matrix of costs - one row per treatment,
#'    one column per cost
#' @param cost_included vector, length of costs - should each
#'    cost be included
#'
#' @return  total cost for a strategy
#'
#' @details This function calculates the total cost for a given strategy.
#'   A strategy includes the proportion of different treatments, along
#'   with their costs.  This function also includes the ability to turn
#'   on or off different cost components.
#' 
#' @examples
single_strategy_cost <- function(trt_weights, trt_costs, cost_included){
  cost_included <- as.numeric(cost_included)
  if(! sort(unique(cost_included)) == 0:1)
    stop("cost_included must be all 0's and 1's (or TRUE and FALSE)")
  stopifnot(length(trt_weights) == nrow(trt_costs))
  stopifnot(length(cost_included) == ncol(trt_costs))
  included_trt_cost <- trt_costs %*% cost_included
  trt_weight %*% included_trt_cost
}


#' cost of strategy over time
#'
#' @param trt_weights_over_time a matrix, with one row for each strategy
#'   and one column for each time
#' @param trt_costs_over_time an array; first dimension corresponds to strategies,
#'    second dimension to costs, third to time points
#' @param cost_included vector, length of costs
#' @param num_patients number of patients in overall population
#'
#' @return vector of costs over time for the strategy
#' @export 
#'
#' @examples
single_strategy_cost_over_time <- 
  function(trt_weights_over_time, trt_costs_over_time, cost_included, num_patients){
    stopifnot(ncol(trt_weights_over_time) > 0)
    stopifnot(length(dim(trt_costs_over_time)) == 3)
    stopifnot(dim(trt_costs_over_time)[3] > 0)
    stopifnot(ncol(trt_weights_over_time) == dim(trt_costs_over_time)[3])
    stopifnot(nrow(trt_weights_over_time) == dim(trt_costs_over_time)[1])
    stopifnot(length(cost_included) == dim(trt_costs_over_time)[2])
    wt_sums <- colSums(trt_weights_over_time)
    if(length(wrong_sum <- which(wt_sums != 1)) > 0)
      stop(paste("some column of trt_weights_over_time:",
                 paste(wrong_sum, collapse = ", ")))
    num_times <- dim(trt_costs_over_time)[3]
    res <- sapply(1:num_times, function(this_time){
                  single_strategy_cost(trt_weights_over_time[, this_time],
                                trt_costs_over_time[, , this_time],
                                cost_included = cost_included)
                })
    res <- res * num_patients
}



