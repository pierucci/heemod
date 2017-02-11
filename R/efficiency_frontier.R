#' Return Efficiency Frontier
#' 
#' @param x An \code{eval_strategy_list} object.
#'   
#' @return A vector of model names on the efficiency
#'   frontier.
#'   
#' @keywords internal
get_frontier <- function(x) {
  # recursive function
  # if  all strat have same effect
  #     or root strat is more effective
  #   return less costly
  # else
  #   find root strat
  #   center on root strat
  #   remove less effective strat
  #   compute icer from root strat
  #
  #   return strats with NaN ICER
  #
  #   next strat on frontier: lowest icer & effect
  #   remove strat less effective than next strat
  #   recursively apply function on result
  
  if (stop_frontier(x)) {
    sort(
      (x %>% 
         dplyr::arrange_(~ .cost) %>% 
         dplyr::filter_(~ .cost == .cost[1]))$.strategy_names)
  } else {
    bm <- get_root_strategy(x)
    ebm <- x$.effect[x$.strategy_names == bm]
    cbm <- x$.cost[x$.strategy_names == bm]
    
    x$.effect <- x$.effect - ebm
    x$.cost <- x$.cost - cbm
    
    x <- x %>% 
      dplyr::filter_(~ .effect >= 0) %>% # not needed in theory
      dplyr::mutate_(
        .icer = ~ .cost / .effect
      ) %>% 
      dplyr::arrange_(.dots = list(~.icer, ~ .effect))
    
    enext <- dplyr::slice(x, 1)$.effect # relies on NaN last sorting
    
    x_res <- x %>% dplyr::filter_(
      substitute(.effect >= enext,
                 list(enext = enext)))
    # 0/0 = NaN = NA
    # x/0 = Inf != NA
    # is.na(.icer) excludes same effect more cost
    c(sort((dplyr::filter_(x, ~ is.na(.icer)))$.strategy_names),
      get_frontier(x_res))
  }
}

stop_frontier <- function(x) {
  length(unique(x$.effect)) == 1 ||
    get_root_strategy(x) %in% 
    (dplyr::filter_(x, ~ .effect == max(.effect)))$.strategy_names
}
