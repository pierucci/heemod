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
  # gets strategy with lowest icer -> next on frontier
  # filters strat <= next on frontier
  # re-apply function on remaining
  if (stop_frontier(x)) {
    (x %>% 
      dplyr::arrange_(~ .cost) %>% 
      dplyr::filter_(~ .cost == .cost[1]))$.strategy_names
  } else {
    bm <- get_root_strategy(x)
    ebm <- x$.effect[x$.strategy_names == bm]
    cbm <- x$.cost[x$.strategy_names == bm]
    
    x$.effect <- x$.effect - ebm
    x$.cost <- x$.cost - cbm
    
    x <- x %>% 
      dplyr::filter_(~ .effect >= 0) %>% 
      dplyr::mutate_(
        .icer = ~ .cost / .effect
      ) %>% 
      dplyr::arrange_(.dots = list(~.icer, ~ .effect))
    
    enext <- dplyr::slice(x, 1)$.effect
    
    x_res <- x %>% dplyr::filter_(
      substitute(.effect >= enext,
                 list(enext = enext)))
    
    c((dplyr::filter_(x, ~ is.na(.icer)))$.strategy_names,
      get_frontier(x_res))
  }
}

stop_frontier <- function(x) {
  length(unique(x$.effect)) == 1
}
