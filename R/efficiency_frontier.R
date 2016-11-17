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
  f <- function(x) {
    if (nrow(x) == 0) {
      NULL
    } else {
      x <- dplyr::mutate_(x, .icer = ~ .cost / .effect) %>% 
        dplyr::arrange_(.dots = list(~.icer, ~ .effect))
      
      bm <- (dplyr::slice(x, 1))$.strategy_names
      ebm <- x$.effect[x$.strategy_names == bm]
      
      x_res <- x %>% dplyr::filter_(
        substitute(.effect > ebm,
                   list(ebm = ebm)))
      
      c((dplyr::slice(x, 1))$.strategy_names, f(x_res))
    }
  }
  
  bm <- get_root_strategy(x)
  ebm <- x$.effect[x$.strategy_names == bm]
  
  c(bm, f(x %>% dplyr::filter_(
    substitute(
      .strategy_names != bm & .effect > ebm,
      env = list(bm = bm, ebm = ebm)
    ))))
}
