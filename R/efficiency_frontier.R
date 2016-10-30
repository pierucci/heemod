#' Return Efficiency Frontier
#' 
#' @param x An \code{eval_model_list} object.
#'   
#' @return A vector of model names on the efficiency
#'   frontier.
#'   
#' @keywords internal
get_frontier <- function(x) {
  base_model <- get_base_model(x)
  
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
      
      res <- (dplyr::slice(x, 1))$.model_names
      effect_res <- x$.effect[x$.model_names == res]
      x_res <- x %>% dplyr::filter_(~ .effect > effect_res)
      
      c(res, f(x_res))
    }
  }
  
  c(base_model,
    f(x %>% dplyr::filter_(~ .model_names != base_model)))
}
