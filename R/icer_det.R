
#' Return Efficiency Frontier
#'
#' @param x An \code{eval_model_list} object.
#' @param cost 
#' @param effect 
#' @param ... 
#'
#' @return A vector of model names on the efficiency frontier.
#'
get_frontier <- function(x, cost, effect, ...) {
  base_model <- get_base_model(x, effect = effect)
  
  # remove models less effective than base
  tab_model <- x[x[[effect]] >= x[[effect]][x$.model_name == base_model], ]
  
  tab_icer <- compute_icer(tab_model, cost = cost, effect = effect)
  
  tab_icer <- tab_icer[order(tab_icer$.icer, - tab_icer[[effect]]), ]
  
  # higher ICER must be for better effect
  tab_icer <- tab_icer[tab_icer[[effect]] >= cummax(tab_icer[[effect]]), ]
  
  tab_icer$.model_names[order(tab_icer[[effect]])]
}
