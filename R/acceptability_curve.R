#' Acceptability Curve from Probabilistic Analysis
#' 
#' 
#' @param x Result from \code{\link{run_sensitivity}}.
#' @param wtp_thresholds willingness to pay thresholds
#'   
#' @return A data frame with columns \code{.ceac} (the 
#'   cost-effectiveness acceptability threshold), 
#'   \code{.top} (treatments or models), \code{.n} (the 
#'   number of cases in which the treatment was most 
#'   cost-effective), and \code{.p} (the proportion of cases
#'   in which the treatment was most effective).
#'   
#' @keywords internal
acceptability_curve <- function(x, wtp_thresholds){
  
  f2 <- function(.effect, .icer, .ceac, .model_names){
    max.effect <- max(.effect[.effect >= 0 & .icer <= .ceac])
    .model_names[.effect == max.effect][1]
  }
  
  suppressMessages({
    part1 <- normalize_ce(x) 
    part1$.icer <- part1$.cost / part1$.effect
    part1$.icer <- replace(part1$.icer, is.na(part1$.icer), 0)
    part1$.key <- 1
    
    with_thresholds  <- part1 %>%  
      dplyr::left_join(
        dplyr::data_frame(.ceac = wtp_thresholds, .key = 1)
      ) 
    
    which_best_by_index_and_thresh <- with_thresholds %>% 
      dplyr::group_by_(".ceac", ".index") %>%
      dplyr::summarize_(
        .model = ~ f2(.effect, .icer, .ceac, .model_names)
      ) 
    
    proportion_best_by_thresh <- which_best_by_index_and_thresh %>%   
      dplyr::group_by_(".ceac", ".model") %>%
      dplyr::summarise_(.n = ~ n()) %>%
      dplyr::mutate_(.p = ~ .n / sum(.n))
  })
  proportion_best_by_thresh
}
