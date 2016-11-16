#' Acceptability Curve from Probabilistic Analysis
#' 
#' @param x Result from \code{\link{run_psa}}.
#' @param wtp_thresholds willingness to pay thresholds
#'   
#' @return A data frame with columns \code{.ceac} (the 
#'   cost-effectiveness acceptability threshold), 
#'   \code{.model} (treatments or models), \code{.n} (the 
#'   number of cases in which the treatment was most 
#'   cost-effective), and \code{.p} (the proportion of cases
#'   where the treatment was most effective).
#'   
#' @keywords internal
acceptability_curve <- function(x, wtp_thresholds) {
  x %>% 
    dplyr::mutate(.key = 1) %>% 
    dplyr::left_join(
      tibble::tibble(
        .ceac = wtp_thresholds,
        .key = 1
      ),
      by = ".key"
    ) %>% 
    dplyr::group_by_(~ .index, ~ .ceac) %>% 
    dplyr::mutate_(
      .nmb = ~ .effect * .ceac - .cost,
      .top_strategy = ~ .nmb == max(.nmb),
      .top_strategy = ~ .top_strategy & cumsum(.top_strategy) == 1
      # in case 2 nmb are identical, pick first
    ) %>% 
    dplyr::group_by_(~ .ceac, ~ .strategy_names) %>% 
    dplyr::summarise_(.n = ~ sum(.top_strategy)) %>% 
    dplyr::group_by_(~ .ceac) %>% 
    dplyr::mutate_(.p = ~ .n / sum(.n))
}
