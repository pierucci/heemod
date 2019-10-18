#' Acceptability Curve from Probabilistic Analysis
#' 
#' @param x Result from [run_psa()].
#' @param wtp_thresholds willingness to pay thresholds
#'   
#' @return A data frame with columns `.ceac` (the 
#'   cost-effectiveness acceptability threshold), 
#'   `.model` (treatments or models), `.n` (the 
#'   number of cases in which the treatment was most 
#'   cost-effective), and `.p` (the proportion of cases
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
    dplyr::group_by(.index, .ceac) %>% 
    dplyr::mutate(
      .nmb = .effect * .ceac - .cost,
      .top_strategy = .nmb == max(.nmb),
      .top_strategy = .top_strategy & cumsum(.top_strategy) == 1
      # in case 2 nmb are identical, pick first
    ) %>% 
    dplyr::group_by(.ceac, .strategy_names) %>% 
    dplyr::summarise(.n = sum(.top_strategy)) %>% 
    dplyr::group_by(.ceac) %>% 
    dplyr::mutate(.p = .n / sum(.n))
}

generate_wtp <- function(max_wtp,
                         min_wtp = max_wtp / 1000,
                         n, log_scale) {
  stopifnot(
    max_wtp > 0
  )
  if (log_scale) {
    res <- seq(from = log(min_wtp, base = 10),
               to = log(max_wtp, base = 10),
               length.out = n)
    10 ^ res
  } else {
    seq(from = 0, to = max_wtp, length.out = n)
  }
}
