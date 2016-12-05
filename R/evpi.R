compute_evpi <- function(x, wtp_thresholds) {
  x$psa %>% 
    dplyr::mutate(.key = 1) %>% 
    dplyr::left_join(
      tibble::tibble(
        .ceac = wtp_thresholds,
        .key = 1,
        .strategy_choice = summary(x, threshold = wtp_thresholds)$res_nmb_strat
      ),
      by = ".key"
    ) %>% 
    dplyr::group_by_(~ .ceac, ~ .index) %>% 
    dplyr::mutate_(
      .nmb = ~ .effect * .ceac - .cost,
      .top_strategy = ~ .nmb == max(.nmb),
      .top_strategy = ~ .top_strategy & cumsum(.top_strategy) == 1,
      .top_choice = ~ .strategy_names == .strategy_choice
      # in case 2 nmb are identical, pick first
    ) %>% 
    dplyr::summarise_(
      .evpi = ~ .nmb[.top_strategy] - .nmb[.top_choice]
    ) %>% 
    dplyr::summarise_(
      .evpi = ~ mean(.evpi)
    )
}

#' Export PSA Results for SAVI
#' 
#' Export the result of a PSA in a format compatible with 
#' Sheffield Accelerated Value of Information sofware.
#' 
#' This function saves 3 files at the path given by
#' \code{folder}: \code{param.csv}, the parameter values,
#' \code{cost.csv} and \code{effect.csv} the cost and effect
#' results.
#' 
#' The official SAVI website can be found at this URL: 
#' http://http://savi.shef.ac.uk/SAVI/
#' 
#' @param x PSA result.
#' @param folder A folder where to save the \code{csv} files.
#'   
#' @return Nothing. Creates 3 files.
#' @export
export_savi <- function(x, folder = "~") {
  res <- x$psa[c(x$resamp_par, ".cost", ".effect", ".strategy_names")] %>% 
    tidyr::gather_(
      key_col = ".key",
      value_col = ".value",
      gather_cols = c(".cost", ".effect")) %>% 
    dplyr::mutate_(
      .var_name = ~ paste(.key, .strategy_names, sep = "_")) %>% 
    dplyr::select_(~ - .key, ~ - .strategy_names) %>% 
    tidyr::spread_(key_col = ".var_name", value_col = ".value")
  
  write.csv(
    x = res[x$resamp_par],
    file = file.path(folder, "param.csv"),
    row.names = FALSE)
  
  write.csv(
    x = res %>% 
      dplyr::select(dplyr::starts_with(".cost")) %>% 
      stats::setNames(get_strategy_names(x)),
    file = file.path(folder, "cost.csv"),
    row.names = FALSE
  )
  
  write.csv(
    x = res %>% 
      dplyr::select(dplyr::starts_with(".effect")) %>% 
      stats::setNames(get_strategy_names(x)),
    file = file.path(folder, "effect.csv"),
    row.names = FALSE
  )
}
