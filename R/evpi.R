compute_evpi <- function(x, wtp_thresholds) {
  x$psa %>% 
    dplyr::mutate(.key = 1) %>% 
    dplyr::left_join(
      tibble::tibble(
        .ceac = wtp_thresholds,
        .key = 1,
        .strategy_choice = summary(
          x, threshold = wtp_thresholds)$res_nmb_strat
      ),
      by = ".key"
    ) %>% 
    dplyr::group_by(.data$.ceac, .data$.index) %>% 
    dplyr::mutate(
      .nmb = .data$.effect * .data$.ceac - .data$.cost,
      .top_strategy = .data$.nmb == max(.data$.nmb),
      .top_strategy = .data$.top_strategy & cumsum(.data$.top_strategy) == 1,
      .top_choice = .data$.strategy_names == .data$.strategy_choice
      # in case 2 nmb are identical, pick first
    ) %>% 
    dplyr::summarise(
      .evpi = .data$.nmb[.data$.top_strategy] - .data$.nmb[.data$.top_choice]
    ) %>% 
    dplyr::summarise(
      .evpi = mean(.data$.evpi)
    )
}

#' Export PSA Results for SAVI
#' 
#' Export the result of a PSA in a format compatible with 
#' Sheffield Accelerated Value of Information software.
#' 
#' This function saves 3 files at the path given by
#' `folder`: `param.csv`, the parameter values,
#' `cost.csv` and `effect.csv` the cost and effect
#' results.
#' 
#' The official SAVI website can be found at this URL: 
#' http://http://savi.shef.ac.uk/SAVI/
#' 
#' @param x PSA result.
#' @param folder A folder where to save the `csv` files.
#'   
#' @return Nothing. Creates 3 files.
#' @export
export_savi <- function(x, folder = ".") {
  res <- export_psa(x)
  
  write.csv(
    x = res$par,
    file = file.path(folder, "param.csv"),
    row.names = FALSE)
  
  write.csv(
    x = res$c,
    file = file.path(folder, "cost.csv"),
    row.names = FALSE
  )
  
  write.csv(
    x = res$e,
    file = file.path(folder, "effect.csv"),
    row.names = FALSE
  )
}

export_psa <- function(x) {
  res <- x$psa[c(x$resamp_par, ".cost", ".effect", ".strategy_names")] %>% 
    reshape_long(
      key_col = ".key",
      value_col = ".value",
      gather_cols = c(".cost", ".effect")) %>% 
    dplyr::mutate(
      .var_name = paste(.data$.key, .data$.strategy_names, sep = "_")) %>% 
    dplyr::select(-.data$.key, -.data$.strategy_names) %>% 
    reshape_wide(key_col = ".var_name", value_col = ".value")
  
  list(
    par = res[x$resamp_par],
    c = res %>% 
      dplyr::select(dplyr::starts_with(".cost")) %>% 
      stats::setNames(get_strategy_names(x)),
    e = res %>% 
      dplyr::select(dplyr::starts_with(".effect")) %>% 
      stats::setNames(get_strategy_names(x))
  )
}

#' Use the BCEA package
#' 
#' Interfaces the output of [run_psa()] into the BCEA package.
#' 
#' The BCEA package is needed for this function to work.
#' 
#' @param x Output from [run_psa()].
#' @param ... Additional arguemnts passed to [BCEA::bcea()].
#'   
#' @return A BCEA analysis.
#' @export
#' 
run_bcea <- function(x, ...) {
  if (! requireNamespace("BCEA")) {
    stop("'BCEA' package required for BCEA analysis.")
  }
  
  res <- export_psa(x)
  
  BCEA::bcea(
    e = as.matrix(res$e),
    c = as.matrix(res$c),
    interventions = get_strategy_names(x),
    ...
  )
}
