#' Plot Results of Probabilistic Analysis
#' 
#' Various plots for Markov models probabilistic analysis.
#' 
#' \code{type = "ac"} plots cost-effectiveness acceptability
#' curves, \code{type = "ce"} plots results on the 
#' cost-efficiency plane, \code{type = "cov"} to perform
#' covariance analysis on the results, \code{type = "evpi"}
#' for expected value of perfect information.
#' 
#' @param x Result from \code{\link{run_model}}.
#' @param type Type of plot, see details.
#' @param max_wtp Maximal willingness to pay.
#' @param n Number of CECA points to estimate (values above
#'   100 may take significant time).
#' @param log_scale Show willingness to pay on a log scale?
#' @param bw Black & white plot for publications?
#' @param ... Additional arguments passed to \code{plot}.
#'   
#' @return A \code{ggplot2} object.
#' @export
#' 
plot.psa <- function(x, type = c("ce", "ac", "cov", "evpi"),
                     max_wtp = 1e5,
                     n = 100, log_scale = TRUE,
                     bw = FALSE, ...) {
  type <- match.arg(type)
  
  switch(
    type,
    ce = {
      tab <- scale(x)
      res <- ggplot2::ggplot(data = tab,
                             ggplot2::aes_string(
                               x = ".effect",
                               y = ".cost",
                               colour = ".strategy_names")) +
        ggplot2::geom_point() +
        ggplot2::scale_colour_hue(name = "Model") +
        ggplot2::xlab("Incremental effect") +
        ggplot2::ylab("Incremental cost")
      
      if (bw) {
        res <- res +
          ggplot2::scale_color_grey(start = 0, end = .8,
                                    name = "Strategy") +
          theme_pub_bw()
      }
      
      res
    },
    ac = {
      values <- generate_wtp(max_wtp = max_wtp,
                             n = n, log_scale = log_scale)
      tab <- acceptability_curve(x$psa, values)
      
      res <- ggplot2::ggplot(tab, 
                             ggplot2::aes_string(
                               x = ".ceac",
                               y = ".p",
                               colour = ".strategy_names")) +
        ggplot2::geom_line() +
        ggplot2::ylim(0, 1) +
        ggplot2::scale_colour_hue(name = "Strategy") +
        ggplot2::xlab("Willingness to pay") +
        ggplot2::ylab("Probability of cost-effectiveness")
      
      if (log_scale) {
        res <- res +
          ggplot2::scale_x_log10()
      }
      
      if (bw) {
        res <- res +
          ggplot2::scale_color_grey(start = 0, end = .8,
                                    name = "Strategy") +
          theme_pub_bw()
      }
      
      res
    },
    evpi = {
      values <- generate_wtp(max_wtp = max_wtp,
                             n = n, log_scale = log_scale)
      tab <- compute_evpi(x, values)
      
      res <- ggplot2::ggplot(tab,
                             ggplot2::aes_string(
                               x = ".ceac",
                               y = ".evpi"
                             )) +
        ggplot2::geom_line() +
        ggplot2::xlab("Willingness to pay") +
        ggplot2::ylab("EVPI")
      
      res
    },
    cov = {
      tab <- compute_cov(x) %>% 
        dplyr::mutate_(
          .prop = ~ .prop * 100
        )
      
      ggplot2::ggplot(
        tab,
        ggplot2::aes_string(".par_names", ".prop")) +
        ggplot2::geom_col() +
        ggplot2::facet_grid(.result ~ .strategy_names) +
        ggplot2::xlab("Parameter") +
        ggplot2::ylab("Variance explained (%)") +
        ggplot2::coord_flip()
    },
    stop("Unknown plot type."))
}

#' @rdname heemod_scale
scale.psa <- function(x, center = TRUE, scale = TRUE) {
  .bm <- get_central_strategy(x)
  
  res <- x$psa
  
  if (scale) {
    res <- res %>% 
      dplyr::mutate_(
        .cost = ~ .cost / sum(get_init(get_model(x))),
        .effect = ~ .effect / sum(get_init(get_model(x)))
      )
  }
  
  if (center) {
    res <- res %>% 
      dplyr::group_by_(".index") %>% 
      dplyr::mutate_(
        .cost = ~ (.cost - sum(.cost * (.strategy_names == .bm))),
        .effect = ~ (.effect - sum(.effect * (.strategy_names == .bm)))
      ) %>% 
      dplyr::ungroup()
  }
  
  res
}

#' @export
summary.psa <- function(object, threshold = NULL, ...) {
  summary.run_model(object = object, threshold = threshold, ...)
}

#' @export
print.psa <- function(x, ...) {
  cat(sprintf(
    "A PSA with %i resamplings.\n\n",
    x$N
  ))
  
  print(summary(x), ...)
}
