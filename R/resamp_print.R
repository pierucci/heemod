#' Plot Results of Probabilistic Analysis
#' 
#' Various plots for Markov models probabilistic analysis.
#' 
#' \code{type = "ac"} plots cost-effectiveness acceptability
#' curves, \code{type = "ce"} plots results on the 
#' cost-efficiency plane.
#' 
#' @param x Result from \code{\link{run_model}}.
#' @param type Type of plot, see details.
#' @param values Values for CEAC.
#' @param ... Additional arguments passed to \code{plot}.
#'   
#' @return A \code{ggplot2} object.
#' @export
#' 
plot.psa <- function(x, type = c("ce", "ac"),
                     values = seq(0, 1e5, 1e3), ...) {
  type <- match.arg(type)
  
  switch(
    type,
    ce = {
      tab <- scale(x)
      ggplot2::ggplot(data = tab,
                      ggplot2::aes_string(
                        x = ".effect",
                        y = ".cost",
                        colour = ".strategy_names")) +
        ggplot2::geom_point() +
        ggplot2::scale_colour_hue(name = "Model") +
        ggplot2::xlab("Incremental effect") +
        ggplot2::ylab("Incremental cost")
    },
    ac = {
      tab <- acceptability_curve(x$psa, values)
      ggplot2::ggplot(tab, 
                      ggplot2::aes_string(
                        x = ".ceac",
                        y = ".p",
                        colour = ".strategy_names")) +
        ggplot2::geom_line() +
        ggplot2::ylim(0, 1) +
        ggplot2::scale_colour_hue(name = "Strategy") +
        ggplot2::xlab("Willingness to pay") +
        ggplot2::ylab("Probability of cost-effectiveness")
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
summary.psa <- function(object, ...) {
  res <- object %>% 
    scale() %>% 
    dplyr::select_(~ - .index) %>% 
    dplyr::group_by_(".strategy_names") %>%
    dplyr::summarise_all(mean) %>% 
    as.data.frame()
  
  res_values <- res %>% 
    dplyr::select_(~ - .cost, ~ - .effect)
  res_values <- res_values[! names(res_values) %in%
                             c(object$resamp_par, ".cost", ".effect")]
  
  res_comp <- res %>% 
    compute_icer() %>% 
    as.data.frame()
  
  structure(
    list(
      res_values = res_values,
      res_comp = res_comp,
      total_result = object
    ),
    class = "summary_psa"
  )
}

#' @export
print.summary_psa <- function(x, ...) {
  cat(sprintf(
    "A PSA with %i resamplings.\n\n",
    x$total_result$N
  ))
  
  print_results(x$res_values, x$res_comp)
}

#' @export
print.psa <- function(x, ...) {
  print(summary(x), ...)
}
