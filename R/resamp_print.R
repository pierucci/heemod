#' Plot Results of Probabilistic Analysis
#' 
#' Various plots for Markov models probabilistic analysis.
#' 
#' \code{type = "ac"} plots cost-effectiveness acceptability
#' curves, \code{type = "ce"} plots results on the 
#' cost-efficiency plane.
#' 
#' @param x Result from \code{\link{run_models}}.
#' @param type Type of plot, see details.
#' @param values Values for CEAC.
#' @param ... Additional arguments passed to \code{plot}.
#'   
#' @return A \code{ggplot2} object.
#' @export
#' 
plot.probabilistic <- function(x, type = c("ce", "ac"),
                               values = seq(0, 1e5, 1e3), ...) {
  type <- match.arg(type)
  
  switch(
    type,
    ce = {
      tab <- normalize_ce(x)
      ggplot2::ggplot(data = tab,
                      ggplot2::aes_string(
                        x = ".effect",
                        y = ".cost",
                        colour = ".model_names")) +
        ggplot2::geom_point() +
        ggplot2::scale_colour_hue(name = "Model") +
        ggplot2::xlab("Effect") +
        ggplot2::ylab("Cost")
    },
    ac = {
      tab <- acceptability_curve(x, values)
      ggplot2::ggplot(tab, 
                      ggplot2::aes_string(
                        x = ".ceac",
                        y = ".p",
                        colour = ".model")) +
        ggplot2::geom_line() +
        ggplot2::ylim(0, 1) +
        ggplot2::scale_colour_hue(name = "Model") +
        ggplot2::xlab("Willingness to pay") +
        ggplot2::ylab("Probability of cost-effectiveness")
    },
    stop("Unknown plot type."))
}

normalize_ce.probabilistic <- function(x) {
  .bm <- get_base_model(x)
  
  x %>% 
    dplyr::group_by_(".index") %>% 
    dplyr::mutate_(
      .cost = ~ .cost - sum(.cost * (.model_names == .bm)),
      .effect = ~ .effect - sum(.effect * (.model_names == .bm))
    )
}
