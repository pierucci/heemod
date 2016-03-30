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
                      aes(x = .effect, y = .cost, colour = .model_names)) +
        ggplot2::geom_point() +
        ggplot2::scale_colour_hue(name = "Model") +
        ggplot2::xlab("Effect") +
        ggplot2::ylab("Cost")
    },
    ac = {
      f <- function(.cost, .effect, .ceac, .model_names) {
        t1 <- dplyr::data_frame(
          .cost = .cost,
          .effect = .effect,
          .ceac = .ceac,
          .model_names = .model_names
        )
        .icer <- .cost / .effect
        t1$.icer <- replace(.icer, is.na(.icer), 0)
        t2 <- filter(t1, .effect >= 0 & .icer <= .ceac)
        filter(t2, .effect == max(.effect))$.model_names[1]
      }
      suppressMessages({
        # to optimize
        tab <- normalize_ce(x) %>%
          dplyr::mutate(.key = 1) %>%
          dplyr::left_join(dplyr::data_frame(.ceac = values, .key = 1)) %>%
          dplyr::group_by(.index, .ceac) %>%
          dplyr::summarise(.top = f(.cost, .effect, .ceac, .model_names)) %>%
          dplyr::group_by(.ceac, .top) %>%
          dplyr::summarise(.n = n()) %>%
          dplyr::mutate(.p = .n / sum(.n))
      })
      ggplot2::ggplot(tab, aes(x = .ceac, y = .p, colour = .top)) +
        ggplot2::geom_line() +
        ggplot2::ylim(0, 1) +
        ggplot2::scale_colour_hue(name = "Model") +
        ggplot2::xlab("Willingness to pay") +
        ggplot2::ylab("Probability of cost-effectiveness")
    },
    stop("Unknown plot type."))
}

if(getRversion() >= "2.15.1")
  utils::globalVariables(c(".ceac", ".index", ".effect", ".p", "n",
                           ",cost", ".n", ".key", ".model_name", ".top"))

normalize_ce.probabilistic <- function(x) {
  .bm <- get_base_model(x)
  res <- dplyr::mutate(
    dplyr::group_by(x, .index),
    .cost = .cost - sum(.cost * (.model_names == .bm)),
    .effect = .effect - sum(.effect * (.model_names == .bm))
  )
}
if(getRversion() >= "2.15.1")
  utils::globalVariables(c(".index", ".cost", ".effect"))
