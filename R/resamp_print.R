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
      tab <- normalize_ce(x)
      ggplot2::ggplot(data = tab,
                      ggplot2::aes_string(
                        x = ".effect",
                        y = ".cost",
                        colour = ".model_names")) +
        ggplot2::geom_point() +
        ggplot2::scale_colour_hue(name = "Model") +
        ggplot2::xlab("Incremental effect") +
        ggplot2::ylab("Incremental cost")
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

normalize_ce.psa <- function(x) {
  .bm <- get_base_model(x)
  
  n_ind <- sum(get_init(attr(x, "model")))
  
  x %>% 
    dplyr::group_by_(".index") %>% 
    dplyr::mutate_(
      .cost = ~ (.cost - sum(.cost * (.model_names == .bm))) / n_ind,
      .effect = ~ (.effect - sum(.effect * (.model_names == .bm))) / n_ind
    )
}

#' @export
summary.psa <- function(object, ...) {
  res <- object %>% 
    dplyr::group_by_(".index") %>% 
    dplyr::do_(~ compute_icer(.)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select_(~ - .dref, ~ - .index) %>%
    dplyr::group_by_(".model_names") %>%
    dplyr::summarise_all(
      mean
    )
  
  structure(
    list(
      average = res,
      total = object
    ),
    class = "summary_psa"
  )
}

#' @export
print.summary_psa <- function(x, ...) {
  cat(sprintf(
    "A PSA with %i resamplings.\n\n",
    attr(x$total, "N")
  ))
  values <- x$average
  values <- values[! names(values) %in% attr(x$total, "resamp_par")]
  values <- values %>% 
    dplyr::select_(
      ~ - .cost,
      ~ - .effect,
      ~ - .icer,
      ~ - .dcost,
      ~ - .deffect,
      ~ - .model_names
    ) %>% 
    as.matrix()
  
  rownames(values) <- x$average$.model_names
  
  cat("Values:\n\n")
  
  print(values, ...)
  
  if (nrow(x$average) > 1) {
    cat("\nDifferences:\n\n")
    res_comp <- x$average[c(".dcost", ".deffect", ".icer")]
    is.na(res_comp$.icer) <- ! is.finite(res_comp$.icer)
    res_comp$.dcost <- res_comp$.dcost / sum(attr(attr(x$total, "model"), "init"))
    res_comp$.deffect <- res_comp$.deffect / sum(attr(attr(x$total, "model"), "init"))
    res_comp <- as.matrix(res_comp)
    rownames(res_comp) <- x$average$.model_names
    print(pretty_names(res_comp[! is.na(res_comp[, ".icer"]), , drop = FALSE]), ...)
  }
}

#' @export
print.psa <- function(x, ...) {
  print(summary(x), ...)
}
