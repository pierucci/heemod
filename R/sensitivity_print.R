#' Plot Sensitivity Analysis
#' 
#' Plot the results of a sensitivity analysis as a tornado 
#' plot.
#' 
#' Plot type \code{simple} plots variations of single strategy 
#' values, while \code{difference} plots incremental values.
#' 
#' @param x A result of \code{\link{run_dsa}}.
#' @param strategy Name or index of strategy to plot.
#' @param type Type of plot (see details).
#' @param result Plot cost, effect, or ICER.
#' @param widest_on_top logical. Should bars be sorted so
#'   widest are on top?
#' @param ... Additional arguments passed to \code{plot}.
#'   
#' @return A \code{ggplot2} object.
#' @export
#' 
plot.dsa <- function(x, type = c("simple", "difference"),
                     result = c("cost", "effect", "icer"),
                     strategy = 1, widest_on_top = TRUE, ...) {
  type <- match.arg(type)
  result <- match.arg(result)
  
  model_ref <- get_model(x)
  
  strategy <- check_strategy_index(x = model_ref, i = strategy)
  
  if (type == "simple" & result == "icer") {
    stop("Result 'icer' can conly be computed with type = 'difference'.")
  }
  
  if (type == "difference" & strategy == get_frontier(model_ref$run_model)[1]) {
    stop("Type = 'difference' cannot be computed for base strategy.")
  }
  
  switch(
    paste(type, result, sep = "_"),
    simple_cost = {
      var_plot <- ".cost"
      var_ref <- ".cost_ref"
      var_col <- ".col_cost"
      xl <- "Cost"
    },
    simple_effect = {
      var_plot <- ".effect"
      var_ref <- ".effect_ref"
      var_col <- ".col_effect"
      xl <- "Effect"
    },
    difference_cost = {
      var_plot <- ".dcost"
      var_ref <- ".dcost_ref"
      var_col <- ".col_dcost"
      xl <- "Cost Diff."
    },
    difference_effect = {
      var_plot <- ".deffect"
      var_ref <- ".deffect_ref"
      var_col <- ".col_deffect"
      xl <- "Effect Diff."
    },
    difference_icer = {
      var_plot <- ".icer"
      var_ref <- ".icer_ref"
      var_col <- ".col_icer"
      xl <- "ICER"
    }
  )
  
  tab <- summary(x, center = FALSE)$res_comp %>% 
    dplyr::left_join(
      summary(model_ref, center = FALSE)$res_comp %>%
        dplyr::select_(
          ".strategy_names",
          .cost_ref = ".cost",
          .effect_ref = ".effect",
          .dcost_ref = ".dcost",
          .deffect_ref = ".deffect",
          .icer_ref = ".icer"
        ),
      by = ".strategy_names"
    ) %>% 
    dplyr::mutate_(
      .col_cost = ~ ifelse(.cost > .cost_ref, ">",
                           ifelse(.cost == .cost_ref, "=", "<")),
      .col_effect = ~ ifelse(.effect > .effect_ref, ">",
                             ifelse(.effect == .effect_ref, "=", "<")),
      .col_dcost = ~ ifelse(.dcost > .dcost_ref, ">",
                            ifelse(.dcost == .dcost_ref, "=", "<")),
      .col_deffect = ~ ifelse(.deffect > .deffect_ref, ">",
                              ifelse(.deffect == .deffect_ref, "=", "<")),
      .col_icer = ~ ifelse(.icer > .icer_ref, ">",
                           ifelse(.icer == .icer_ref, "=", "<"))
    ) %>% 
    dplyr::filter_(
      substitute(.strategy_names == strategy,
                 list(strategy = strategy))
    ) %>%
    dplyr::arrange_(
      ".par_names", var_plot) %>%
    dplyr::group_by_(".par_names") %>%
    dplyr::mutate_(.hjust = ~ 1 - (row_number() - 1))
  
  if (widest_on_top) {
    tab$.par_names <- stats::reorder(
      tab$.par_names,
      (tab %>% dplyr::group_by_(~ .par_names) %>% 
         dplyr::mutate_(d = substitute(diff(range(xxx)),
                                       list(xxx = as.name(var_plot)))))$d
    )
  }
  
  l <- diff(range(tab[[var_plot]])) * .1
  
  ggplot2::ggplot(tab, ggplot2::aes_string(
    y = ".par_names",
    yend = ".par_names",
    x = var_plot,
    xend = var_ref,
    colour = var_col)) +
    ggplot2::geom_segment(size = 5) +
    ggplot2::guides(colour = FALSE) +
    ggplot2::ylab("Variable") +
    ggplot2::xlab(xl) +
    ggplot2::xlim(min(tab[[var_plot]]) - l, max(tab[[var_plot]]) + l) +
    ggplot2::geom_text(
      ggplot2::aes_string(
        x = var_plot,
        y = ".par_names",
        label = ".par_value",
        hjust = ".hjust"
      )
    )
}

#' @export
print.summary_dsa <- function(x, ...) {
  v <- x$object$variables
  cat(sprintf("A sensitivity analysis on %i parameters.\n\n",
              length(v)))
  cat(paste(c("Parameters:", v), collapse = "\n  -"))
  cat("\n\nSensitivity analysis:\n\n")
  
  rn <- paste(x$res_comp$.strategy_names,
              x$res_comp$.par_names, "=",
              x$res_comp$.par_value)
  
  x <- dplyr::select_(x$res_comp, ~ - .par_names,
                      ~ - .par_value,
                      ~ - .strategy_names)
  x <- pretty_names(x)
  
  res <- as.matrix(x)
  
  rownames(res) <- rn
  print(res, na.print = "-", quote = FALSE)
}

#' @export
print.dsa <- function(x, ...) {
  print(summary(x))
}

#' @rdname heemod_scale
scale.dsa <- function(x, center = TRUE, scale = TRUE) {
  .bm <- get_base_strategy(get_model(x))
  
  res <- x$dsa
  
  if (scale) {
    res <- res %>% 
      dplyr::mutate_(
        .cost = ~ .cost / sum(get_init(get_model(x))),
        .effect = ~ .effect / sum(get_init(get_model(x)))
      )
  }
  
  if (center) {
    res <- res %>% 
      dplyr::group_by_(~ .par_names, ~ .par_value) %>% 
      dplyr::mutate_(
        .cost = ~ .cost - sum(.cost * (.strategy_names == .bm)),
        .effect = ~ .effect - sum(.effect * (.strategy_names == .bm))
      ) %>% 
      dplyr::ungroup()
  }
  res
}

#' @export
summary.dsa <- function(object, ...) {
  res <- object %>% 
    scale(...) %>% 
    dplyr::group_by_(~ .par_names, ~ .par_value)  %>% 
    dplyr::do_(~ compute_icer(
      ., strategy_order = order(get_effect(get_model(object)))
    )) %>% 
    dplyr::ungroup()
  
  structure(
    list(
      res_comp = res,
      object = object
    ),
    class = c("summary_dsa", class(res))
  )
}

tidy_dsa <- function(x) {
  tab <- summary(x)$res_comp
  tab %>% 
    tidyr::gather()
}
