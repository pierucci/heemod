#' Plot Sensitivity Analysis
#' 
#' Plot the results of a sensitivity analysis as a tornado 
#' plot.
#' 
#' Plot type `simple` plots variations of single strategy 
#' values, while `difference` plots incremental values.
#' 
#' @param x A result of [run_dsa()].
#' @param strategy Name or index of strategies to plot.
#' @param type Type of plot (see details).
#' @param result Plot cost, effect, or ICER.
#' @param widest_on_top logical. Should bars be sorted so 
#'   widest are on top?
#' @param limits_by_bars logical. Should the limits used
#'   for each parameter be printed in the plot, next to the
#'   bars?
#' @param resolve_labels logical. Should we resolve all
#'   labels to numbers instead of expressions (if there are
#'   any)?
#' @param shorten_labels logical. Should we shorten the
#'   presentation of the parameters on the plot to highlight
#'   where the values differ?
#' @param bw Black & white plot for publications?
#' @param remove_ns Remove variables that are not sensitive.
#' @param ... Additional arguments passed to `plot`.
#'   
#' @return A `ggplot2` object.
#' @export
#' 
plot.dsa <- function(x, type = c("simple", "difference"),
                     result = c("cost", "effect", "icer"),
                     strategy = NULL, widest_on_top = TRUE,
                     limits_by_bars = TRUE,
                     resolve_labels = FALSE,
                     shorten_labels = FALSE,
                     remove_ns = FALSE,
                     bw = FALSE, ...) {
  
  type <- match.arg(type)
  result <- match.arg(result)
  
  model_ref <- get_model(x)
  
  if (is.null(strategy)) {
    strategy <- get_strategy_names(get_model(x))
  } else {
    
    strategy <- check_strategy_index(x = model_ref, i = strategy,
                                     allow_multiple = TRUE)
  }
  
  if (type == "simple" & result == "icer") {
    stop("Result 'icer' can conly be computed with type = 'difference'.")
  }
  
  if (type == "difference") {
    strategy <- setdiff(strategy, get_noncomparable_strategy(model_ref))
    if (length(strategy) == 0) {
      stop("Cannot plot type = 'difference' for noncomparable strategy.")
    }
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
  
  if (resolve_labels) {
    x$dsa <- x$dsa %>%
      dplyr::mutate_(
        .par_value = ~ .par_value_eval
      )
  }
  
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
      substitute(.strategy_names %in% strategy,
                 list(strategy = strategy))
    ) %>%
    dplyr::arrange_(
      ".par_names", var_plot) %>%
    dplyr::group_by_(~ .par_names, ~ .strategy_names) %>%
    dplyr::mutate_(.hjust = ~ 1 - (row_number() - 1))
  
  if (remove_ns) {
    tab <- tab %>% 
      dplyr::group_by_(".par_names") %>% 
      dplyr::filter_(
        substitute(! all(var_col == "="), list(var_col = as.name(var_col)))
      )
  }
  
  if (widest_on_top) {
    tab$.par_names <- stats::reorder(
      tab$.par_names,
      (tab %>% dplyr::group_by_(~ .par_names) %>% 
         dplyr::mutate_(d = substitute(diff(range(xxx)),
                                       list(xxx = as.name(var_plot)))))$d
    )
  }
  
  l <- diff(range(tab[[var_plot]])) * .1
  
  if (type == "difference") {
    tab$.strategy_names <- "difference"
  }
  
  if (shorten_labels) {
    odds <- seq(from = 1, to = nrow(tab), by = 2)
    evens <- odds + 1
    new_digits <- digits_at_diff(as.numeric(tab$.par_value[odds]), 
                                 as.numeric(tab$.par_value[evens]),
                                 addl_digits = 2)
    tab$.par_value[odds] <- new_digits$x
    tab$.par_value[evens] <- new_digits$y
    
    if (limits_by_bars) l <- l * (1 + 0.075 * max(new_digits$nd))
  }
  
  res <- ggplot2::ggplot(tab, ggplot2::aes_string(
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
    ggplot2::facet_wrap(stats::as.formula("~ .strategy_names"))
  
  if (limits_by_bars) {
    res <- res + 
      ggplot2::geom_text(
        ggplot2::aes_string(
          x = var_plot,
          y = ".par_names",
          label = ".par_value",
          hjust = ".hjust"
        )
      )
  }
  
  if (bw) {
    res <- res +
      ggplot2::scale_color_grey(start = 0, end = .8) +
      theme_pub_bw()
  }
  
  res
}

#' @export
print.summary_dsa <- function(x, ...) {
  v <- x$object$variables
  cat(sprintf("A sensitivity analysis on %i parameters.\n\n",
              length(v)))
  cat(paste(c("Parameters:", v), collapse = "\n  -"))
  cat("\n\nSensitivity analysis:\n\n")
  
  rn <- sprintf(
    "%s, %s = %s",
    x$res_comp$.strategy_names,
    x$res_comp$.par_names,
    x$res_comp$.par_value
  )
  
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

get_central_strategy.dsa <- function(x, ...) {
  get_central_strategy(get_model(x))
}

#' @rdname heemod_scale
scale.dsa <- function(x, center = TRUE, scale = TRUE) {
  .bm <- get_central_strategy(x)
  
  res <- x$dsa
  
  if (scale) {
    res <- res %>% 
      dplyr::mutate_(
        .cost = ~ .cost / .n_indiv,
        .effect = ~ .effect / .n_indiv
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

digits_at_diff <- function(x, y, addl_digits = 1){
  stopifnot(length(x) == length(y))
  diff <- abs(x - y)
  num_digits <- -floor(log(diff, 10)) + addl_digits
  round_x <- 
    sapply(seq(along = x), 
           function(i){round(x[i], num_digits[i])})
  round_y <- 
    sapply(seq(along = y), 
           function(i){round(y[i], num_digits[i])})
  list(x = round_x, y = round_y, nd = num_digits)
}
