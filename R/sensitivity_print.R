#' Plot Sensitivity Analysis
#' 
#' Plot the results of a sensitivity analysis as a tornado 
#' plot.
#' 
#' Plot type \code{simple} plots variations of single model 
#' costs, while \code{difference} plots cost difference
#' between the specified model and the reference model.
#' 
#' @param x A result of \code{\link{run_dsa}}.
#' @param model Name or index of model to plot.
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
                     model = 1, widest_on_top = TRUE, ...) {
  type <- match.arg(type)
  result <- match.arg(result)
  
  n_ind <- sum(attr(attr(x, "model_ref"), "init"))
  
  model_ref <- attr(x, "model_ref")
  
  check_model_index(x = model_ref, i = model)
  
  if (is.numeric(model)) {
    model <- get_model_names(model_ref)[model]
  }
  
  if (type == "simple" & result == "icer") {
    stop("Result ICER can conly be computed with type = 'difference'.")
  }
  
  if (type == "difference" & model == get_lowest_model(model_ref)) {
    stop("type = 'difference' cannot be computed for base model.")
  }
  
  if (type == "simple") {
    main_title <- paste("Model:", model)
  } else {
    main_title <- paste("Model:", model, "vs.",
                        get_base_model(model_ref))
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
  
  suppressMessages({
    tab <- summary(x) %>% 
      dplyr::left_join(
        model_ref %>%
          dplyr::bind_cols(summary(model_ref)$res_comp) %>% 
          dplyr::select_(
            ".model_names",
            .cost_ref = ".cost",
            .effect_ref = ".effect",
            .dcost_ref = ".dcost",
            .deffect_ref = ".deffect",
            .icer_ref = ".icer"
          )
      ) %>% 
      dplyr::mutate_(
        .dcost = ~ .dcost / n_ind,
        .deffect = ~ .deffect / n_ind,
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
        ~ .model_names == model
      ) %>%
      dplyr::arrange_(
        ".par_names", var_plot) %>%
      dplyr::group_by_(".par_names") %>%
      dplyr::mutate_(.hjust = ~ 1 - (row_number() - 1))
  })
  
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
    ggplot2::ggtitle(main_title) + 
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
  
  n_ind <- if (scale) sum(get_init(get_model(x))) else 1
  
  if (center) {
    x$dsa %>% 
      dplyr::group_by_(~ .par_names, ~ .par_value) %>% 
      dplyr::mutate_(
        .cost = ~ (.cost - sum(.cost * (.strategy_names == .bm))) / n_ind,
        .effect = ~ (.effect - sum(.effect * (.strategy_names == .bm))) / n_ind
      ) %>% 
      dplyr::ungroup()
  } else {
    x$dsa
  }
}

#' @export
summary.dsa <- function(object, ...) {
  res <- object %>% 
    scale() %>% 
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
