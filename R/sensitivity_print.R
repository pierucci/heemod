#' Plot Sensitivity Analysis
#' 
#' Plot the results of a sensitivity analysis as a tornado 
#' plot.
#' 
#' Plot type \code{simple} plots variations of single model 
#' costs, while \code{difference} plots cost difference
#' between the specified model and the reference model.
#' 
#' @param x A result of \code{\link{run_sensitivity}}.
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
plot.eval_sensitivity <- function(x, type = c("simple", "difference"),
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
  
  if (type == "difference" & model == get_base_model(model_ref)) {
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
      xl <- "\u0394 Cost"
    },
    difference_effect = {
      var_plot <- ".deffect"
      var_ref <- ".deffect_ref"
      var_col <- ".col_deffect"
      xl <- "\u0394 Effect"
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
print.summary_sensitivity <- function(x, ...) {
  v <- attr(attr(x, "sensitivity"), "variables")
  cat(sprintf("A sensitivity analysis on %i parameters.\n\n",
              length(v)))
  cat(paste(c("Parameters:", v), collapse = "\n  -"))
  cat("\n\n")
  cat("Original results:\n\n")
  model_ref <- attr(x, "sensitivity") %>% attr("model_ref")
  print(model_ref)
  cat("\nSensitivity analysis:\n\n")
  
  rn <- paste0(
    x$.par_names,
    " = ",
    x$.par_value,
    " (",
    x$.model_names,
    ")"
  )
  
  x$.dcost <- x$.dcost / sum(attr(model_ref, "init"))
  x$.deffect <- x$.deffect / sum(attr(model_ref, "init"))
  
  res <- as.matrix(x[, c(".cost", ".effect",
                         ".dcost", ".deffect", ".icer")])
  
  rownames(res) <- rn
  colnames(res) <- c("Cost", "Effect", "\u0394 Cost",
                     "\u0394 Effect", "ICER")
  
  is.na(res) <- ! is.finite(res)
  print(res, na.print = "-")
}

#' @export
print.eval_sensitivity <- function(x, ...) {
  print(summary(x))
}

#' @export
summary.eval_sensitivity <- function(object, ...) {
  res <- object %>% 
    dplyr::rowwise() %>% 
    dplyr::do_(~ get_total_state_values(.$.mod)) %>% 
    dplyr::bind_cols(object %>% dplyr::select_(~ - .mod)) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by_(~ .par_names, ~ .par_value) %>% 
    dplyr::mutate_(.dots = attr(object, "model_ref") %>% attr("ce")) %>% 
    dplyr::do_(~ compute_icer(., model_order = order(attr(object, "model_ref")$.effect))) %>% 
    dplyr::select_(".model_names", ".par_names", ".par_value",
                   ".cost", ".effect", ".dcost", ".deffect", ".icer") %>% 
    dplyr::ungroup()
  
  structure(res, class = c("summary_sensitivity", class(res)),
            sensitivity = object)
}
