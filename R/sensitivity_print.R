#' Plot Sensitivity Analysis
#' 
#' Plot the results of a sensitivity analysis as a tornado 
#' plot.
#' 
#' Plot type \code{simple} plots variations of single model 
#' costs, while \code{difference} plots cost difference between 
#' the specified model and the reference model.
#' 
#' @param x A result of \code{\link{run_sensitivity}}.
#' @param model Name or index of model to plot.
#' @param type Type of plot (see details).
#' @param widest_on_top logical. Should bars be sorted so widest are on top?
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
      xl <- "\u0394 Cost"
    },
    difference_effect = {
      var_plot <- ".deffect"
      xl <- "\u0394 Effect"
    },
    difference_icer = {
      var_plot <- ".icer"
      xl <- "ICER"
    }
  )
  
  tab <- summary(x) %>% 
    dplyr::left_join(
      model_ref %>%
        dplyr::select_(
          ".model_names",
          .cost_ref = ".cost",
          .effect_ref = ".effect"
        )
    ) %>% 
    dplyr::mutate_(
      .col_cost = quote(ifelse(.cost > .cost_ref, ">",
                               ifelse(.cost == .cost_ref, "=", "<"))),
      .col_effect = quote(ifelse(.effect > .effect_ref, ">",
                                 ifelse(.effect == .effect_ref, "=", "<")))
    ) %>% 
    dplyr::filter_(
      substitute(
        .model_names == model,
        list(model = model)
      )
    ) %>%
    dplyr::arrange_(
      ".par_names", var_plot) %>%
    dplyr::group_by_(".par_names") %>%
    dplyr::mutate_(.hjust = quote(1 - (row_number() - 1)))
  
  if (widest_on_top) {
    tab$.par_names <- stats::reorder(
      tab$.par_names,
      (tab %>% dplyr::group_by_(quote(.par_names)) %>% 
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
    ggplot2::ggtitle(paste("Model:", model)) + 
    ggplot2::xlim(min(tab[[var_plot]]) - l, max(tab[[var_plot]]) + l) +
    ggplot2::geom_text(
      ggplot2::aes_string(
        x = var_plot,
        y = ".par_names",
        label = ".par_value",
        hjust = ".hjust"
      )
    )
  
  
  switch(
    type,
    simple = {
      tab <- process_sensitivity(x, type = type, model = model)
      l <- diff(range(tab$.y)) * .1
      
      if(widest_on_top){
        order_df <- tab %>%
          dplyr::group_by(.variable) %>% 
          dplyr::summarize(width = max(.y) - min(.y))
        
        tab$.variable <- factor(
          tab$.variable, 
          levels = unique(order_df$.variable)[order(order_df$width)]
        )
      }
      plot_title <- paste("Model:", unique(tab$.model_names))
      ggplot2::ggplot(
        tab,
        ggplot2::aes(
          x = .x,
          y = .variable
        )
      ) + 
        ggplot2::geom_segment(
          ggplot2::aes(
            xend = .y,
            yend = .variable,
            colour = .sign
          ),
          size = 5
        ) +
        ggplot2::guides(colour = FALSE) +
        ggplot2::geom_text(
          ggplot2::aes(
            x = .y,
            y = .variable,
            label = .value,
            hjust = .hjust
          )
        ) +
        ggplot2::xlab("Cost") +
        ggplot2::ylab("Variable") +
        ggplot2::ggtitle(plot_title) + 
        ggplot2::xlim(min(tab$.y) - l, max(tab$.y) + l)
    },
    diff = {
      tab1 <- process_sensitivity(x, type = type, model = model)
      l <- diff(range(tab1$.y)) * .1
      plot_title <- paste(unique(tab1$.model_names), bm, sep = " - ")
      ## order .variable by width of effect, so widest bars
      ##   of tornado plot will be on top
      if(widest_on_top){
        order_df <- tab1 %>% 
          dplyr::group_by(.variable) %>% 
          dplyr::summarize(width = max(.y) - min(.y))
        
        tab1$.variable <- factor(
          tab1$.variable, 
          levels = unique(order_df$.variable)[order(order_df$width)]
        )
      }
      ggplot2::ggplot(
        tab1,
        ggplot2::aes(
          x = .ref,
          y = .variable
        )
      ) +
        ggplot2::geom_segment(
          ggplot2::aes(
            xend = .y,
            yend = .variable,
            colour = .sign
          ),
          size = 5) +
        ggplot2::guides(colour = FALSE) +
        ggplot2::geom_text(
          ggplot2::aes(
            x = .y,
            y = .variable,
            label = .value,
            hjust = .hjust
          )
        ) +
        ggplot2::xlab("Difference in Cost") +
        ggplot2::ylab("Variable") +
        ggplot2::ggtitle(plot_title) + 
        ggplot2::xlim(min(tab1$.y) - l, max(tab1$.y) + l)
      
    },
    stop("Unknown type.")
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
  object %>% 
    dplyr::rowwise() %>% 
    dplyr::do(heemod:::get_total_state_values(.$.mod)) %>% 
    dplyr::bind_cols(x %>% dplyr::select_(quote(- .mod))) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by_(quote(.par_names), quote(.par_value)) %>% 
    dplyr::mutate_(.dots = attr(x, "model_ref") %>% attr("ce")) %>% 
    dplyr::do(heemod:::compute_icer(., model_order = order(attr(x, "model_ref")$.effect))) %>% 
    dplyr::select_(".model_names", ".par_names", ".par_value",
                   ".cost", ".effect", ".dcost", ".deffect", ".icer") %>% 
    structure(., class = c("summary_sensitivity", class(.)),
              sensitivity = object)
}
