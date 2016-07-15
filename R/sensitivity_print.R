#' Plot Sensitivity Analysis
#' 
#' Plot the results of a sensitivity analysis as a tornado 
#' plot.
#' 
#' Plot type \code{simple} plots variations of single model 
#' costs, while \code{diff} plots cost difference between 
#' the specified model and the reference model.
#' 
#' @param x A result of \code{\link{run_sensitivity}}.
#' @param model Name or index of model to plot.
#' @param type Type of plot (see details).
#' @param widest_on_top Logical. Should bars be sorted so widest are on top?
#' @param ... Additional arguments passed to \code{plot}.
#'   
#' @return A \code{ggplot2} object.
#' @export
#' 
plot.eval_sensitivity <- function(x, type = c("simple", "diff"),
                                  model = 1, widest_on_top = TRUE, ...) {
  type <- match.arg(type)
  
  n_ind <- sum(attr(attr(x, "model_ref"), "init"))
  
  if(length(model) != 1) stop("Argumemt 'model' must have length 1.")  
  
  switch(
    type,
    simple = {
      tab <- get_model(x, model)
      ref <- get_model(attr(x, "model_ref"), model)
      
      tab <- tidyr::gather_(
        data = tab,
        key_col = ".variable",
        value_col = ".value",
        gather_cols = attr(x, "variables"),
        na.rm = TRUE
      )
      
      tab$.y <- (tab$.cost - ref$.cost) / n_ind
      tab$.sign <- sign(tab$.y)
      tab$.sign <- as.factor(replace(tab$.sign, tab$.sign == -1, 0))
      tab$.x <- 0
      tab <- tab %>%
        dplyr::arrange(.variable, dplyr::desc(.sign), dplyr::desc(.value)) %>%
        dplyr::group_by(.variable) %>%
        dplyr::mutate(.hjust = row_number() - 1)
      
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
      bm <- get_base_model(attr(x, "model_ref"))
      
      tab0 <- get_model(x, bm)
      tab1 <- get_model(x, model)
      ref0 <- get_model(attr(x, "model_ref"), bm)
      ref1 <- get_model(attr(x, "model_ref"), model)
      if(identical(tab0, tab1)) 
        stop("It doesn't make sense to plot the difference of a model with itself.")      
      tab0 <- tidyr::gather_(
        data = tab0,
        key_col = ".variable",
        value_col = ".value",
        gather_cols = attr(x, "variables"),
        na.rm = TRUE
      )
      tab1 <- tidyr::gather_(
        data = tab1,
        key_col = ".variable",
        value_col = ".value",
        gather_cols = attr(x, "variables"),
        na.rm = TRUE
      )
      
      tab1$.y <- (tab1$.cost - tab0$.cost) / n_ind
      tab1$.ref <- (ref1$.cost - ref0$.cost) / n_ind
      tab1$.sign <- -sign(tab1$.y-tab1$.ref)
      tab1$.sign <- as.factor(replace(tab1$.sign, tab1$.sign == -1, 0))
      
      tab1 <- tab1 %>%
        dplyr::arrange(.variable, dplyr::desc(.sign), dplyr::desc(.value)) %>%
        dplyr::group_by(.variable) %>%
        dplyr::mutate(.hjust = 1 - (row_number() - 1))
      
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
if(getRversion() >= "2.15.1") utils::globalVariables(
  c(".variable", ".x", ".y", ".ref", ".value", ".sign", ".hjust")
)
