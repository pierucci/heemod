#' @export
print.uneval_model <- function(x, ...) {
  n_states <- get_state_number(get_states(x))
  n_state_values <- length(get_state_value_names(get_states(x)))
  
  cat(sprintf(
    "An unevaluated Markov model:

    %i state%s,
    %i state value%s\n",
    n_states,
    plur(n_states),
    n_state_values,
    plur(n_state_values)
  ))
}

#' Plot Results of a Markov Model
#' 
#' Various plots for Markov models.
#' 
#' @param x Result from \code{\link{run_models}}.
#' @param type Type of plot, see details.
#' @param model Name or position of model of interest.
#' @param include_states Names of states to be included in
#'   the plot.
#' @param panels Should plots be faceted by model or by
#'   value or by state (state for counts only)?
#' @param value Names of values to be plotted.  These can be
#'   any of the costs or effects defined in state files.
#' @param ... Additional arguments passed to \code{plot}.
#'   
#' @details \code{type = "counts"} represents state
#' memberships (corrected) by cycle, \code{type = "ce"}
#' plots models on the cost-efficiency plane with the
#' efficiency frontier.
#' 
#' When \code{type = "count"}, model can be a vector of
#' model numbers or names, or "all".
#' 
#' \code{include_states} and \code{panels} are active only
#' when \code{type = "count"}.   When \code{include_states =
#' character(0)} (the default), all states will be included.
#' 
#' @return A \code{ggplot2} object.
#' 
#' @example inst/examples/example_plot.run_models.R
#' 
#' @export
plot.run_models <- function(x, type = c("counts", "ce", "values"),
                            model = "all", 
                            include_states = character(0), 
                            panels = c("by_model", "by_state", "by_value"),
                            value = NULL,
                            ...){
  type <- match.arg(type)
  panels <- match.arg(panels)
  extra_args <- list(...)
  
  switch(
    type,
    counts = {
      ## for backwards compatibility
      plot.run_models(x, type = "values", model = model,
                      include_states = include_states,
                      panels = panels,
                      value = "count", 
                      ...)
    },
    ce = {
      tab_ce <- normalize_ce(x)
      ef <- get_frontier(x)
      
      ggplot2::ggplot(tab_ce,
                      ggplot2::aes_string(
                        x = ".effect",
                        y = ".cost",
                        label = ".model_names")) +
        ggplot2::geom_line(data = tab_ce[tab_ce$.model_names %in% ef, ]) +
        ggplot2::geom_point() +
        ggplot2::geom_text(hjust = 1) +
        ggplot2::xlab("Effect") +
        ggplot2::ylab("Cost")
    },
    values = {
      if(length(value) > 1 & "count" %in% value)
        stop("count can't be plotted along with other values")
      if((length(value) > 1 || value != "count") & 
         !(panels %in% c("by_model", "by_value")))
        stop("to plot values, panels must be 'by_model' or 'by_value'")
      if((length(value) == 1 && value == "count") & 
         !(panels %in% c("by_model", "by_state")))
        stop("to plot values, panels must be 'by_model' or 'by_state'")
      all_states <- names(attributes(x)$eval_model_list[[1]]$counts)
      model_info <- attr(x, "eval_model_list")
      if(model == "all") model <- 1:length(model_info)
      if(is.character(model))
        model <- match(model, names(model_info))
      if(length(include_states) > 0){
        if(!all(include_states %in% all_states))
          stop("all elements of include_states must be states of the model")
      }
      
      if(length(value) == 1 && value == "count") 
        get_for_plot <- get_counts
      else{
        get_for_plot <- function(...){get_values(...)[, value, drop = FALSE]}
        val_names <- names(get_values(x, 1))
        names_present <- value %in% val_names
        if(!all(names_present))
          stop(paste(value[names_present], "is not one of the values calculated for the model:",
                     paste(val_names, collapse = ", ")))
      }

      ## set up some of the plot parameters we'll need
      title_string <- NULL
      if(panels == "by_model"){
        color_string <- "key"
        facet_string <- ".model ~ ."
        legend_name <- value
        scale_string <- "fixed"
        use_facets <- TRUE
        if(length(model) == 1){
          use_facets <- FALSE
          if(is.character(model)) title_string <- model
          if(is.numeric(model)) title_string <- names(model_info)[model]
        }
      }
      if(panels == "by_value" | panels == "by_state"){
        color_string <- ".model"
        legend_name <- "Model"
        use_facets <- TRUE
        facet_string <- "key ~ ."
        scale_string <- "free_y"
        if(length(include_states) == 1){
          title_string <- include_states
        }
      }
      
      ## prepare the data
      tab_values_pieces <- lapply(names(model_info),
                                  function(this_model) {
                                    dplyr::mutate_(
                                      get_for_plot(x, this_model),
                                      .model = ~ this_model,
                                      markov_cycle = ~ row_number())
                                  })
      tab_values <- do.call("rbind", tab_values_pieces)
      tab_values$.model <- factor(tab_values$.model, levels = names(model_info))
      pos_cycle <- pretty(seq_len(nrow(tab_values)), n = min(nrow(tab_values), 10))
      
      tab_values <- tidyr::gather_(
        data = tab_values,
        key_col = "key",
        value_col = "value",
        gather_cols = setdiff(names(tab_values),
                              c(".model", "markov_cycle")))
      
      if(panels == "by_model"){
        tab_values <- dplyr::filter_(
          tab_values, ~ .model %in% names(model_info)[model]
        )
      }
      if(length(include_states) > 0)
        tab_values <- dplyr::filter_(
          tab_values, ~ key %in% include_states
        )
      
      y_max <- max(tab_values$value)
      this_plot <- ggplot2::ggplot(
        tab_values, 
        ggplot2::aes_string(
          x = "markov_cycle", 
          y = "value", 
          colour = color_string)) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::scale_x_continuous(breaks = pos_cycle) +
        ggplot2::xlab("Markov cycle") +
        ggplot2::ylab(value) +
        ggplot2::scale_colour_hue(name = legend_name) +
        ggplot2::ggtitle(label = title_string)
      
      if(use_facets) 
        this_plot <- this_plot +
        ggplot2::facet_grid(
          facet_string, scales = scale_string
        )
      
      if(scale_string == "fixed") this_plot <- this_plot +
        ggplot2::ylim(0, y_max)
      if("legend.position" %in% names(extra_args))
        this_plot <- this_plot +
        ggplot2::theme(legend.position = extra_args$legend.position)
      this_plot
    },
    stop(sprintf("Unknown type: '%s'.", type))
  )
}
  