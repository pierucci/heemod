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
#'   state?
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
plot.run_models <- function(x, type = c("counts", "ce"),
                            model = "all", 
                            include_states = character(0), 
                            panels = c("by_model", "by_state"),
                            ...){
  type <- match.arg(type)
  panels <- match.arg(panels)
  extra_args <- list(...)
  
  switch(
    type,
    counts = {
      all_states <- names(attributes(x)$eval_model_list[[1]]$counts)
      model_info <- attr(x, "eval_model_list")
      if(model == "all") model <- 1:length(model_info)
      if(is.character(model))
        model <- match(model, names(model_info))
      if(length(include_states) > 0){
        if(!all(include_states %in% all_states))
          stop("all elements of include_states must be states of the model")
      }
      
      ## set up some of the plot parameters we'll need
      title_string <- NULL
      if(panels == "by_model"){
        color_string <- "key"
        facet_string <- ".model ~ ."
        legend_name <- "State"
        scale_string <- "fixed"
        use_facets <- TRUE
        if(length(model) == 1){
          use_facets <- FALSE
          if(is.character(model)) title_string <- model
          if(is.numeric(model)) title_string <- names(model_info)[model]
        }
      }
      if(panels == "by_state"){
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
      tab_counts_pieces <- lapply(names(model_info),
                                  function(this_model) {
                                    dplyr::mutate_(
                                      get_counts(model_info[[this_model]]),
                                      .model = ~ this_model,
                                      markov_cycle = ~ row_number())
                                  })
      tab_counts <- do.call("rbind", tab_counts_pieces)
      tab_counts$.model <- factor(tab_counts$.model, levels = names(model_info))
      pos_cycle <- pretty(seq_len(nrow(tab_counts)), n = min(nrow(tab_counts), 10))
      
      tab_counts <- tidyr::gather_(
        data = tab_counts,
        key_col = "key",
        value_col = "value",
        gather_cols = setdiff(names(tab_counts),
                              c(".model", "markov_cycle")))
      
      if(panels == "by_model"){
      tab_counts <- dplyr::filter_(
        tab_counts, ~ .model %in% names(model_info)[model]
      )
      }
      if(length(include_states) > 0)
        tab_counts <- dplyr::filter_(
          tab_counts, ~ key %in% include_states
        )
      
      y_max <- max(tab_counts$value)
      this_plot <- ggplot2::ggplot(
        tab_counts, 
        ggplot2::aes_string(
          x = "markov_cycle", 
          y = "value", 
          colour = color_string)) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::scale_x_continuous(breaks = pos_cycle) +
        ggplot2::xlab("Markov cycle") +
        ggplot2::ylab("Count") +
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
    stop(sprintf("Unknown type: '%s'.", type))
  )
}
