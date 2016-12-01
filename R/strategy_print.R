#' @export
print.uneval_model <- function(x, ...) {
  n_states <- get_state_number(get_states(x))
  n_state_values <- length(get_state_value_names(get_states(x)))
  
  cat(sprintf(
    "A Markov model strategy:

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
#' @param x Result from \code{\link{run_model}}.
#' @param type Type of plot, see details.
#' @param strategy Name or position of model(s) of interest.
#' @param states Names of states to be included in
#'   the plot.
#' @param panels Should plots be faceted by model, by
#'   value or by state?
#' @param values Names of values to be plotted. These can be
#'   any of the costs or effects defined in states.
#' @param free_y Should y limits be free between panels?
#' @param bw Black & white plot for publications?
#' @param ... Additional arguments passed to \code{plot}.
#'   
#' \code{type = "counts"} represents state
#' memberships (corrected) by cycle, \code{type = "ce"}
#' plots models on the cost-efficiency plane with the
#' efficiency frontier, and \code{type = "values"} state
#' values per cycle.
#' 
#' @return A \code{ggplot2} object.
#' 
#' @example inst/examples/example_plot.run_model.R
#' 
#' @export
plot.run_model <- function(x, type = c("counts", "ce", "values"),
                           panels = c("by_strategy", "by_state", "by_value"),
                           values = NULL,
                           strategy = NULL, 
                           states = NULL,
                           free_y = FALSE,
                           bw = FALSE,
                           ...) {
  type <- match.arg(type)
  panels <- match.arg(panels)
  
  scales <- if (free_y) "free_y" else "fixed"
  
  switch(
    type,
    counts = {
      if (panels == "by_strategy") {
        colour_var <- "state_names"
        colour_lab <- "State"
        panel_var <- ".strategy_names"
      } else if (panels == "by_state") {
        colour_var <- ".strategy_names"
        colour_lab <- "Strategy"
        panel_var <- "state_names"
      } else {
        stop("'panels' arguement not compatible.")
      }
      
      tab <- get_counts(x)
      
      if (! is.null(states)) {
        if (any(pb <- ! states %in% get_state_names(x))) {
          stop(sprintf(
            "Some state do not exist: %s.",
            paste(states[pb], collapse = ", ")
          ))
        }
        tab <- tab[tab$state_names %in% states, ]
      }
      
      if (! is.null(strategy)) {
        strategy <- check_strategy_index(x, strategy,
                                         allow_multiple = TRUE)
        
        tab <- tab[tab$.strategy_names %in% strategy, ]
      }
      
      
      pos_cycle <- pretty(sort(unique(tab$markov_cycle)),
                          n = min(max(tab$markov_cycle), 10))
      res <- ggplot2::ggplot(
        tab,
        ggplot2::aes_string(x = "markov_cycle", y = "count",
                            colour = colour_var)) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::facet_grid(stats::as.formula(paste(panel_var, "~ .")),
                            scales = scales) +
        ggplot2::ylim(0, NA) +
        ggplot2::xlab("Markov cycle") +
        ggplot2::ylab("Count") +
        ggplot2::scale_x_continuous(breaks = pos_cycle) +
        ggplot2::scale_colour_hue(name = colour_lab)
      
      if (bw) {
        res <- res +
          ggplot2::scale_color_grey(start = 0, end = .8,
                                    name = colour_lab) +
          theme_pub_bw()
      }
      
      res
    },
    ce = {
      tab_ce <- scale(x)
      ef <- get_frontier(get_model_results(x))
      
      ggplot2::ggplot(tab_ce,
                      ggplot2::aes_string(
                        x = ".effect",
                        y = ".cost",
                        label = ".strategy_names")) +
        ggplot2::geom_line(data = tab_ce[tab_ce$.strategy_names %in% ef, ]) +
        ggplot2::geom_label() +
        ggplot2::xlab("Effect") +
        ggplot2::ylab("Cost")
    },
    values = {
      
      if (panels == "by_strategy") {
        colour_var <- "value_names"
        colour_lab <- "Value"
        panel_var <- ".strategy_names"
      } else if (panels == "by_value") {
        colour_var <- ".strategy_names"
        colour_lab <- "Strategy"
        panel_var <- "value_names"
      } else {
        stop("'panels' arguement not compatible.")
      }
      
      tab <- get_values(x)
      
      if (! is.null(values)) {
        if (any(pb <- ! values %in% get_state_value_names(x))) {
          stop(sprintf(
            "Some state do not exist: %s.",
            paste(values[pb], collapse = ", ")
          ))
        }
        tab <- tab[tab$value_names %in% values, ]
      }
      
      if (! is.null(strategy)) {
        strategy <- check_strategy_index(x, strategy,
                                         allow_multiple = TRUE)
        
        tab <- tab[tab$.strategy_names %in% strategy, ]
      }
      
      
      pos_cycle <- pretty(sort(unique(tab$markov_cycle)),
                          n = min(max(tab$markov_cycle), 10))
      
      res <- ggplot2::ggplot(
        tab,
        ggplot2::aes_string(x = "markov_cycle", y = "value",
                            colour = colour_var)) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::facet_grid(as.formula(paste(panel_var, "~ .")),
                            scales = scales) +
        ggplot2::ylim(0, NA) +
        ggplot2::xlab("Markov cycle") +
        ggplot2::ylab("Value") +
        ggplot2::scale_x_continuous(breaks = pos_cycle) +
        ggplot2::scale_colour_hue(name = colour_lab)
      
      if (bw) {
        res <- res +
          ggplot2::scale_color_grey(start = 0, end = .8,
                                    name = colour_lab) +
          theme_pub_bw()
      }
      
      res
    },
    stop(sprintf("Unknown plot type: '%s'.", type))
  )
}
