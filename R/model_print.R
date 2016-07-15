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
#' \code{type = "counts"} represents state memberships 
#' (corrected) by cycle, \code{type = "ce"} plots models on
#' the cost-efficiency plane with the efficiency frontier.
#' 
#' @param x Result from \code{\link{run_models}}.
#' @param type Type of plot, see details.
#' @param model Name or position of model of interest.
#' @param ... Additional arguments passed to \code{plot}.
#'   
#' @return A \code{ggplot2} object.
#' @export
#' 
plot.eval_model_list <- function(x, type = c("counts", "ce"), model = 1, ...) {
  type <- match.arg(type)
  
  switch(
    type,
    counts = {
      tab_counts <- dplyr::mutate(
        get_counts(attr(x, "eval_model_list")[[model]]),
        markov_cycle = row_number()
      )
      pos_cycle <- pretty(seq_len(nrow(tab_counts)), n = min(nrow(tab_counts), 10))
      tab_counts <- tidyr::gather(data = tab_counts, ... = - markov_cycle)
      
      y_max <- max(attr(x, "init"), tab_counts$value)
      ggplot2::ggplot(tab_counts, ggplot2::aes(markov_cycle, value, colour = key)) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::scale_x_continuous(breaks = pos_cycle) +
        ggplot2::xlab("Markov cycle") +
        ggplot2::ylab("Count") +
        ggplot2::scale_colour_hue(name = "State") +
        ggplot2::ylim(0, y_max)
    },
    ce = {
      tab_ce <- normalize_ce(x)
      ef <- get_frontier(x)
      
      ggplot2::ggplot(tab_ce,
                      ggplot2::aes(x = .effect, y = .cost, label = .model_names)) +
        ggplot2::geom_line(data = tab_ce[tab_ce$.model_names %in% ef, ]) +
        ggplot2::geom_point() +
        ggplot2::geom_text(hjust = 1) +
        ggplot2::xlab("Effect") +
        ggplot2::ylab("Cost")
    },
    stop(sprintf("Unknown type: '%s'.", type))
  )
}
if(getRversion() >= "2.15.1")
  utils::globalVariables(c("row_number", "markov_cycle", "value", "key",
                           ".cost", ".effect", ".model_names"))
