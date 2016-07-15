#' @export
print.eval_model_list <- function(x, ...) {
  print(summary(x, ...))
}
#' Summarise Markov Model Results
#' 
#' @param object Output from \code{\link{run_models}}.
#' @param ... additional arguments affecting the summary
#'   produced.
#'   
#' @return A \code{summary_eval_model_list} object.
#' @export
#' 
summary.eval_model_list <- function(object, ...) {
  if (! all(c(".cost", ".effect") %in% names(object))) {
    warning("No cost and/or effect defined, model summary unavailable.")
    return(invisible(NULL))
  }
  
  res <- as.data.frame(compute_icer(normalize_ce(object)))
  
  res <- dplyr::select(res, - .model_names)
  
  rownames(res) <- object$.model_names
  
  res_comp <- res[c(".cost", ".effect", ".icer")]
  is.na(res_comp$.icer) <- ! is.finite(res_comp$.icer)
  res_comp$.icer <- format(res_comp$.icer)
  res_comp$.icer[res_comp$.icer == "NA"] <- "-"
  res_comp$.cost <- res_comp$.cost / sum(attr(object, "init"))
  res_comp$.effect <- res_comp$.effect / sum(attr(object, "init"))
  names(res_comp) <- c("Cost", "Effect", "ICER")
  
  structure(
    list(
      res = dplyr::select(res, - .cost, - .effect, - .icer),
      res_comp = res_comp[-1, ],
      cycles = attr(object, "cycles"),
      init = attr(object, "init"),
      count_args = attr(object, "count_args"),
      frontier = get_frontier(object)
    ),
    class = "summary_eval_model_list"
  )
}
if(getRversion() >= "2.15.1")
  utils::globalVariables(c(".model_names", ".cost", ".effect", ".icer"))

#' Normalize Cost and Effect
#' 
#' Normalize cost and effect values taking base model as a 
#' reference.
#' 
#' @param x Result of \code{\link{run_models}} or
#'   \code{\link{run_probabilistic}}.
#'   
#' @return Input with normalized \code{.cost} and 
#'   \code{.effect}, ordered by \code{.effect}.
normalize_ce <- function(x) {
  UseMethod("normalize_ce")
}
normalize_ce.eval_model_list <- function(x) {
  bm <- get_base_model(x)
  x$.cost <- x$.cost - x$.cost[x$.model_names == bm]
  x$.effect <- x$.effect - x$.effect[x$.model_names == bm]
  x[order(x$.effect), ]
}

#' Compute ICER
#' 
#' Compute ICER for Markov models.
#' 
#' Models are ordered by effectiveness and ICER are computed sequencially.
#' 
#' @param x Result of \code{\link{run_models}}.
#'   
#' @return A \code{data.frame} with computed ICER.
#' @export
#' 
compute_icer <- function(x) {
  tab <- x[order(x$.effect), ]
  
  tab$.icer <- NA
  for (i in seq_len(nrow(tab))) {
    if (i == 1) {
      tab$.icer[i] <- -Inf
    } else {
      tab$.icer[i] <- (tab$.cost[i] - tab$.cost[i-1]) /
        (tab$.effect[i] - tab$.effect[i-1])
    }
  }
  tab
}

#' @export
print.summary_eval_model_list <- function(x, ...) {
  cat(sprintf(
    "%i Markov model%s run for %i cycle%s.\n\n",
    nrow(x$res),
    plur(nrow(x$res)),
    x$cycles,
    plur(x$cycles)
  ))
  cat("Initial states:\n\n")
  print(matrix(
    x$init,
    dimnames = list(
      names(x$init),
      "N"
    )
  ))
  print(x$res)
  
  if (nrow(x$res) > 1) {
    cat("\nEfficiency frontier:\n\n")
    cat(x$frontier)
    cat("\n\nModel difference:\n\n")
    print(x$res_comp)
  }
}
