#' @export
print.run_model <- function(x, ...) {
  print(summary(x, ...))
}

#' Summarise Markov Model Results
#' 
#' @param object Output from \code{\link{run_model}}.
#' @param ... additional arguments affecting the summary 
#'   produced.
#'   
#' @return A \code{summary_eval_strategy_list} object.
#' @export
summary.run_model <- function(object, ...) {
  if (! all(c(".cost", ".effect") %in% names(object$run_model))) {
    warning("No cost and/or effect defined, model summary unavailable.")
    return(invisible(NULL))
  }
  
  res_values <- as.data.frame(compute_icer(normalize_ce(object$run_model)))
  rownames(res_values) <- res_values$.strategy_names
  res_values <- dplyr::select_(res_values, ~ - .strategy_names)
  
  
  res_comp <- res_values[c(".dcost", ".deffect", ".icer", ".dref")]
  is.na(res_comp$.icer) <- ! is.finite(res_comp$.icer)
  res_comp$.dcost <- res_comp$.dcost / sum(get_init(object))
  res_comp$.deffect <- res_comp$.deffect / sum(get_init(object))
  
  structure(
    list(
      res_values = dplyr::select_(
        res_values,
        ~ - .cost,
        ~ - .effect,
        ~ - .icer,
        ~ - .dcost,
        ~ - .deffect,
        ~ - .dref
      ),
      res_comp = res_comp,
      cycles = get_cycles(object),
      init = get_init(object),
      method = get_method(object),
      frontier = get_frontier(object)
    ),
    class = "summary_run_model"
  )
}

#' Normalize Cost and Effect
#' 
#' Normalize cost and effect values taking base model as a 
#' reference.
#' 
#' @param x Result of \code{\link{run_model}} or 
#'   \code{\link{run_psa}}.
#'   
#' @return Input with normalized \code{.cost} and 
#'   \code{.effect}, ordered by \code{.effect}.
#'   
#' @keywords internal
normalize_ce <- function(x) {
  UseMethod("normalize_ce")
}
normalize_ce.run_model <- function(x) {
  bm <- get_base_model(x)
  res <- tibble::tibble(
    .strategy_names = get_strategy_names(x)
  )
  re$.cost <- get_cost(x) -
    get_cost(x)[get_strategy_names(x) == bm]
  res$.effect <- get_effect(x) -
    get_effect(x)[get_strategy_names(x) == bm]
  
  res[order(res$.effect), ]
}

#' Compute ICER
#' 
#' Compute ICER for Markov models.
#' 
#' Models are ordered by effectiveness and ICER are computed
#' sequencially.
#' 
#' @param x Result of \code{\link{run_model}}.
#' @param strategy_order Order in which the strategies
#'   should be sorted. Default: by increasing effect.
#'   
#' @return A \code{data.frame} with computed ICER.
#'   
#' @keywords internal
compute_icer <- function(x, strategy_order = order(get_effect(x))) {
  ef <- get_frontier(x)
  tab <- x[strategy_order, ]
  
  tab$.icer <- NA
  tab$.dcost <- NA
  tab$.deffect <- NA
  tab$.dref <- NA
  
  for (i in seq_len(nrow(tab))) {
    if (i == 1) {
      tab$.icer[i] <- NA
      ref_cost <- tab$.cost[i]
      ref_effect <- tab$.effect[i]
      ref_name <- tab$.strategy_names[i]
      
    } else {
      tab$.dcost[i] <- tab$.cost[i] - ref_cost
      tab$.deffect[i] <- tab$.effect[i] - ref_effect
      tab$.icer[i] <- tab$.dcost[i] / tab$.deffect[i]
      tab$.dref[i] <- ref_name
      
      if (tab$.strategy_names[i] %in% ef) {
        ref_cost <- tab$.cost[i]
        ref_effect <- tab$.effect[i]
        ref_name <- tab$.strategy_names[i]
      }
    }
  }
  tab
}

#' @export
print.summary_run_model <- function(x, ...) {
  cat(sprintf(
    "%i strateg%s run for %i cycle%s.\n\n",
    nrow(x$res),
    plur_y(nrow(x$res)),
    x$cycles,
    plur(x$cycles)
  ))
  cat("Initial state counts:\n\n")
  print(matrix(
    x$init,
    dimnames = list(
      names(x$init),
      "N"
    )
  ))
  cat(sprintf(
    "\nCounting method: '%s'.\n\n", x$method
  ))
  print(x$res_values)
  
  res_comp <- x$res_comp
  res_comp$.icer <- format(res_comp$.icer)
  res_comp$.icer[res_comp$.icer == "NA"] <- "-"
  res_comp <- res_comp[-1, ]
  names(res_comp) <- c("Cost Diff.", "Effect Diff.", "ICER", "Ref.")
  
  if (nrow(x$res) > 1) {
    cat("\nEfficiency frontier:\n\n")
    cat(paste(x$frontier, collapse = " -> "))
    cat("\n\nDifferences:\n\n")
    print(res_comp)
  }
}
