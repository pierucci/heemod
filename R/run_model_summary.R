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
#' @return A \code{summary_run_model} object.
#' @export
summary.run_model <- function(object, ...) {
  if (! all(c(".cost", ".effect") %in% names(object$run_model))) {
    warning("No cost and/or effect defined, model summary unavailable.")
    return(invisible(NULL))
  }
  
  res_values <- object$run_model %>% 
    dplyr::select_(
      ~ - .cost,
      ~ - .effect
    ) %>% 
    as.data.frame()
  
  res_comp <- object %>%
    scale() %>% 
    compute_icer() %>% 
    as.data.frame()
  
  structure(
    list(
      res_values = res_values,
      res_comp = res_comp,
      cycles = get_cycles(object),
      init = get_init(object),
      method = get_method(object),
      frontier = get_frontier(scale(object))
    ),
    class = "summary_run_model"
  )
}

get_cost <- function(x) {
  x$run_model$.cost
}

get_effect <- function(x) {
  x$run_model$.effect
}

#' Normalize Cost and Effect
#' 
#' Normalize cost and effect values taking base model as a 
#' reference.
#' 
#' @param x Result of \code{\link{run_model}} or 
#'   \code{\link{run_psa}}.
#' @param center Center results around base model?
#' @param scale Scale results to individual values?
#'   
#' @return Input with normalized \code{.cost} and 
#'   \code{.effect}, ordered by \code{.effect}.
#'   
#' @keywords internal
scale.run_model <- function(x, center = TRUE, scale = TRUE) {
  bm <- get_base_strategy(x)
  res <- tibble::tibble(
    .strategy_names = get_strategy_names(x),
    .cost = get_cost(x),
    .effect = get_effect(x)
  )
  
  if (center) {
    res$.cost <- res$.cost -
      res$.cost[res$.strategy_names == bm]
    res$.effect <- res$.effect -
      res$.effect[res$.strategy_names == bm]
  }
  
  if (scale) {
    res$.cost = res$.cost / sum(get_init(x))
    res$.effect = res$.effect / sum(get_init(x))
  }
  
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
compute_icer <- function(x, strategy_order = order(x$.effect)) {
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
    nrow(x$res_values),
    plur_y(nrow(x$res_values)),
    x$cycles,
    plur(x$cycles)
  ))
  cat("Initial state counts:\n\n")
  print(matrix(
    get_init(x),
    dimnames = list(
      names(x$init),
      "N"
    )
  ))
  cat(sprintf(
    "\nCounting method: '%s'.\n\n", x$method
  ))
  
  res_values <- x$res_values
  rownames(res_values) <- res_values$.strategy_names
  res_values <- dplyr::select_(res_values, ~ - .strategy_names)
  print(res_values)
  
  if (nrow(x$res_values) > 1) {
    cat("\nEfficiency frontier:\n\n")
    cat(paste(x$frontier, collapse = " -> "))
    cat("\n\nDifferences:\n\n")
    
    res_comp <- x$res_comp
    rownames(res_comp) <- res_comp$.strategy_names
    res_comp <- res_comp %>% 
      dplyr::select_(".dcost", ".deffect",
                     ".icer", ".dref")
    res_comp$.icer <- format(res_comp$.icer)
    res_comp$.icer[res_comp$.icer == "NA"] <- "-"
    res_comp <- res_comp[-1, ]
    res_comp <- pretty_names(res_comp)
    
    print(res_comp)
  }
}
