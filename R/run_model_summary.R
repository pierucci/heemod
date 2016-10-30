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
#' @return A \code{summary_eval_model_list} object.
#' @export
summary.run_model <- function(object, ...) {
  if (! all(c(".cost", ".effect") %in% names(object))) {
    warning("No cost and/or effect defined, model summary unavailable.")
    return(invisible(NULL))
  }
  
  res <- as.data.frame(compute_icer(normalize_ce(object)))
  rownames(res) <- res$.model_names
  res <- dplyr::select_(res, ~ - .model_names)
  
  
  res_comp <- res[c(".dcost", ".deffect", ".icer", ".dref")]
  is.na(res_comp$.icer) <- ! is.finite(res_comp$.icer)
  res_comp$.dcost <- res_comp$.dcost / sum(attr(object, "init"))
  res_comp$.deffect <- res_comp$.deffect / sum(attr(object, "init"))
  
  structure(
    list(
      res = dplyr::select_(
        res,
        ~ - .cost,
        ~ - .effect,
        ~ - .icer,
        ~ - .dcost,
        ~ - .deffect
      ),
      res_comp = res_comp,
      cycles = attr(object, "cycles"),
      init = attr(object, "init"),
      method = attr(object, "method"),
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
  x$.cost <- x$.cost - x$.cost[x$.model_names == bm]
  x$.effect <- x$.effect - x$.effect[x$.model_names == bm]
  x[order(x$.effect), ]
}

#' Compute ICER
#' 
#' Compute ICER for Markov models.
#' 
#' Models are ordered by effectiveness and ICER are computed
#' sequencially.
#' 
#' @param x Result of \code{\link{run_model}}.
#' @param model_order Order in which the models should be
#'   sorted. Default: by increasing effect.
#'   
#' @return A \code{data.frame} with computed ICER.
#'   
#' @keywords internal
compute_icer <- function(x, model_order = order(x$.effect)) {
  ef <- get_frontier(x)
  tab <- x[model_order, ]
  
  tab$.icer <- NA
  tab$.dcost <- NA
  tab$.deffect <- NA
  tab$.dref <- NA
  for (i in seq_len(nrow(tab))) {
    if (i == 1) {
      tab$.icer[i] <- -Inf
      ref_cost <- tab$.cost[i]
      ref_effect <- tab$.effect[i]
      ref_name <- tab$.model_names[i]
    } else {
      tab$.dcost[i] <- tab$.cost[i] - ref_cost
      tab$.deffect[i] <- tab$.effect[i] - ref_effect
      tab$.icer[i] <- tab$.dcost[i] / tab$.deffect[i]
      tab$.dref[i] <- ref_name
      if (tab$.model_names[i] %in% ef) {
        ref_cost <- tab$.cost[i]
        ref_effect <- tab$.effect[i]
        ref_name <- tab$.model_names[i]
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
  print(x$res)
  
  res_comp <- x$res_comp
  res_comp$.icer <- format(res_comp$.icer)
  res_comp$.icer[res_comp$.icer == "NA"] <- "-"
  res_comp <- res_comp[-1, ]
  names(res_comp) <- c("\u0394 Cost", "\u0394 Effect", "ICER", "Ref.")
  
  if (nrow(x$res) > 1) {
    cat("\nEfficiency frontier:\n\n")
    cat(paste(x$frontier, collapse = " -> "))
    cat("\n\nModel difference:\n\n")
    print(res_comp)
  }
}
