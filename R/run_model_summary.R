#' @export
print.run_model <- function(x, ...) {
  print(summary(x, ...))
}

#' Summarise Markov Model Results
#' 
#' @param object Output from [run_model()].
#' @param threshold ICER threshold (possibly several) for
#'   net monetary benefit computation.
#' @param ... additional arguments affecting the summary 
#'   produced.
#'   
#' @return A `summary_run_model` object.
#' @export
summary.run_model <- function(object, threshold = NULL, ...) {
  if (! all(c(".cost", ".effect") %in% names(get_model_results(object)))) {
    warning("No cost and/or effect defined, model summary unavailable.")
    return(invisible(NULL))
  }
  
  res_values <- get_model_results(object) %>% 
    dplyr::select_(
      ~ - .cost,
      ~ - .effect
    ) %>% 
    as.data.frame()
  
  if (! is.null(threshold)) {
    stopifnot(
      all(threshold >= 0),
      ! any(duplicated(threshold))
    )
    
    res_nmb <- object %>% 
      scale.run_model(center = FALSE) %>% 
      dplyr::select_(".strategy_names", ".cost", ".effect") %>% 
      as.data.frame()
    
    res_nmb_strat <- character()
    
    for (tr in threshold) {
      res_nmb[format(tr, digits = 2)] <- res_nmb$.effect * tr -
        res_nmb$.cost
      
      res_nmb_strat <- c(
        res_nmb_strat,
        res_nmb$.strategy_names[res_nmb[format(tr, digits = 2)] ==
                                  max(res_nmb[format(tr, digits = 2)])][1]
      )
    }
    
  } else {
    res_nmb <- NULL
    res_nmb_strat <- NULL
  }
  
  res_comp <- object %>%
    scale.run_model(...) %>% 
    compute_icer() %>% 
    as.data.frame()
  
  structure(
    list(
      res_values = res_values,
      res_comp = res_comp,
      res_nmb = res_nmb,
      res_nmb_strat = res_nmb_strat,
      cycles = get_cycles(object),
      init = get_uneval_init(object),
      method = get_method(object),
      frontier = get_frontier(object)
    ),
    class = "summary_run_model"
  )
}

get_init.summary_run_model <- function(x) {
  x$init
}

get_model_results <- function(x) {
  UseMethod("get_model_results")
}

get_model_results.run_model <- function(x) {
  x$run_model
}

get_cost <- function(x) {
  get_model_results(x)$.cost
}

get_effect <- function(x) {
  get_model_results(x)$.effect
}

#' Normalize Cost and Effect
#' 
#' Normalize cost and effect values taking base model as a 
#' reference.
#' @name heemod_scale
#' @param x Result of [run_model()] or 
#'   [run_psa()].
#' @param center Center results around base model?
#' @param scale Scale results to individual values?
#'   
#' @return Input with normalized `.cost` and 
#'   `.effect`, ordered by `.effect`.
#'   
#' @keywords internal
NULL

#' @rdname heemod_scale
scale.run_model <- function(x, center = TRUE, scale = TRUE) {
  bm <- get_central_strategy(x)
  res <- tibble::tibble(
    .strategy_names = get_strategy_names(x),
    .cost = get_cost(x),
    .effect = get_effect(x),
    .n_indiv = get_n_indiv(x)
  )
  
  if (center) {
    res$.cost <- res$.cost -
      res$.cost[res$.strategy_names == bm]
    res$.effect <- res$.effect -
      res$.effect[res$.strategy_names == bm]
  }
  
  if (scale) {
    res$.cost = res$.cost / res$.n_indiv
    res$.effect = res$.effect / res$.n_indiv
  }
  
  res[order(res$.effect), ]
}

#' Compute ICER
#' 
#' Compute ICER for Markov models.
#' 
#' Models are ordered by effectiveness and ICER are computed
#' sequentially.
#' 
#' @param x Result of [run_model()].
#' @param strategy_order Order in which the strategies 
#'   should be sorted. Default: by increasing effect.
#' @param threshold ICER threshold for net monetary benefit
#'   computation.
#'   
#' @return A `data.frame` with computed ICER.
#'   
#' @keywords internal
compute_icer <- function(x, strategy_order = order(x$.effect),
                         threshold = 3e4) {
  
  stopifnot(length(threshold) == 1)
  
  ef <- get_frontier(x)
  tab <- x[strategy_order, ]
  
  tab$.icer <- NA
  tab$.dcost <- NA
  tab$.deffect <- NA
  tab$.dref <- NA
  tab$.nmb <- NA
  tab$.dnmb <- NA
  
  for (i in seq_len(nrow(tab))) {
    if (i == 1) {
      tab$.icer[i] <- NA
      tab$.nmb[i] <- tab$.effect[i] * threshold - tab$.cost[i]
      ref_nmb <- tab$.nmb[i]
      ref_cost <- tab$.cost[i]
      ref_effect <- tab$.effect[i]
      ref_name <- tab$.strategy_names[i]
      
    } else {
      tab$.nmb[i] <- tab$.effect[i] * threshold - tab$.cost[i]
      tab$.dnmb[i] <- tab$.nmb[i] - ref_nmb
      tab$.dcost[i] <- tab$.cost[i] - ref_cost
      tab$.deffect[i] <- tab$.effect[i] - ref_effect
      tab$.icer[i] <- tab$.dcost[i] / tab$.deffect[i]
      tab$.dref[i] <- ref_name
      
      if (tab$.strategy_names[i] %in% ef) {
        ref_nmb <- tab$.nmb[i]
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
  cat(paste(
    to_text_dots(get_uneval_init(x)),
    collapse = "\n"
  ))
  cat(sprintf(
    "\n\nCounting method: '%s'.\n\n", x$method
  ))
  
  print_results(x$res_values, x$res_comp, x$res_nmb)
}

print_results <- function(res_values, res_comp, res_nmb) {
  cat("Values:\n\n")
  rownames(res_values) <- res_values$.strategy_names
  res_values <- dplyr::select_(res_values,
                               ~ - .strategy_names,
                               ~ - .n_indiv)
  print(res_values)
  
  if (! is.null(res_nmb)) {
    cat("\nNet monetary benefit difference:\n\n")
    .strategy_names <- res_nmb$.strategy_names
    f <- function(x) x - min(x)
    res_nmb <- res_nmb %>% 
      dplyr::select_(
        ~ - .strategy_names,
        ~ - .cost,
        ~ - .effect
      ) %>% 
      dplyr::mutate_all(dplyr::funs(f))
    
    rownames(res_nmb) <- .strategy_names
    print(res_nmb)
  }
  
  if (nrow(res_values) > 1) {
    cat("\nEfficiency frontier:\n\n")
    cat(paste(get_frontier(res_comp), collapse = " -> "))
    cat("\n\nDifferences:\n\n")
    
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
