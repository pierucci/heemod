#' Run Model on New Data
#' 
#' Given a table of new parameter values with a new 
#' parameter set per line, runs iteratively Markov models 
#' over these sets.
#' 
#' \code{newdata} must be a \code{data.frame} with the
#' following properties: the column names must be parameter 
#' names used in \code{\link{define_parameters}}; and an
#' optional column \code{.weights} can give the respective
#' weight of each row in the target population.
#' 
#' Weights are automatillcally scaled. If no weights are
#' provided equal weights are used for each strata.
#' 
#' For the plotting function, the \code{type} argument can
#' take the following values: \code{"cost"}, \code{"effect"}
#' or \code{"icer"} to plot the heterogeneity of the
#' respective values. Furthermore \code{"ce"} and
#' \code{"count"} can produce from the combined model plots
#' similar to those of \code{\link{run_model}}.
#' 
#' @name update-model
#' @param object The result of \code{\link{run_model}}.
#' @param newdata A \code{data.frame} of new parameter sets,
#'   one column per parameter and one row per parameter set.
#'   An optional \code{.weights} column can be included for
#'   a weighted analysis.
#' @param x Updated model to plot.
#' @param strategy A model index, character or numeric.
#' @param result The the result to plot (see details).
#' @param type Plot simple values or differences?
#' @param ... Additional arguments passed to
#'   \code{geom_histogram}. Especially usefull to specify
#'   \code{binwidth}.
#'   
#' @section Warning:
#'   
#'   Histograms do not account for weights. On the other
#'   hand summary results do.
#'   
#' @return A \code{data.frame} with one row per model/value.
#' @export
#' 
#' @example inst/examples/example_update.R
#'   
update.run_model <- function(object, newdata, ...) {
  
  if (! any(class(object) %in% "run_model")) {
    stop("'object' must be the result of 'run_model()'.")
  }
  
  has_weights <- ".weights" %in% names(newdata)
  
  if (has_weights) {
    weights <- newdata$.weights
    newdata <- dplyr::select_(newdata, ~ (- .weights))
    
  } else {
    message("No weights specified in update, using equal weights.")
    weights <- rep(1, nrow(newdata))
  }
  
  ce <- get_ce(object)
  list_res <- list()
  
  for (n in get_strategy_names(object)) {
    message(sprintf("Updating strategy '%s'...", n))
    suppressMessages({
      list_res <- c(
        list_res,
        list(eval_strategy_newdata(
          object, strategy = n, 
          newdata = newdata
        ))
      )
    })
  }
  
  names(list_res) <- get_strategy_names(object)
  
  for (n in names(list_res)) {
    list_res[[n]]$.strategy_names <- n
    list_res[[n]]$.index <- seq_len(nrow(newdata))
  }
  
  res <- Reduce(dplyr::bind_rows, list_res)
  
  suppressMessages({
    res_total <- res %>% 
      dplyr::rowwise() %>% 
      dplyr::do_(~ get_total_state_values(.$.mod)) %>% 
      dplyr::bind_cols(res %>% dplyr::select_(~ - .mod)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate_(.dots = ce) %>% 
      dplyr::left_join(
        dplyr::data_frame(
          .index = seq_len(nrow(newdata)),
          .weights = weights
        )
      ) %>% 
      dplyr::ungroup()
  })
  
  comb_mods <- combine_models(
    newmodels = list_res,
    weights = weights,
    oldmodel = object
  )
  
  structure(
    list(
      updated_model = res_total,
      newdata = newdata,
      model = object,
      combined_model = comb_mods,
      has_weights = has_weights,
      weights = weights
    ),
    class = c("updated_model", class(res))
  )
}

#' @export
print.updated_model <- function(x, ...) {
  print(summary(x), ...)
}

#' @export
#' @rdname update-model
plot.updated_model <- function(x, type = c("simple", "difference",
                                           "counts", "ce", "values"),
                               result = c("cost", "effect", "icer"),
                               strategy = NULL,
                               ...) {
  type <- match.arg(type)
  result <- match.arg(result)
  
  if (type %in% c("counts", "ce", "values")) {
    return(plot(x$combined_model,
                type = type, strategy = strategy,
                ...) 
    )
  }
  if (is.null(strategy)) {
    strategy <- get_strategy_names(get_model(x))
    
    if (type == "difference") {
      strategy <- setdiff(strategy, get_noncomparable_strategy(get_model(x)))
    }
    
  } else {
    strategy <- check_strategy_index(
      get_model(x),
      strategy,
      allow_multiple = TRUE
    )
  }
  
  if (get_noncomparable_strategy(get_model(x)) %in% strategy &&
      "difference" %in% type) {
    stop("Cannot represent value differences from uncomparable strategy.")
  }
  
  if (type == "simple" && result == "icer") {
    stop("Result 'icer' can conly be computed with type = 'difference'.")
  }
  
  switch(
    paste(type, result, sep = "_"),
    simple_cost = {
      x_var <- ".cost"
      x_lab <- "Cost"
    },
    simple_effect = {
      x_var <- ".effect"
      x_lab <- "Effect"
    },
    difference_cost = {
      x_var <- ".dcost"
      x_lab <- "Cost Diff."
    },
    difference_effect = {
      x_var <- ".deffect"
      x_lab <- "Effect Diff."
    },
    difference_icer = {
      x_var <- ".icer"
      x_lab <- "ICER"
    }
  )
  summary(x)$scaled_results %>% 
    dplyr::filter_(
      substitute(.strategy_names %in% .x, list(.x = strategy))
    ) %>% 
    ggplot2::ggplot(ggplot2::aes_string(x = x_var)) +
    ggplot2::geom_histogram(...) +
    ggplot2::xlab(x_lab)+
    ggplot2::facet_grid(.strategy_names ~ .)
}

scale.updated_model <- function(x, scale = TRUE, center = TRUE) {
  .bm <- get_root_strategy(get_model(x))
  
  res <- x$updated_model
  
  if (scale) {
    res <- res %>% 
      dplyr::mutate_(
        .cost = ~ .cost / sum(get_init(get_model(x))),
        .effect = ~ .effect / sum(get_init(get_model(x)))
      )
  }
  
  if (center) {
    res <- res %>% 
      dplyr::group_by_(".index") %>% 
      dplyr::mutate_(
        .cost = ~ (.cost - sum(.cost * (.strategy_names == .bm))),
        .effect = ~ (.effect - sum(.effect * (.strategy_names == .bm)))
      ) %>% 
      dplyr::ungroup()
  }
  
  res
}

get_model.updated_model <- function(x) {
  x$model
}

#' @export
summary.updated_model <- function(object, ...) {
  
  strategy_names <- get_strategy_names(
    get_model(object)
  )[ord_eff <- order(get_effect(get_model(object)))]
  
  list_res <- list()
  
  tab_scaled <- object %>% 
    scale(center = FALSE) %>% 
    dplyr::group_by_(".index") %>% 
    dplyr::do_(~ compute_icer(
      ., strategy_order = ord_eff)
    )
  
  for (.n in strategy_names) {
    
    tmp <- tab_scaled %>%
      dplyr::filter_(~ .strategy_names == .n)
    
    list_res <- c(
      list_res,
      lapply(
        c(".cost", ".effect", ".dcost", ".deffect", ".icer"),
        function(x) {
          wsum <- wtd_summary(
            tmp[[x]],
            tmp$.weights
          )
          is.na(wsum) <- ! is.finite(wsum)
          tab_summary <- matrix(
            wsum,
            nrow = 1
          )
          colnames(tab_summary) <- names(wsum)
          cbind(
            data.frame(Model = .n,
                       Value = x),
            tab_summary
          )
        }
      )
    )
  }
  
  tab_res <- Reduce(rbind, list_res)
  
  tab_res$Value <- tab_res$Value %>% 
    factor(
      levels = c(".cost", ".effect",
                 ".dcost", ".deffect", 
                 ".icer"),
      labels = c("Cost", "Effect",
                 "Cost Diff.", "Effect Diff.",
                 "Icer")
    )
  
  mat_res <- dplyr::select_(
    tab_res,
    ~ (- Model),
    ~ (- Value)
  ) %>% 
    as.matrix()
  
  rownames(mat_res) <- tab_res$Model %>% 
    paste(tab_res$Value, sep = " - ")
  
  structure(
    list(
      summary_results = tab_res,
      scaled_results = tab_scaled,
      model = object,
      to_print = mat_res,
      sum_comb = summary(object$combined_model, ...)
    ),
    class = c("summary_updated_model", class(tab_res))
  )
}

get_model.summary_updated_model <- function(x) {
  x$model
}

#' @export
print.summary_updated_model <- function(x, ...) {
  object <- get_model(x)
  
  cat(sprintf(
    "An analysis re-run on %i parameter sets.\n\n",
    nrow(object$newdata)
  ))
  
  if (! object$has_weights) {
    cat("* Unweighted analysis.")
  } else {
    cat("* Weigths distribution:\n\n")
    print(summary(object$weights))
    cat(sprintf("\nTotal weight: %s",
                format(sum(object$weights))))
  }
  
  cat("\n\n* Values distribution:\n\n")
  
  print(x$to_print, na.print = "-")
  
  cat("\n* Combined result:\n\n")
  
  print(x$sum_comb)
  
  invisible(x)
}
