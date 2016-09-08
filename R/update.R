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
#' similar to those of \code{\link{run_models}}.
#' 
#' @name update-model
#' @param object The result of \code{\link{run_models}}.
#' @param newdata A \code{data.frame} of new parameter sets,
#'   one column per parameter and one row per parameter set.
#'   An optional \code{.weights} column can be included for
#'   a weighted analysis.
#' @param x Updated model to plot.
#' @param model A model index, character or numeric.
#' @param type The type of plot to return (see details).
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
update.run_models <- function(object, newdata, ...) {
  
  if (! any(class(object) %in% "run_models")) {
    stop("'object' must be the result of 'run_models()'.")
  }
  
  has_weights <- ".weights" %in% names(newdata)
  
  if (has_weights) {
    weights <- newdata$.weights
    newdata <- dplyr::select_(newdata, ~ (- .weights))
  } else {
    message("No weights specified in model update, using equal weights.")
    weights <- rep(1, nrow(newdata))
  }
  
  ce <- attr(object, "ce")
  list_res <- list()
  
  for (n in get_model_names(object)) {
    message(sprintf("Updating model '%s'...", n))
    suppressMessages({
      list_res <- c(
        list_res,
        list(eval_model_newdata(object, model = n, newdata = newdata))
      )
    })
  }
  
  names(list_res) <- get_model_names(object)
  
  for (n in names(list_res)) {
    list_res[[n]]$.model_names <- n
    list_res[[n]]$.index <- seq_len(nrow(newdata))
  }
  
  res <- Reduce(dplyr::bind_rows, list_res)
  
  suppressMessages({
    res_total <- res %>% 
      dplyr::rowwise() %>% 
      dplyr::do_(~ get_total_state_values(.$.mod)) %>% 
      dplyr::bind_cols(res %>% dplyr::select_(~ - .mod)) %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by_(".index") %>% 
      dplyr::mutate_(.dots = ce) %>% 
      dplyr::do_(~ compute_icer(., model_order = order(object$.effect))) %>% 
      dplyr::left_join(
        dplyr::data_frame(
          .index = seq_len(nrow(newdata)),
          .weights = weights
        )
      )
  })
  
  comb_mods <- combine_models(
    list_newmodels = list_res,
    weights = weights,
    oldmodel = object
  )
  
  structure(
    res_total,
    class = c("updated_models", class(res)),
    newdata = newdata,
    original_model = object,
    combined_models = comb_mods,
    has_weights = has_weights,
    weights = weights
  )
}

#' @export
print.updated_models <- function(x, ...) {
  print(summary(x, ...))
}

#' @export
#' @rdname update-model
plot.updated_models <- function(x, model,
                                type = c("cost", "effect", "icer",
                                         "counts", "ce"),
                                ...) {
  type <- match.arg(type)
  
  if (type %in% c("counts", "ce")) {
    return(graphics::plot(attr(x, "combined_models"),
                          type = type, model = model))
  }
  
  check_model_index(
    attr(x, "original_model"),
    model,
    allow_multiple = TRUE
  )
  
  if (get_base_model(attr(x, "original_model")) %in% model) {
    stop("Cannot represent value differences from base model.")
  }
  
  if (is.numeric(model)) {
    model <- get_model_names(attr(x, "original_model"))[model]
  }
  
  switch(
    type,
    cost = {
      x_var <- ".dcost"
      x_lab <- "Cost"
    },
    effect = {
      x_var <- ".deffect"
      x_lab <- "Effect"
    },
    icer = {
      x_var <- ".icer"
      x_lab <- "ICER"
    }
  )
  
  res_plot <- (x[x$.model_names %in% model, ]) %>% 
    ggplot2::ggplot(ggplot2::aes_string(x = x_var)) +
    ggplot2::geom_histogram(...) +
    ggplot2::xlab(x_lab)
  
  if (length(model) > 1) {
    res_plot <- res_plot +
      ggplot2::facet_wrap(~ .model_names)
  }
  
  res_plot
}

#' @export
summary.updated_models <- function(object, ...) {
  
  model_names <- get_model_names(
    attr(object, "original_model")
  )[order(attr(object, "original_model")$.effect)]
  
  list_res <- list()
  
  for (n in model_names) {
    list_res <- c(
      list_res,
      lapply(
        c(".cost", ".effect", ".dcost", ".deffect", ".icer"),
        function(x) {
          wsum <- wtd_summary(
            object[object$.model_names == n, ][[x]],
            object[object$.model_names == n, ]$.weights
          )
          is.na(wsum) <- ! is.finite(wsum)
          tab_summary <- matrix(
            wsum,
            nrow = 1
          )
          colnames(tab_summary) <- names(wsum)
          cbind(
            data.frame(Model = n,
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
                 "\u0394 Cost", "\u0394 Effect",
                 "Icer")
    )
  
  mat_res <- dplyr::select_(
    tab_res,
    ~ (- Model),
    ~ (- Value)
  ) %>% 
    as.matrix
  
  rownames(mat_res) <- tab_res$Model %>% 
    paste(tab_res$Value, sep = " - ")
  
  structure(
    tab_res,
    class = c("summary_updated_models", class(tab_res)),
    model = object,
    to_print = mat_res
  )
}

#' @export
print.summary_updated_models <- function(x, ...) {
  object <- attr(x, "model")
  
  cat(sprintf(
    "An analysis re-run on %i parameter sets.\n\n",
    nrow(attr(object, "newdata"))
  ))
  
  if (! attr(object, "has_weights")) {
    cat("* Unweighted analysis.")
  } else {
    cat("* Weigths distribution:\n\n")
    print(summary(attr(object, "weights")))
    cat(sprintf("\nTotal weight: %s",
                format(sum(attr(object, "weights")))))
  }
  
  cat("\n\n* Values distribution:\n\n")
  
  print(attr(x, "to_print"), na.print = "-")
  
  cat("\n* Combined result:\n\n")
  
  print(attr(object, "combined_models"))
  
  invisible(x)
}
