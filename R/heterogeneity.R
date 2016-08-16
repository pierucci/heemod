#' Run Heterogeneity Analysis
#'  
#' Given a table of new parameter values with a new 
#' parameter set per line, runs iteratively Markov models 
#' over these sets and return an heterogeneity analysis.
#' 
#' @param x The result of \code{\link{run_models}}.
#' @param newdata A \code{data.frame} of new parameter sets, one 
#'   column per parameter and one row per parameter set. Can
#'   be prespecified for heterogeneity analysis or randomly 
#'   drawn with resample for probability analysis.
#' @param model A model index, character or numeric.
#' @param type The type of plot to return (see details).
#' @param ... Additional arguments passed to \code{geom_histogram}.
#' Especially usefull to specify \code{binwidth}.
#' 
#' For the plotting function, the \code{type} argument can take
#' the following values: \code{"cost"}, \code{"effect"} or 
#' \code{"icer"} to plot the heterogeneity of the respective
#' values; \code{"ce"} to draw a scatterplot of cost vs. effect.
#'   
#' @return A \code{data.frame} with one row per model/value.
#' @export
#' 
#' @example inst/examples/example_run_heterogeneity.R
#'   
run_heterogeneity <- function(x, newdata) {
  
  if (! any(class(x) %in% "run_models")) {
    stop("Object 'x' must be the result of 'run_models()'.")
  }
  
  ce <- attr(x, "ce")
  list_res <- list()
  for (n in get_model_names(x)) {
    message(sprintf("Running analysis for model '%s'.", n))
    suppressMessages({
      list_res <- c(
        list_res,
        list(eval_model_newdata(x, model = n, newdata = newdata))
      )
    })
  }
  
  names(list_res) <- get_model_names(x)
  
  for (n in names(list_res)) {
    list_res[[n]]$.model_names <- n
    list_res[[n]]$.index <- seq_len(nrow(newdata))
  }
  
  res <- Reduce(dplyr::bind_rows, list_res)
  
  res_total <- res %>% 
    dplyr::rowwise() %>% 
    dplyr::do(get_total_state_values(.$.mod)) %>% 
    dplyr::bind_cols(res %>% dplyr::select_(quote(- .mod))) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(.index) %>% 
    dplyr::mutate_(.dots = ce) %>% 
    dplyr::do(compute_icer(., model_order = order(x$.effect)))
  
  structure(
    res_total,
    class = c("heterogeneity", class(res)),
    newdata = newdata,
    original_model = x
  )
}

#' @export
print.heterogeneity <- function(x, ...) {
  cat(sprintf(
    "An heterogeneity analysis on %i parameter sets.\n",
    nrow(attr(x, "newdata"))
  ))
}

#' @export
#' @rdname run_heterogeneity
plot.heterogeneity <- function(x, model,
                               type = c("cost", "effect", "icer"),
                               ...) {
  if (get_base_model(attr(x, "original_model")) %in% model) {
    stop("Cannot represent value differences from base model.")
  }
  type <- match.arg(type)
  check_model_index(
    attr(x, "original_model"),
    model,
    allow_multiple = TRUE
  )
  
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
summary.heterogeneity <- function(object, model, ...) {
  if (get_base_model(attr(object, "original_model")) %in% model) {
    stop("Cannot represent value differences from base model.")
  }
  
  check_model_index(
    attr(object, "original_model"),
    model,
    allow_multiple = FALSE
  )
  
  if (is.numeric(model)) {
    model <- get_model_names(attr(object, "original_model"))[model]
  }
  
  tab <- object[object$.model_names == model, ]
  list_res <- list()
  
  for (n in c(".dcost", ".deffect", ".icer")) {
    list_res <- c(
      list_res, list(summary(tab[[n]]))
    )
  }
  
  tab_res <- Reduce(rbind, list_res)
  rownames(tab_res) <- c("Cost", "Effect", "ICER")
  
  tab_res
}
