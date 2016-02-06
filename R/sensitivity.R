
#' Define a Sensitivity Analysis
#' 
#' Define parameter variations for a Markov model 
#' sensitivity analysis.
#' 
#' Parameter variations windows are given as length 2 vector
#' of the form \code{c(min, max)}.
#' 
#' @param ... A named list of min and max values that
#'   parameters will take.
#'   
#' @return A \code{sensitivity} object.
#' @export
#' 
#' @examples
#' 
#' define_sensitivity(
#'   a = c(10, 45),
#'   b = c(.5, 1.5)
#' )
#' 
define_sensitivity <- function(...) {
  .dots <- list(...)
  define_sensitivity_(.dots)
}

define_sensitivity_ <- function(.dots) {
  stopifnot(
    all(unlist(lapply(.dots, function(x) length(x))) == 2),
    ! is.null(names(.dots)),
    ! any(names(.dots) == ""),
    ! any(duplicated(names(.dots)))
  )
  
  f <- function(x, y) {
    x <- dplyr::data_frame(x = x)
    names(x) <- y
    x
  }
  
  list_df <- mapply(f , .dots, names(.dots), SIMPLIFY = FALSE)
  
  structure(
    Reduce(dplyr::bind_rows, list_df),
    class = c("sensitivity", class(list_df[[1]])),
    variables = names(.dots)
  )
}

#' Run Sensitivity Analysis
#' 
#' @param model An evaluated Markov model
#' @param sensitivity An object returned by 
#'   \code{\link{define_sensitivity}}.
#'   
#' @return A \code{data.frame} with one row per model and
#'   parameter value.
#' @export
#' 
#' @example inst/examples/example_run_sensitivity.R
run_sensitivity <- function(model, sensitivity) {
  
  init <- attr(model, "init")
  cycles <- attr(model, "cycles")
  method <- attr(model, "method")
  list_models <- attr(model, "uneval_model_list")
  
  list_res <- lapply(list_models, eval_model_newdata, method = method,
                     init = init, cycles = cycles, newdata = sensitivity)
  for (n in names(list_res)) {
    list_res[[n]]$.model_names <- n
  }
  
  res <- Reduce(dplyr::bind_rows, list_res)
  
  structure(
    res,
    class = c("eval_sensitivity", class(res)),
    variables = attr(sensitivity, "variables"),
    model_ref = model
  )
}

#' Plot Sensitivity Analysis
#' 
#' Plot the results of a sensitivity analysis as a tornado plot.
#'
#' @param x A result of \code{\link{run_sensitivity}}.
#' @param model Name or index of model to plot.
#' @param value State value to plot.
#' @param xlab x-axis label.
#' @param ylab t-axis label.
#' @param ... Additional arguments passed to \code{plot}.
#'
#' @return A \code{ggplot2} object.
#' @export
#'
plot.eval_sensitivity <- function(x, model = 1,
                                  value, xlab = "Parameter",
                                  ylab = value, ...) {
  stopifnot(
    ! missing(value)
  )
  
  tab <- get_model(x, model)
  ref <- get_model(attr(x, "model_ref"), model)
  
  tab <- tidyr::gather_(
    data = tab,
    key_col = ".variable",
    value_col = ".value",
    gather_col = attr(x, "variables"),
    na.rm = TRUE
  )
  tab$.y <- tab[[value]]- ref[[value]]
  
  ggplot(tab, aes(
    x = .variable,
    y = .y,
    fill = as.factor(sign(.y)))) + 
    geom_bar(position = "identity", stat = "identity") +
    coord_flip() +
    guides(fill=FALSE) +
    geom_text(aes(
      x = as.numeric(as.factor(.variable)),
      y = .y,
      label = .value
    )) +
    xlab(xlab) +
    ylab(ylab)
}
if(getRversion() >= "2.15.1") utils::globalVariables(
  c(".variable", ".y", ".value")
)
