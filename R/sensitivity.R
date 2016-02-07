
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
  
  res <- dplyr::mutate_(res, .dots = attr(model, "ce"))
  
  structure(
    res,
    class = c("eval_sensitivity", class(res)),
    variables = attr(sensitivity, "variables"),
    model_ref = model
  )
}

#' Plot Sensitivity Analysis
#' 
#' Plot the results of a sensitivity analysis as a tornado 
#' plot.
#' 
#' Plot type \code{simple} plots variations of single model 
#' costs, while \code{diff} plots cost difference between 
#' the specified model and the reference model.
#' 
#' @param x A result of \code{\link{run_sensitivity}}.
#' @param model Name or index of model to plot.
#' @param type Type of plot (see details).
#' @param ... Additional arguments passed to \code{plot}.
#'   
#' @return A \code{ggplot2} object.
#' @export
#' 
plot.eval_sensitivity <- function(x, type = c("simple", "diff"),
                                  model = 1, ...) {
  type <- match.arg(type)
  
  
  switch(
    type,
    simple = {
      tab <- get_model(x, model)
      ref <- get_model(attr(x, "model_ref"), model)
      
      tab <- tidyr::gather_(
        data = tab,
        key_col = ".variable",
        value_col = ".value",
        gather_col = attr(x, "variables"),
        na.rm = TRUE
      )
      
      tab$.y <- tab$.cost - ref$.cost
      tab$.sign <- sign(tab$.y)
      tab$.sign <- as.factor(replace(tab$.sign, tab$.sign == -1, 0))
      tab$.x <- 0
      
      ggplot2::ggplot(
        tab,
        ggplot2::aes(
          x = .x,
          y = .variable
        )
      ) + 
        ggplot2::geom_segment(
          ggplot2::aes(
            xend = .y,
            yend = .variable,
            colour = .sign
          ),
          size = 5
        ) +
        ggplot2::guides(colour = FALSE) +
        ggplot2::geom_text(
          ggplot2::aes(
            x = .y,
            y = .variable,
            label = .value
          ),
          hjust = "outward"
        )
    },
    diff = {
      bm <- get_base_model(attr(x, "model_ref"))
      
      tab0 <- get_model(x, bm)
      tab1 <- get_model(x, model)
      ref0 <- get_model(attr(x, "model_ref"), bm)
      ref1 <- get_model(attr(x, "model_ref"), model)
      
      tab0 <- tidyr::gather_(
        data = tab0,
        key_col = ".variable",
        value_col = ".value",
        gather_col = attr(x, "variables"),
        na.rm = TRUE
      )
      tab1 <- tidyr::gather_(
        data = tab1,
        key_col = ".variable",
        value_col = ".value",
        gather_col = attr(x, "variables"),
        na.rm = TRUE
      )
      
      tab1$.y <- tab1$.cost - tab0$.cost
      tab1$.ref <- ref1$.cost - ref0$.cost
      tab1$.sign <- -sign(tab1$.y-tab1$.ref)
      tab1$.sign <- as.factor(replace(tab1$.sign, tab1$.sign == -1, 0))
      
      ggplot2::ggplot(
        tab1,
        ggplot2::aes(
          x = .ref,
          y = .variable
        )
      ) +
        ggplot2::geom_segment(
          ggplot2::aes(
            xend = .y,
            yend = .variable,
            colour = .sign
          ),
          size = 5) +
        ggplot2::guides(colour = FALSE) +
        ggplot2::geom_text(
          ggplot2::aes(
            x = .y,
            y = .variable,
            label = .value
          ),
          hjust = "outward") 
      
    },
    stop("Unknown type.")
  )
}
if(getRversion() >= "2.15.1") utils::globalVariables(
  c(".variable", ".x", ".y", ".ref", ".value", ".sign")
)
