
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
    class = c("sensitivity", class(list_df[[1]]))
  )
}

#' Run Sensitivity Analysis
#' 
#' @param model An evaluated Markov model
#' @param sensitivity An object returned by 
#'   \code{\link{define_sensitivity}}.
#'   
#' @return A list with one \code{data.frame} per model.
#' @export
#' 
#' @example inst/examples/example_run_sensitivity.R
run_sensitivity <- function(model, sensitivity) {
  
  init <- attr(model, "init")
  cycles <- attr(model, "cycles")
  method <- attr(model, "method")
  list_models <- attr(model, "uneval_model_list")
  
  res <- lapply(list_models, eval_model_newdata, method = method,
                init = init, cycles = cycles, newdata = sensitivity)
  return(res)
}
