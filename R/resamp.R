#' Define parameters distribution for probabilistc analysis
#' 
#' Define the properties of parameter distributions and 
#' their correlation structure for probabilistic incertitude
#' analysis of Markov models.
#' 
#' @param ... Name-value pairs of expressions definig 
#'   parameter distributions.
#' @param mat_cor A correlation matrix for parameters.
#'   
#' @return An object of class \code{resamp_definition}.
#' @export
#' 
define_resample <- function(...,
                            mat_cor = diag(length(list(...)))) {
  list_qdist <- list(...)
  structure(
    list(
      list_qdist = list_qdist,
      mat_cor = mat_cor
    ),
    class = "resamp_definition"
  )
}


