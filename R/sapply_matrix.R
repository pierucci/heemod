#' sapply_matrix
#'
#' @param this_mat a matrix or array
#' @param fun function to apply to each element of `this_mat`
#' @param ... additional arguments for `fun`
#'
#' @return a matrix or array of the same dimensions as `this_mat`,
#'   with each element containing the results of running `fun` on
#'   the corresponding element of `this_mat`
#' @export
#'
#' @examples
sapply_matrix <- function(this_mat, fun, ...){
  use_dim <- dim(this_mat)
  use_dimnames <- dimnames(this_mat)
  stopifnot(length(use_dim) > 0)
  res <- sapply(this_mat, fun, ...)
  dim(res) <- use_dim
  dimnames(res) <- use_dimnames
  res
}