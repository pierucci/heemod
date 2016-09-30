#' Prepare a cluster to use with heemod
#'
#' @param num_cores The number of cores desired
#'
#' @return If num_cores is 1 or 0, NULL.   If num_cores is
#'   a larger integer, a cluster loaded with the appropriate packages.
#' @export
#'
#' @examples
cluster_for_heemod <- function(num_cores) {
  cl <- NULL
  if (length(num_cores)) {
    stopifnot(round(num_cores) == num_cores)
    if (num_cores > 1) {
      if (!requireNamespace("parallel"))
        stop("parallel package required to use a cluster")
      cl <- parallel::makeCluster(num_cores)
      if (options()$heemod.verbose)
        message(paste("Using a cluster with", length(cl), "cores"))
      parallel::clusterEvalQ(cl, library(heemod))
      parallel::clusterEvalQ(cl, library(dplyr))
      cl
    }
  }
}