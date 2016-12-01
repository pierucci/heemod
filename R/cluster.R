reach_cluster <- local({
  heemod_cluster <- NULL
  
  function(operation = c("get", "set", "close"), value) {
    operation <- match.arg(operation)
    
    switch(
      operation,
      "get" = {
        heemod_cluster
      },
      "set" = {
        heemod_cluster <<- value
      },
      "close" = {
        parallel::stopCluster(heemod_cluster)
        heemod_cluster <<- NULL
      }
    )
  }
})

get_cluster <- function() {
  reach_cluster(operation = "get")
}

set_cluster <- function(x) {
  reach_cluster(operation = "set", value = x)
}

#' Run \code{heemod} on a Cluster
#' 
#' These functions create or delete a cluster for 
#' \code{heemod}. When the cluster is created it is 
#' automagically used by \code{heemod} functions.
#' 
#' The usual wokflow is to create the cluster with 
#' \code{use_cluster}, then run functions such as 
#' \code{\link{run_psa}} that make use of the cluster. To 
#' stop using the cluster run \link{close_cluster}.
#' 
#' The cluster status is given by \code{status_cluster}.
#' 
#' A custom cluster can be passed to \code{use_cluster} with
#' the \code{cluster} argument. This custom custer needs to 
#' work with \code{\link{parallel}{parLapply}}.
#' 
#' @name cluster
#' @param num_cores Number of core.
#' @param cluster A custom cluster. See details.
#' @param close Close existing cluster before defining a new
#'   one?
#' @param verbose Print cluster info.
#'   
#' @return \code{use_cluster} and \code{close_cluster} 
#'   return \code{TRUE} invisibly in case of success. 
#'   \code{status_cluster} returns \code{TRUE} if a cluster 
#'   is defined, \code{FALSE} otherwise.
#'   
#' @export
use_cluster <- function(num_cores, cluster = NULL, close = TRUE) {
  if (status_cluster(verbose = FALSE)) {
    if (close) {
      close_cluster()
    } else {
      stop("A cluster is already defined, use 'close_cluster()' before defining a new cluster.")
    }
  }
  
  if (! is.null(cluster)) {
    set_cluster(cluster)
    message(paste("Using a cluster with", length(cluster), "cores."))
    
  } else {
    if (! requireNamespace("parallel"))
      stop("'parallel' package required to define a cluster.")
    
    if (! is.wholenumber(num_cores))
      stop("'num_cores' is not a whole number.")
    
    cl <- parallel::makeCluster(num_cores)
    parallel::clusterEvalQ(cl, library(heemod))
    parallel::clusterEvalQ(cl, library(dplyr))
    
    set_cluster(cl)
    
    message(paste("Using a cluster with", num_cores, "cores."))
  }
  
  invisible(TRUE)
}

#' @rdname cluster
#' @export
status_cluster <- function(verbose = TRUE) {
  sc <- ! is.null(get_cluster())
  
  if (verbose) {
    if (sc) {
      message(sprintf(
        "Running on a %i-cores cluster.",
        length(get_cluster())
      ))
    } else {
      message("No cluster defined.")
    }
  }
  
  invisible(sc)
}

#' @rdname cluster
#' @export
close_cluster <- function() {
  if (! status_cluster(verbose = FALSE)) {
    warning("No cluster defined.")
    return(invisible(FALSE))
  }
  
  catch <- try(reach_cluster(operation = "close"))
  
  if (inherits(catch, "try-error")) {
    stop("Failed to close the cluster.")
  } else {
    message("Cluster closed.")
    invisible(TRUE)
  }
}
