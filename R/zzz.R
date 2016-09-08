.onLoad <- function(libname, pkgname) {
  op <- options()
  op.heemod <- list(
    heemod.verbose = FALSE,
    heemod.memotime = 3600
  )
  toset <- !(names(op.heemod) %in% names(op))
  if(any(toset)) options(op.heemod[toset])
  
  invisible()
}
