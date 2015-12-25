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


