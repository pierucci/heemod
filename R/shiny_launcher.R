#' Run Shiny Graphical Interface
#' 
#' Run the \code{shiny} graphical interface to \code{heemod}.
#'
#' @return \code{NULL}
#' @export
#'
run_shiny <- function() {
  app_dir <- system.file("shiny", "main", package = "heemod")
  if (app_dir == "") {
    stop("Could not find 'shiny' directory. Try re-installing `heemod`.", call. = FALSE)
  }
  
  shiny::runApp(app_dir, display.mode = "normal")
}
