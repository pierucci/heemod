#' Import Analysis from Tabular Data
#'
#' @param path Path to a folder containing the model information.
#'
#' Import an analysis saved in a folder as tabular data.
#'
#' A saved analysis follows a very specific format. Use
#' \code{\link{create_import_template}} or \code{\link{export}}
#' to populate the folder.
#'
#' @return Same result as \code{\link{run_models}}.
#' @export
#'
import <- function(path) {
  parsed_folder <- parse_import_folder(path)
  check_import_folder(parsed_folder)
  # import files for model definition
  list_import_models
  # build models from files
  list_models <- lapply(
    list_import_models,
    create_model
  )
  # import options for running models
  list_import_option
  # run models from files
  build_run_models
}

parse_import_folder <- function(path) {
  file_names <- list.files(path, full.names = FALSE)
  file_paths <- list.files(path, full.names = TRUE)
  
  list(
    ext = tools::file_ext(file_names),
    file_names = file_names,
    parsed_names = strsplit(
      x = tools::file_path_sans_ext(file_names),
      split = "_"
    ),
    file_paths = file_paths
  )
}

check_import_folder <- function(x) {
  # check coherence
  #   message if recycling needed
  #   message if optional file missing
  #   warning if file not following naming scheme
  #   stop if crucial file missing
  message("Looking for reference file.")
  if (! any(x$file_names == "reference.csv"))
    stop("No reference file found in folder.")
  message("Reading reference file")
  ref <- read.csv(
    file = x$file_path[x$file_names == "reference.csv"],
    stringsAsFactors = FALSE
  )
  model
}
# one reference file
# contains model names and state names

#' Create a Tabular Data Template
#'
#' @param path Path to an empty folder where
#'  the template will be created.
#' @param model_names Names of models.
#' @param state_names Names of states.
#' @param state_value_names Names of state values.
#'
#' @return \code{NULL}
#' @export
#'
create_import_template <- function(path, model_names,
                                   state_names,
                                   state_value_names) {
  
}
