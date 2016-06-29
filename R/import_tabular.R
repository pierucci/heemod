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
  description <- import_description(path)

  
  # read all the other files
  # check if the structure is as expected
  # check if state and model naming is consistent
  # convert the files to R objects
  # run the analysis
}

import_description <- function(path) {
  tab_ref <- import_table(path, "description")
  stopifnot(
    all(names(tab_ref) == c("data", "file", "run")),
    all(tab_ref$data %in% c("matrix", "state_values", "parameters", "run")),
    all(tab_ref$file[tab_ref$data != "run"] %in% list.files(path)),
    all(tab_ref$run[tab_ref$data == "run"] %in% list.files(path))
  )
  for (r in unique(tab_ref$run[tab_ref$data == "run"])) {
    stopifnot(
      all(tab_ref$file[tab_ref$run == r] %in% list.files(file.path(path, r)))
    )
  }
  tab_ref
}

import_table <- function(folder, file) {
  list_files <- list.files(path)
  pos_match <- file %in% tools::file_path_sans_ext(list_files)
  file_match <- list_files[pos_match]
  ext <- tolower(tools::file_ext(file_match))
  
  if (! pos_match)
    stop(sprintf("File '%s' not found in folder '%s'.",
                 file, folder))
  
  if (sum(pos_match) > 1)
    stop(sprintf("More than one file matches '%s' in folder '%s'.",
                 file, folder))
  
  if (! ext %in% c("xls", "xlsx", "csv"))
    stop(sprintf("File extension '%s' of file '%s' is not supported.",
                 ext, file))
  
  if (ext %in% c("xls", "xlsx")) {
    readxl::read_excel(file.path(folder, file_match))
  } else if (ext %in% c("csv")) {
    read.csv(
      file.path(folder, file_match),
      stringsAsFactors = FALSE)
  } else {
    stop("You're not supposed to be there...")
  }
}

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
