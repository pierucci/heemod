#' Title  Load data frames in files in a directory into an environment
#' @description Reads files containing data frames (in tabular format)
#'   from a directory, and loads them in an environment to be available
#'   during an analysis.  Typically used with 
#'   \code{\link{f_look_up_values_df}}.
#' @param base_dir a directory
#' @param df_dir a subdirectory containing the files
#' @param this_envir an environment
#' @details The files must be in .csv, .xls, or .xlsx format.
#'   A file my_df.csv (or my_df.xls, or my_df.xlsx) 
#'   will cause the contents to be loaded as a data frame my_df.
#' @return invisibly, the environment with loaded objects
#'
load_df_from_files <- function(base_dir, df_dir, this_envir){
  full_path <- file.path(base_dir, df_dir)
  if(!file.exists(full_path) & file.exists(one_up <- dirname(base_dir))){
      full_path <- file.path(one_up, df_dir)
      message("reading data frames from one directory up")
      }
  if(!file.exists(full_path))
    stop(paste(df_dir, "does not exist in either", base_dir, "or", one_up))
      
  obj_names <- list.files(full_path)
  all_files <- list.files(full_path, full.names = TRUE)

  ## get rid of any file extensions to make object names
  obj_names <- gsub(".csv$", "", obj_names)
  obj_names <- gsub(".xls$", "", obj_names)
  obj_names <- gsub(".xlsx$", "", obj_names)
  
  if(any(duplicate_names <- duplicated(obj_names)))
    stop(paste("duplicate files in directory", df_dir, 
               "implying same name(s):",
               paste(obj_names[duplicate_names], collapse = ", ")))
  
  ## do the assignments
  for(i in seq(along = all_files)){
    this_val <- f_read_file(all_files[i])
    assign(obj_names[i], this_val, envir = this_envir)
  }
  invisible(this_envir)
}
