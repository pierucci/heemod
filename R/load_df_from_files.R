#' Load Data From a Folder Into an Environment
#' 
#' Reads files containing data frames (in tabular format) 
#' from a directory, and loads them in an environment to be 
#' available during an analysis.  Typically used with 
#' \code{\link{f_look_up_values_df}}.
#' 
#' The files must be in .csv, .xls, or .xlsx format. A file 
#' my_df.csv (or my_df.xls, or my_df.xlsx) will be loaded as
#' a data frame my_df.
#' 
#' @param full_path A directory containing the files.
#' @param df_envir An environment.
#' 
#' @return The environment with the data frames.
load_df_from_files <- function(df_dir, df_envir) {
  if(! file.exists(df_dir))
    stop(paste(df_dir, "does not exist."))
  
  all_files <- list.files(df_dir, full.names = TRUE)
  obj_names <- all_files %>% 
    basename %>% 
    tools::file_path_sans_ext()
  
  if(any(duplicate_names <- duplicated(obj_names)))
    stop(paste("Duplicate data names:",
               paste(obj_names[duplicate_names], collapse = ", ")))
  
  ## do the assignments
  for(i in seq(along = all_files)){
    this_val <- read_file(all_files[i])
    assign(obj_names[i], this_val, envir = df_envir)
  }
  df_envir
}
