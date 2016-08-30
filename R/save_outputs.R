#' Title Save graphs and other outputs to a designated directory
#'
#' @param outputs from run_models_from_files
#' @param base_dir directory in which the analysis is being conducted
#' @param output_dir ="output" subdirectory in which to write output
#' @param overwrite =FALSE if \code{output_dir} already exists, should it
#'    be overwritten?
#' @param create_dir =FALSE if\code{output_dir} does not exist, should it be
#'   created? 
#' @param use_devices =c("pdf", "png") the set of devices to output to
#'   (each plot will be written out to each device)
#'
#' @return   NULL; written for the side effect of creating files 
#'   in the output directory
#'
save_outputs <- function(outputs, base_dir, output_dir = "output",
                         overwrite = FALSE, create_dir = FALSE,
                         use_devices = c("pdf", "png")){
  full_path <- file.path(base_dir, output_dir)
  if(dir.exists(full_path) & !overwrite)
    stop(paste(full_path, "already exists, and overwrite = FALSE"))
  if(!dir.exists(full_path) & !create_dir)
    stop(paste(full_path, "does not exist, and create_dir = FALSE"))
  if(!dir.exists(full_path))
    make_model_directory_structure(base_dir, output_dir)
  

  ## plots about individual models
  model_names <- names(outputs$models)

  message("Generating plots for individual models...")
  for(this_model in model_names){
    this_plot <- plot(outputs$model_runs, model = this_model)
    this_file <- paste("state_count_plot", this_model, sep = "_")
    save_graph(this_plot, full_path, this_file)
    ##save_graph2(this_plot, full_path, this_file, use_devices)

    this_plot <- plot(outputs$dsa, model = this_model)
    this_file <- paste("dsa", this_model, sep = "_")
    save_graph(this_plot, full_path, this_file)
    ##save_graph2(this_plot, full_path, this_file, use_devices)
    
  }
  
  base_model <- get_base_model(outputs$model_runs)
  
  ## plots about differences between models
  message("Generating plots with model differences...")
  for(this_model in setdiff(model_names, base_model)){
    this_plot <- plot(outputs$dsa, type = "diff", model = this_model)
    this_file <- paste("dsa", this_model, "vs", base_model, sep = "_")
    save_graph(this_plot, full_path, this_file)
    ##save_graph2(this_plot, full_path, this_file, use_devices)

    this_plot <- plot(outputs$psa, model = this_model)
    this_file <- paste("psa", this_model, "vs", base_model, sep = "_")
    save_graph(this_plot, full_path, this_file)
    ##save_graph2(this_plot, full_path, this_file, use_devices)
    
  }
  
  ## acceptability curve
  message("Generating acceptability curve...")
  this_plot <- plot(outputs$psa, type = "ac")
  save_graph(plot(outputs$psa, type = "ac"),
             full_path, "acceptability")
  
  message("Writing ICER by group to a file...")
  write.csv(outputs$demographics$icers,
    file = paste(file.path(full_path, "icer_by_group"), ".csv", sep = ""))
  NULL
}

make_model_directory_structure <-
  function(base_dir, output_dir = "output"){
    full_path <- file.path(base_dir, output_dir)
    if(dir.exists(full_path))
      stop(paste(full_path, "already exists"))
    dir.create(full_path)
  }

save_graph <- function(plot, path, file_name){
  full_file <- file.path(path, file_name)
  png(filename = paste(full_file, "png", sep = "."))
  print(plot)
  dev.off()
  pdf(file = paste(full_file, "pdf", sep = "."))
  print(plot)
  dev.off()
}

## something about png currently doesn't work here,
##   possibly an issue with RStudio
save_graph2 <- function(plot, path, file_name, 
                        use_devices, ...){
  old_dev <- options()$device
  base_file <- file.path(path, file_name)
  
  for(this_device in use_devices){
    options(device = this_device)
    this_file <- paste(base_file, this_device, sep = ".")
    print(this_file)
    
    dev.new(file = this_file, ...)
    print(plot)
    dev.off()
  }
  options(device = old_dev)    
}