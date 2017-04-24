
pad_matrix <- function(orig_matrix, pad_to){
  new_matrix <- matrix(NA, nrow = nrow(orig_matrix), ncol = pad_to)
  new_matrix[, 1:ncol(orig_matrix)] <- orig_matrix
  dimnames(new_matrix) <- list(rownames(orig_matrix), NULL)
  new_matrix
}

prepare_vcov <- function(fit_list){
  km_pos <- match("km", names(fit_list))
  ## drop Kaplan-Meier, if present
  km_pos <- km_pos[!is.na(km_pos)]
  if(length(km_pos) > 0)
    fit_list <- fit_list[-km_pos]
  
  max.n.pars <- max(sapply(fit_list, function(x){x$npars}))
  
  vcov_matrices <- lapply(fit_list, vcov)
  vcov_matrices <- lapply(vcov_matrices, pad_matrix, pad_to = max.n.pars)
  prefixes <- names(fit_list)
  for(i in 1:length(vcov_matrices))
    rownames(vcov_matrices[[i]]) <- 
    paste(prefixes[i], rownames(vcov_matrices[[i]]), sep = "_")
  
  do.call("rbind", vcov_matrices)
}

prepare_fit_info <- function(fit_list){
  ## drop Kaplan-Meier, if present
  km_pos <- match("km", names(fit_list))
  km_pos <- km_pos[!is.na(km_pos)]
  if(length(km_pos) > 0)
    fit_list <- fit_list[-km_pos]
  par.est <- lapply(fit_list,
                    function(x) {
                      res <- x$res.t[, 1]
                      names(res) <-rownames(x$res.t)
                      res
                    })
  AIC <- sapply(fit_list, get_component, comp = "AIC") #function(x){x$AIC})
  BIC <- sapply(fit_list, get_component, comp = "BIC") #function(x){x$BIC})
  vcov <- prepare_vcov(fit_list)
  list(par.est = par.est, AIC = AIC, BIC = BIC, vcov = vcov)
}

write_fits_to_excel_from_tibble <- 
  function(fit_tibble, wb, skip_at_start = 3, skip_between = 1,
           alignment = c("horizontal", "vertical")){
    if(!require("XLConnect"))
      stop("XLConnect package required to write Excel files")
    
    ## if necessary, get flexsurvreg fits out of surv_X objects
    
    
    if(is.character(wb)) 
      wb <- loadWorkbook(wb, create = TRUE)
    alignment <- match.arg(alignment)
    stopifnot(inherits(wb, "workbook"))
    stopifnot(identical(names(fit_tibble),
                c("type", "treatment", "set_name",
                  "dist", "fit", "set_def", "time_subtract"))
              )
    createSheet(wb, "OPCPem")

    fit_tibble_nest <- 
      dplyr::filter(fit_tibble, dist != "km")
    fit_tibble_nest$fit <- lapply(fit_tibble_nest$fit, extract_fits)
    fit_tibble_nest <- 
      fit_tibble_nest %>%
        dplyr::group_by(type, treatment, set_name) %>%
          tidyr::nest()
    
    
    
    ## need to know how many rows of output we're going to write
    num_rows <- sum(sapply(fit_tibble_nest[[1, "data"]]$dist, 
                             function(x){
                               length(formals(get(paste("r", x, sep = "")))) - 1}
                             )
                      )
    if(alignment == "vertical"){
    rows_per <- 2 * (num_rows + skip_between) + 
      nrow(fit_tibble_nest[[1, "data"]]) + 1
    }
    if(alignment == "horizontal"){
      rows_per <- 
        num_rows + 
          skip_between + nrow(fit_tibble_nest[[1, "data"]]) + 1
    }
    fit_tibble_nest$start_row <- skip_at_start + 1 + 
      rows_per * (1:nrow(fit_tibble_nest) - 1)
  
    fit_tibble_nest <-
      fit_tibble_nest %>%
        dplyr::group_by(type, treatment, set_name) %>%
          dplyr::do(send_info_to_workbook(., wb = wb, 
                                          skip_between = skip_between,
                                          alignment = alignment))
    
    plot_data <- prepare_fit_list_plot_data_from_tibble(fit_tibble)
    plot_data_km <- plot_data %>% dplyr::filter(dist == "km")
    
    createSheet(wb, "km")
    writeWorksheet(wb, 
                   plot_data_km[, c("type", "treatment", "set_name", "dist",
                                         "time", "est", "lcl", "ucl")],
                   sheet= "km", 
                   startRow = skip_at_start,
                   startCol = 1)
          
    saveWorkbook(wb)
    invisible(plot_data)
  }


send_info_to_workbook <- 
  function(fit_tib, wb, skip_between = 1,
           alignment = c("horizontal", "vertical")){

  alignment <- match.arg(alignment)
  
  fit_list <- fit_tib[[1, "data"]][["fit"]]
  names(fit_list) <- fit_tib[[1, "data"]][["dist"]]
  start_row <- fit_tib[[1, "start_row"]]
  dist_names <- names(fit_list)
  fit_info <- prepare_fit_info(fit_list)
  
  if(alignment == "vertical"){
    start_row_2 <- start_row + length(dist_names) + skip_between + 1
    start_row_3 <- 
      start_row_2 + length(unlist(fit_info$par.est)) + skip_between
    start_col_1 <- start_col_2 <- start_col_3 <- 1
  }
  if(alignment == "horizontal"){
    start_row_2 <- start_row + length(dist_names) + skip_between + 1
    start_row_3 <- start_row_2
    start_col_1 <- 1
    start_col_2 <- 1
    start_col_3 <- 8
    
  }
  
  stat_output <- data.frame(fit_tib[[1, "type"]],
                            fit_tib[[1, "treatment"]],
                            fit_tib[[1, "set_name"]],
                            dist = dist_names, 
                           AIC = fit_info$AIC,
                           BIC = fit_info$BIC)
  names(stat_output)[1:3] <- ""
    writeWorksheet(wb, stat_output,
                 sheet = "OPCPem",
                  startRow = start_row, startCol = 1, header=TRUE)
 #   new_start_row <- start_row + length(dist_names) + skip_between + 1
    
    write_pars <- data.frame(
      fit_tib[[1, "type"]],
      fit_tib[[1, "treatment"]],
      fit_tib[[1, "set_name"]],
      unlist(lapply(fit_info$par.est, names)),
      rep(names(fit_info$par.est),
          sapply(fit_info$par.est, length)),
      unlist(fit_info$par.est))
    writeWorksheet(wb, write_pars,
                   sheet = "OPCPem",
                   startRow = start_row_2, 
                   startCol = start_col_2, 
                   header = FALSE) 
    use_names <- rownames(fit_info$vcov)
    write_vcov <- fit_info$vcov
    if(alignment == "vertical"){
    write_vcov <- data.frame(fit_tib[[1, "type"]],
                            fit_tib[[1, "treatment"]],
                            fit_tib[[1, "set_name"]],
                            do.call("rbind", strsplit(use_names, "_")),
                            write_vcov)
    write_vcov[, 4:5] <- write_vcov[, 5:4]
    }
  writeWorksheet(wb, write_vcov,
                 sheet = "OPCPem",
                 startRow = start_row_3,
                 startCol = start_col_3, header = FALSE)
  data.frame(numeric(0))
}


prepare_fit_list_plot_data_from_tibble <-
  function(fit_tib){
    survival_summaries <- 
      fit_tib %>% 
        dplyr::group_by(type, treatment, set_name, dist) %>%
          dplyr::do(summary_helper(.$fit[[1]], type = "survival", tidy = TRUE)) %>%
            dplyr::ungroup()
    survival_summaries$fn <- "survival"
    cumhaz_summaries <- 
      fit_tib %>% 
      dplyr::group_by(type, treatment, set_name, dist) %>%
      dplyr::do(summary_helper(.$fit[[1]], type = "cumhaz", tidy = TRUE)) %>%
      dplyr::ungroup()
    cumhaz_summaries$fn <- "cumulative hazard"
    rbind(survival_summaries, cumhaz_summaries)
  }

summary_helper <- function(fit, ...){
  stopifnot(inherits(fit, c("flexsurvreg", "survfit", "surv_shift")))
    if(inherits(fit, "surv_shift")){
      res1 <- heemod:::summary.surv_shift(fit, ...)
    }
  else{
    res1 <- summary(fit, ...)
    all_times <- sort(unique(c(res1[["time"]],
                               seq(from = 1, 
                                   to = max(res1[["time"]]),
                                   by = 1))))
    res1 <- summary(fit, t = all_times, ...)
  }
  if(inherits(res1, "summary.survfit")){
    res1 <- data.frame(res1[c("time", "surv", "upper", "lower")])
    names(res1) <- c("time", "est", "lcl", "ucl")
    res1
  }
  res1
}



plot_fit_data <- function(data_to_plot, 
                          type = c("survival", "cumulative hazard"),
                          logy = FALSE,
                          scale_time = 1,
                          time_label = "time",
                          max_scaled_time = Inf,
                          title = NULL,
                          x_axis_gap){
  type <- match.arg(type)
  data_to_plot <- dplyr::filter_(data_to_plot, 
                                 lazyeval::interp(~fn == var, var = type))
  data_to_plot$time <- data_to_plot$time * scale_time
  data_to_plot <- dplyr::filter(data_to_plot, time <= max_scaled_time)
  res <- 
    ggplot2::ggplot(data_to_plot,
                    ggplot2::aes(x = time, y = est, color = dist)) + 
      ggplot2::geom_line(data = dplyr::filter(data_to_plot, dist != "km")) +
      ggplot2::geom_step(data = dplyr::filter(data_to_plot, dist == "km"), 
                         col = "black", lwd = 1.5) 
  if(logy) res <- res + ggplot2::scale_y_log10()
  if(!is.infinite(max_scaled_time) & !missing(x_axis_gap)){
    breaks <- seq(from = 0, to = max_scaled_time, by = x_axis_gap)
    res <- res + ggplot2::scale_x_continuous(breaks = breaks)
  }
  res <- res + ggplot2::labs(y = type, x = time_label, title = title)
  res
}

get_component <- function(obj, comp){
  if(comp %in% names(obj)) return(obj[[comp]])
  else{
    if("dist" %in% names(obj)) return(get_component(obj[["dist"]], comp))
    else return(NULL)
  }
}

extract_fits <- function(x) {
  if (inherits(x, "flexsurvreg"))
    x
  else{
    if ("dist" %in% names(x) && inherits(x$dist, "flexsurvreg"))
      x$dist
    else
      stop(
        "unrecognized input; not a flexsurvreg object and ",
        "doesn't contain a flexsurvreg object as 'dist'"
      )
  }
}



