#' Look Up Values in a Data Frame
#' 
#' A convenience function to easily look for values in a 
#' data frame.
#' 
#' This function is mostly used to extract population 
#' informations (such as mortality rates), given some 
#' individual caracteristics.
#' 
#' If binning is activated, numeric individual
#' characteristics are matched to the corresponding
#' reference value that is directly inferior.
#' 
#' @param data A reference data frame.
#' @param value The value to extract ffrom the reference 
#'   data frame.
#' @param ... Individual characteristics, should be named 
#'   like the columns of \code{data}.
#' @param bin Should numeric variable be binned? (see 
#'   examples).
#'   
#' @return A vector of values, same lenght as \code{...}.
#' @export
#' 
#' @example inst/examples/example_look_up.R
look_up <- function(data, value, ..., bin = FALSE) {
  data <- data %>% 
    mutate_if(is.factor, "as.character")
  
  list_specs <- list(...)
  
  if (any(names(list_specs) == "")) {
    stop("All arguments to 'look_up()' must be named.")
  }
  
  df_vars <- do.call(
    tibble::tibble,
    list_specs
  )
  
  if (! all(pb <- names(df_vars) %in% names(data))) {
    stop(sprintf(
      "Names passed to 'look_up()' not found in data: %s.",
      paste(names(df_vars)[pb], collapse = ", ")
    ))
  }
  
  if (bin) {
    num_cols <- unlist(Map(is.numeric, df_vars))
    for (n in names(num_cols)[num_cols]) {
      bin_values <- c(sort(unique(data[[n]])), +Inf)
      data[[n]] <- cut(data[[n]], bin_values,
                       include.lowest = TRUE,
                       right = FALSE)
      df_vars[[n]] <- cut(df_vars[[n]], bin_values,
                          include.lowest = TRUE,
                          right = FALSE)
    }
  }
  
  res <- suppressMessages(
    dplyr::left_join(
      df_vars,
      data
    )[[value]]
  )
  
  if (length(res) != nrow(df_vars)) {
    stop("Ooops, something unexpected went wrong...")
  }
  
  res
}