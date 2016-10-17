#' Use WHO Mortality Rate
#' 
#' Returns age and sex-specific mortality probabilities for 
#' a given country.
#' 
#' Locally cached data is used in case of connection 
#' problems, of if \code{local = TRUE}. For memory space 
#' reasons local data is only available for WHO high-income 
#' countries, and only for the latest year.
#' 
#' The results of \code{get_who_mr} are memoised for
#' \code{options("heemod.memotime")} (default: 1 hour) to
#' increase resampling performance.
#' 
#' @name who-mortality
#' @param age age as a continuous variable.
#' @param sex sex as \code{"FMLE"}-\code{"MLE"}, 
#'   \code{0}-\code{1} (male = 0, female = 1) or 
#'   \code{1}-\code{2} (male = 1, female = 2).
#' @param country Country code (see details).
#' @param year Use data from that year. Defaults to 
#'   \code{"latest"}.
#' @param pool Pool female and male mortality rates?
#' @param local Fetch mortality data from package cached 
#'   data?
#'   
#' @return This function should be used within 
#'   \code{\link{define_transition}} or 
#'   \code{\link{define_parameters}}.
#'   
#' @examples 
#' 
#' define_transition(
#'   C, get_who_mr(age = 50 + markov_cycle, sex = "FMLE", country = "FRA"),
#'   0, 1
#' )
#' 
get_who_mr_ <- function(age, sex = NULL, country,
                        year = "latest", pool = FALSE,
                        local = FALSE) {
  if (is.null(sex) && ! pool) {
    stop("'sex' must be provided for non-pooled results.")
  }
  if (! local) {
    message("Fetching mortality data from WHO server.")
    mr_data <- try(get_gho_mr(
      country = country,
      year = as.character(year),
      pool = pool
    ), silent = TRUE)
    
    if (inherits(mr_data, "try-error"))
      message("Failed to fetch mortality data from WHO server.")
  }
  
  if (local || inherits(mr_data, "try-error")) {
    message("Fetching mortality data from package cached data.")
    mr_data <- get_package_mr(
      country = country,
      year = as.character(year),
      pool = pool
    )
  }
  
  ref_data <- dplyr::data_frame(
    AGEGROUP =  trans_age_gho(age)
  )
  
  if (! pool) {
    ref_data$SEX <- trans_sex_gho(sex)
  }
  
  suppressMessages({
    dplyr::left_join(
      ref_data,
      mr_data
    )$Numeric
  })
}

#' @rdname who-mortality
#' @export
get_who_mr <- memoise::memoise(
  get_who_mr_,
  ~ memoise::timeout(options()$heemod.memotime)
)

get_gho_mr <- function(country, year, pool) {
  mr_data <- rgho::get_gho_data(
    dimension = "GHO",
    code = "LIFE_0000000029",
    filter = list(
      COUNTRY = country
    )
  )
  
  years <- unique(mr_data$YEAR)
  
  if (year == "latest") {
    study_year <- max(years)
    message(sprintf("Using latest year: %s", study_year))
    
  } else if (! year %in% years) {
    stop(sprintf(
      "Mortality data for YEAR '%s' not available for COUNTRY '%s'.",
      year, country
    ))
  } else {
    study_year <- year
  }
  
  mr_data_year <- mr_data[mr_data$YEAR == study_year, ]
  
  if (nrow(mr_data_year) != 44) {
    stop("Strange GHO mortality data.")
  }
  
  if (pool) {
    mr_data_year <- pool_data(mr_data_year,
                              country, study_year)
  }
  
  mr_data_year
}

pool_data <- function(mr_data, country, year) {
  pop_data <- rgho::get_gho_data(
    dimension = "GHO",
    code = "LIFE_0000000031",
    filter = list(
      COUNTRY = country,
      YEAR = year
    )
  )
  
  if (nrow(pop_data) == 0) {
    stop("No population structure for the selected year, cannot pool rates.")
  }
  if (nrow(pop_data) != 44) {
    stop("Strange population structure data.")
  }
  
  suppressMessages({
    pop_data %>% 
      dplyr::select_(
        "AGEGROUP", "SEX",
        weight = ~ Numeric
      ) %>% 
      dplyr::left_join(mr_data) %>% 
      dplyr::group_by_("AGEGROUP") %>% 
      dplyr::summarise_(
        Numeric = ~ sum(Numeric * weight) / sum(weight)
      )
  })
}

get_package_mr <- function(country, year, pool) {
  if (! country %in% names(list_morta)) {
    stop(sprintf(
      "No local data available for country '%s'.",
      country
    ))
  }
  
  if (year != "latest" && year != list_morta[[country]]$year) {
    stop(sprintf(
      "No local data available for specified year (specified: %s, available: %s).",
      year,
      list_morta[[country]]$year
    ))
  }
  message(sprintf(
    "Using cached data from year %s.",
    list_morta[[country]]$year
  ))
  
  if (pool) {
    list_morta[[country]]$pool
  } else {
    list_morta[[country]]$data
  }
}

trans_age_gho <- function(age) {
  stopifnot(! is.null(age))
  stopifnot(
    age >= 0,
    is.numeric(age),
    ! any(is.na(age)),
    length(age) > 0
  )
  labs <- c(
    "AGELT1", "AGE1-4", "AGE5-9",
    "AGE10-14", "AGE15-19", "AGE20-24",
    "AGE25-29", "AGE30-34", "AGE35-39",
    "AGE40-44", "AGE45-49", "AGE50-54",
    "AGE55-59", "AGE60-64", "AGE65-69",
    "AGE70-74", "AGE75-79", "AGE80-84",
    "AGE85-89", "AGE90-94", "AGE95-99",
    "AGE100PLUS"
  )
  
  cut(
    age,
    c(0, 1, seq(5, 100, 5), +Inf),
    right = FALSE,
    labels = labs
  ) %>% 
    as.character()
}

trans_sex_gho <- function(sex) {
  stopifnot(! is.null(sex))
  u_sex <- sort(unique(sex))
  
  if (length(u_sex) > 2)
    stop("More than 2 sex modalities.")
  
  stopifnot(
    all(u_sex %in% c("FMLE", "MLE")) ||
      all(u_sex %in% 0:1) ||
      all(u_sex %in% 1:2),
    ! any(is.na(sex)),
    length(sex) > 0
  )
  
  if (all(u_sex == 1)) {
    stop("All values of 'sex' are equal to 1, this format is ambiguous.")
  }
  
  if (all(u_sex %in% c("FMLE", "MLE"))) {
    return(as.character(sex))
    
  } else if (all(u_sex %in% 0:1) || all(u_sex %in% 1:2)) {
    message("Converting sex values (male = 0, female = 1) or (male = 1, female = 2).")
    sex %>% 
      factor(labels = c("MLE", "FMLE")) %>% 
      as.character() %>% 
      return()
    
  } else {
    stop("Error during conversion of labels for 'sex'.")
  }
}

