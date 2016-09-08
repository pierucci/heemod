#' Use WHO Mortality Rate
#' 
#' Returns age and sex-specific mortality probabilities for
#' a given country.
#' 
#' The results of \code{get_who_mr} are memoised for 1 hour
#' to increase resampling performance. \code{get_who_mr_} is
#' not memoised.
#' 
#' @name who-mortality
#' @param age age as a continuous variable.
#' @param sex sex as \code{"FMLE"} or \code{"MLE"}.
#' @param country Country code (see details).
#' @param year Use data from that year. Defaults to 
#'   \code{"latest"}.
#'   
#' @return This function should be used within 
#'   \code{\link{define_matrix}} or 
#'   \code{\link{define_parameters}}.
#'   
#' @examples 
#' 
#' define_matrix(
#'   C, get_who_mr(age = 50 + markov_cycle, sex = "FMLE", country = "FRA"),
#'   0, 1
#' )
#' 
get_who_mr_ <- function(age, sex, country, year = "latest") {
  mr_data <- get_gho_mr(country = country, year = as.character(year))
  
  age_gho <- trans_age_gho(age)
  sex_gho <- trans_sex_gho(sex)
  
  suppressMessages({
    dplyr::left_join(
      dplyr::data_frame(
        AGEGROUP = as.character(age_gho),
        SEX = as.character(sex_gho)
      ),
      mr_data
    )$Numeric
  })
}

#' @rdname who-mortality
#' @export
get_who_mr <- memoise::memoise(
  get_who_mr_,
  ~ memoise::timeout(3600)
)

get_gho_mr <- function(country, year) {
  gho_data <- rgho::get_gho_data(
    dimension = "GHO",
    code = "LIFE_0000000029",
    filter = list(
      COUNTRY = country
    )
  )
  
  years <- unique(gho_data$YEAR)
  
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
  
  gho_data_year <- gho_data[gho_data$YEAR == study_year, ]
  
  if (nrow(gho_data_year) != 44) {
    stop("Strange GHO mortality data.")
  }
  
  gho_data_year
}

trans_age_gho <- function(age) {
  stopifnot(
    age >= 0,
    is.numeric(age),
    ! any(is.na(age))
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
  
  cut(age, c(0, 1, seq(5, 100, 5), +Inf), right = FALSE, labels = labs)
}

trans_sex_gho <- function(sex) {
  stopifnot(
    sex %in% c("FMLE", "MLE")
  )
  sex
}

