#' Use WHO Mortality Rate
#' 
#' Returns age and sex-specific mortality probabilities for 
#' a given country.
#' 
#' Locally cached data is used in case of connection 
#' problems, of if `local = TRUE`. For memory space reasons
#' local data is only available for WHO high-income 
#' countries, and only for the latest year.
#' 
#' The results of `get_who_mr` are memoised for 
#' `options("heemod.memotime")` (default: 1 hour) to 
#' increase resampling performance.
#' 
#' @name who_mortality
#' @param age age as a continuous variable.
#' @param sex sex as `"FMLE"`-`"MLE"`, `0`-`1` (male = 0,
#'   female = 1) or `1`-`2` (male = 1, female = 2).
#' @param region Region code. Assumed `NULL` if provided along
#'   with `country`.
#' @param country Country code (see details).
#' @param year Use data from that year. Defaults to 
#'   `"latest"`.
#' @param local Fetch mortality data from package cached 
#'   data?
#'   
#' @return This function should be used within 
#'   [define_transition()] or [define_parameters()].
#'   
#' @examples 
#' 
#' define_transition(
#'   C, get_who_mr(age = 50 + markov_cycle, sex = "FMLE", country = "FRA"),
#'   0, 1
#' )
#' 
get_who_mr_memo <- function(age, sex = NULL, region = NULL, country = NULL,
                            year = "latest", local = FALSE) {

  if (is.null(region) && is.null(country)){
    message("Assuming 'region' is GLOBAL.")
    region <- "GLOBAL"
    
  } else if (! is.null(country)) {
    if (! is.null(region)) {
      message("'country' provided, ignoring 'region' argument.")
    }
    region <- NULL
  }
  
  if (!local) {
    message("Fetching mortality data from WHO server.")
    mr_data <- try(get_gho_mr(
      sex = sex,
      region = region,
      country = country,
      year = as.character(year)
    ), silent = TRUE)
    
    if (inherits(mr_data, "try-error")) {
      warning("Failed to fetch mortality data from WHO server.")
    }
  }
  
  if (local || inherits(mr_data, "try-error")) {
    if (is.null(country)) {
      stop("Regional mortality rates cannot be estimated from local data.")
    }
    message("Fetching mortality data from package cached data.")
    mr_data <- get_package_mr(
      country = country,
      year = as.character(year),
      pool = is.null(sex)
    )
  }
  
  ref_data <- tibble(
    AGEGROUP =  trans_age_gho(age)
  )
  
  if (!is.null(sex)) {
    ref_data$SEX <- trans_sex_gho(sex)
  }
  if (!is.null(country)) {
    ref_data$COUNTRY <- country
  }

  suppressMessages({
    dplyr::left_join(
      ref_data,
      mr_data
    )$Numeric
  })
}

#' @rdname who_mortality
#' @export
get_who_mr <- memoise::memoise(
  get_who_mr_memo,
  ~ memoise::timeout(options()$heemod.memotime)
)

get_gho_mr <- function(sex, region, country, year) {
  
  if (! requireNamespace("rgho")) {
    stop("'rgho' package required to fetch data from WHO servers.")
  }
  
  mr_data <- rgho::get_gho_data(
    dimension = "GHO",
    code = "LIFE_0000000029",
    filter = as.list(
      c(
        REGION = if (!is.null(region)) region,
        COUNTRY = if (!is.null(country)) country
      )
    ) 
  )
  
  years <- unique(mr_data$YEAR)
  
  if (year == "latest") {
    study_year <- max(years)
    message(sprintf("Using latest year: %s.", study_year))
    
  } else if (!year %in% years) {
    stop(sprintf(
      "Mortality data for YEAR '%s' not available.",
      year
    ))
  } else {
    study_year <- year
  }
  
  mr_data_year <- mr_data[mr_data$YEAR == study_year, ]
  
  if (nrow(mr_data_year) %% 44 != 0) {
    stop("Strange GHO mortality data.")
  }
  
  if (is.null(country) || is.null(sex)) {
    mr_data_year <- pool_data(
      mr_data_year,
      sex,
      region, 
      country, 
      study_year)
  }
  
  mr_data_year
}

pool_data <- function(mr_data, sex, region, country, year) {
  pop_data <- rgho::get_gho_data(
    dimension = "GHO",
    code = "LIFE_0000000031",
    filter = as.list(
      c(
        YEAR = year,
        REGION = if(! is.null(region)) region,
        COUNTRY = if (! is.null(country)) country
      )
    ) 
  )
  
  if (nrow(pop_data) == 0) {
    stop("No population structure for the selected year, cannot pool rates.")
  }
  if (nrow(pop_data) %% 44 != 0) {
    stop("Strange population structure data.")
  }
  exists_col_country <- "COUNTRY" %in% colnames(pop_data)
  cols <- c("AGEGROUP", "SEX", "REGION", if (exists_col_country) "COUNTRY")

  suppressMessages({
    pop_weight <- pop_data %>% 
      dplyr::select(
        .dots = c(cols, weight = "Numeric")
      ) %>% 
      dplyr::left_join(mr_data)
    
    if (exists_col_country && length(unique(pop_weight$COUNTRY)) > 1){
      pop_weight <- dplyr::filter(pop_weight, !is.na(COUNTRY))
    }
    
    
    pop_group <- if ((is.null(country) || !exists_col_country) && is.null(sex)) {
      dplyr::group_by_(pop_weight, "AGEGROUP")
      
    } else if (is.null(sex)){
      dplyr::group_by_(pop_weight, "AGEGROUP", "COUNTRY")
      
    } else if (is.null(country) | !exists_col_country){
      dplyr::group_by_(pop_weight, "AGEGROUP", "SEX")
    }
    
    dplyr::summarise(
        pop_group,
        Numeric = sum(Numeric * weight) / sum(weight)
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
