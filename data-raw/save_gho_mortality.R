library(rgho)
library(heemod)
library(dplyr)

countries <- get_gho_codes(dimension = "COUNTRY") %>% 
  filter_attrs(WORLD_BANK_INCOME_GROUP == "High-income")

get_latest_morta <- function(country) {
  message(country)
  mr_data <- get_gho_data(
    dimension = "GHO",
    code = "LIFE_0000000029",
    filter = list(
      COUNTRY = country
    )
  )
  
  if (nrow(mr_data) == 0) return(NULL)
  
  study_year <- max(mr_data$YEAR)
  mr_data_year <- mr_data[mr_data$YEAR == study_year, ]
  
  if (nrow(mr_data_year) != 44) {
    stop("Strange GHO mortality data.")
  }
  
  pooled_data <- heemod:::pool_data(
    mr_data_year,
    country = country, year = study_year
  )
  
  list(
    data = mr_data_year,
    year = study_year,
    pool = pooled_data
  )
}


list_morta <- lapply(countries, get_latest_morta)
names(list_morta) <- countries
list_morta <- Filter(function(x) ! is.null(x), list_morta)

devtools::use_data(
  list_morta,
  internal = TRUE,
  overwrite = TRUE
)
