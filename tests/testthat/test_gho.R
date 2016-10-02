context("GHO data")

test_that(
  "GHO API", {
    res_latest <- get_who_mr(
      age = 0:99,
      sex = rep(c("MLE", "FMLE"), 50),
      country = "FRA"
    )
    res_2015 <- get_who_mr(
      age = 0:99,
      sex = rep(c("MLE", "FMLE"), 50),
      country = "FRA",
      year = 2015
    )
    res_pooled <- get_who_mr(
      age = 0:99,
      sex = rep(c("MLE", "FMLE"), 50),
      country = "FRA",
      pool = TRUE
    )
    res_pooled_nosex <- get_who_mr(
      age = 0:99,
      country = "FRA",
      pool = TRUE
    )
    
    expect_identical(
      head(res_latest),
      c(0.00391, 0.00018, 0.00023, 0.00018, 0.00023, 7e-05)
    )
    expect_identical(
      res_latest,
      res_2015
    )
    expect_identical(
      res_pooled,
      res_pooled_nosex
    )
    expect_identical(
      round(head(res_pooled), 5),
      c(0.00356, 2e-04, 2e-04, 2e-04, 2e-04, 7e-05)
    )
    expect_error(
      get_who_mr(age = -2, sex = rep(c("MLE", "FMLE"), 50),
                 country = "FRA"))
    expect_error(
      get_who_mr(age = 0:99, sex = rep(c("ML1E", "FMLE"), 50),
                 country = "FRA"))
    expect_error(
      get_who_mr(age = "[00-15[", sex = rep(c("MLE", "FMLE"), 50),
                 country = "FRA"))
    expect_error(
      get_who_mr(age = c(1, NA), sex = rep(c("MLE", "FMLE"), 50),
                 country = "FRA"))
    expect_error(
      get_who_mr(age = 0:99, sex = rep(c("MLE", "FMLE"), 50),
                 country = "XXXX"))
    expect_error(
      get_who_mr(age = 0:99, sex = rep(c("MLE", "FMLE"), 50),
                 country = "FRA", year = 2050))
    expect_error(
      get_who_mr(
        age = 0:99,
        country = "FRA"
      )
    )
  })

test_that(
  "Conversion functions work", {
    sex_char <- c("MLE", "FMLE", "FMLE", "MLE", "FMLE")
    sex_01 <- c(0, 1, 1, 0, 1)
    sex_12 <- c(1, 2, 2, 1, 2)
    expect_identical(
      heemod:::trans_sex_gho(sex_char),
      sex_char
    )
    expect_identical(
      heemod:::trans_sex_gho(sex_01),
      sex_char
    )
    expect_identical(
      heemod:::trans_sex_gho(sex_12),
      sex_char
    )
    expect_error(
      heemod:::trans_sex_gho(c(1, NA))
    )
    expect_error(
      heemod:::trans_sex_gho(numeric())
    )
    expect_error(
      heemod:::trans_sex_gho(1:3)
    )
    expect_error(
      heemod:::trans_sex_gho(3:4)
    )
    expect_error(
      heemod:::trans_sex_gho(c("xxx"))
    )
    expect_error(
      heemod:::trans_sex_gho(NULL)
    )
    
    age <- 0:101
    age_res <- c(
      "AGELT1", "AGE1-4", "AGE1-4", "AGE1-4", "AGE1-4", "AGE5-9", 
      "AGE5-9", "AGE5-9", "AGE5-9", "AGE5-9", "AGE10-14", "AGE10-14", 
      "AGE10-14", "AGE10-14", "AGE10-14", "AGE15-19", "AGE15-19", "AGE15-19", 
      "AGE15-19", "AGE15-19", "AGE20-24", "AGE20-24", "AGE20-24", "AGE20-24", 
      "AGE20-24", "AGE25-29", "AGE25-29", "AGE25-29", "AGE25-29", "AGE25-29", 
      "AGE30-34", "AGE30-34", "AGE30-34", "AGE30-34", "AGE30-34", "AGE35-39", 
      "AGE35-39", "AGE35-39", "AGE35-39", "AGE35-39", "AGE40-44", "AGE40-44", 
      "AGE40-44", "AGE40-44", "AGE40-44", "AGE45-49", "AGE45-49", "AGE45-49", 
      "AGE45-49", "AGE45-49", "AGE50-54", "AGE50-54", "AGE50-54", "AGE50-54", 
      "AGE50-54", "AGE55-59", "AGE55-59", "AGE55-59", "AGE55-59", "AGE55-59", 
      "AGE60-64", "AGE60-64", "AGE60-64", "AGE60-64", "AGE60-64", "AGE65-69", 
      "AGE65-69", "AGE65-69", "AGE65-69", "AGE65-69", "AGE70-74", "AGE70-74", 
      "AGE70-74", "AGE70-74", "AGE70-74", "AGE75-79", "AGE75-79", "AGE75-79", 
      "AGE75-79", "AGE75-79", "AGE80-84", "AGE80-84", "AGE80-84", "AGE80-84", 
      "AGE80-84", "AGE85-89", "AGE85-89", "AGE85-89", "AGE85-89", "AGE85-89", 
      "AGE90-94", "AGE90-94", "AGE90-94", "AGE90-94", "AGE90-94", "AGE95-99", 
      "AGE95-99", "AGE95-99", "AGE95-99", "AGE95-99", "AGE100PLUS", 
      "AGE100PLUS"
    )
    
    expect_identical(
      heemod:::trans_age_gho(age),
      age_res
    )
    expect_error(
      heemod:::trans_age_gho(-1)
    )
    expect_error(
      heemod:::trans_age_gho(c(NA, 1))
    )
    expect_error(
      heemod:::trans_age_gho(numeric())
    )
    expect_error(
      heemod:::trans_age_gho(NULL)
    )
  }
)

test_that(
  "Local data works", {
    mr_aus_p <- get_who_mr(10, sex = "FMLE", country = "AUS",
                         local = TRUE, pool = TRUE)
    mr_aus <- get_who_mr(10, sex = "FMLE", country = "AUS",
                           local = TRUE)
    
    expect_identical(
      signif(mr_aus_p),
      8.9996e-05
    )
    expect_identical(
      signif(mr_aus),
      8e-05
    )
    expect_error(
      get_who_mr(10, sex = "FMLE", country = "AZE",
                 local = TRUE)
    )
    expect_error(
      get_who_mr(10, sex = "FMLE", country = "AUS",
                 local = TRUE, year = 2014)
    )
  }
)
