context("GHO data")

test_that(
  "GHO API", {
    res_latest <- heemod:::get_who_mr_(
      age = 0:99,
      sex = rep(c("MLE", "FMLE"), 50),
      country = "FRA"
    )
    res_2015 <- heemod:::get_who_mr_(
      age = 0:99,
      sex = rep(c("MLE", "FMLE"), 50),
      country = "FRA",
      year = 2015
    )
    res_pooled <- heemod:::get_who_mr_(
      age = 0:99,
      sex = rep(c("MLE", "FMLE"), 50),
      country = "FRA",
      pool = TRUE
    )
    res_pooled_nosex <- heemod:::get_who_mr_(
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
    expect_error(get_who_mr(age = -2, sex = rep(c("MLE", "FMLE"), 50), country = "FRA"))
    expect_error(get_who_mr(age = 0:99, sex = rep(c("ML1E", "FMLE"), 50), country = "FRA"))
    expect_error(get_who_mr(age = "[00-15[", sex = rep(c("MLE", "FMLE"), 50), country = "FRA"))
    expect_error(get_who_mr(age = c(1, NA), sex = rep(c("MLE", "FMLE"), 50), country = "FRA"))
    expect_error(get_who_mr(age = 0:99, sex = rep(c("MLE", "FMLE"), 50), country = "XXXX"))
    expect_error(get_who_mr(age = 0:99, sex = rep(c("MLE", "FMLE"), 50), country = "FRA", year = 2050))
  })
