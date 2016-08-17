context("GHO data")

test_that(
  "GHO API", {
    res_latest <- heemod:::get_who_mr_(
      age = 0:99,
      sex = rep(c("MLE", "FMLE"), 50),
      country = "FRA"
    )
    res_2013 <- heemod:::get_who_mr_(
      age = 0:99,
      sex = rep(c("MLE", "FMLE"), 50),
      country = "FRA",
      year = 2015
    )
    
    expect_output(
      print(head(res_latest)),
      "0.00391 0.00018 0.00023 0.00018 0.00023 0.00007"
    )
    expect_identical(
      res_latest,
      res_2013
    )
    expect_error(get_who_mr(age = -2, sex = rep(c("MLE", "FMLE"), 50), country = "FRA"))
    expect_error(get_who_mr(age = 0:99, sex = rep(c("ML1E", "FMLE"), 50), country = "FRA"))
    expect_error(get_who_mr(age = "[00-15[", sex = rep(c("MLE", "FMLE"), 50), country = "FRA"))
    expect_error(get_who_mr(age = c(1, NA), sex = rep(c("MLE", "FMLE"), 50), country = "FRA"))
    expect_error(get_who_mr(age = 0:99, sex = rep(c("MLE", "FMLE"), 50), country = "XXXX"))
    expect_error(get_who_mr(age = 0:99, sex = rep(c("MLE", "FMLE"), 50), country = "FRA", year = 2050))
  })
