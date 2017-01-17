context("Calculation of probabilities for survival models")
library(flexsurv)

## simple data where half of participants
## die at time 10, rest are censored at time 20
test_dat <- data.frame(
  time = rep(c(10, 20), each = 50),
  status = rep(c(1, 0), each = 50),
  group = rep(1, 100)
)

test_fit <- flexsurv::flexsurvreg(
  survival::Surv(time, status) ~ 1,
  data = test_dat,
  dist = "exp")
bad_test_fit <- test_fit
class(bad_test_fit) <- "random"

km_pred <- data.frame(time = 0, method = "km")

km_probs <- get_probs_from_surv(test_fit,
                                km_limit = 100,
                                cycle = 1:20)
fit_probs <- get_probs_from_surv(test_fit,
                                 km_limit = 0,
                                 cycle = 1:20)

dist_1 <- define_survival(distribution = "exp", rate = 1/5)
dist_prob_1 <- get_probs_from_surv(dist_1, km_limit = 0, cycle = 1:5)
dist_2 <- define_survival(distribution = "gamma", shape = 3, rate = 2)
dist_prob_2 <- get_probs_from_surv(dist_2, km_limit = 0, cycle = 1:5)


test_that("Probabilities are calculated correctly", {
  expect_equal(km_probs, rep(c(0, 0.5, 0),
                             c(9, 1, 10)))
  expect_equal(round(fit_probs, 4),
               rep(0.0328, 20))
  expect_equal(round(dist_prob_1, 6), 
               rep(0.181269, 5))
  expect_equal(round(dist_prob_2, 6),
               c(0.323324, 0.648128, 0.739740, 0.778050, 0.798648))
  })

test_that("input errors are caught", {
  expect_error(
    get_probs_from_surv(list(),
                        km_limit = 100,
                        cycle = 1:20)
  )
  expect_error(
      define_survival(distribution = "exp",
                      shape = 2, rate = 0.01),
      "Incorrect argument"
  )
  
  expect_error(
      define_survival(
        distribution = "gamma",
        shape = 2, random = 0.01),
      "Incorrect argument"
  )
  
  expect_error(
    define_survival(
      distribution = "gamma",
      shape = 2, 0.01),
    "all arguments to the distribution must be named"
  )
  
  expect_error(
    get_probs_from_surv(
      define_survival(
        distribution = "lnorm",
        mean = 2, sdlog = 0.01),
      km_limit = 100,
      cycle = 1:20)
  )
  
  expect_error(
    get_probs_from_surv(
      bad_test_fit,
      km_limit = 10,
      cycle = 1:20
    ),
    "no applicable method"
  )
  expect_error(
    get_probs_from_surv(
      test_fit,
      km_limit = 10,
      cycle = 0:20
    ),
    "all(cycle > 0) is not TRUE", fixed = TRUE
  )
  expect_error(
    get_probs_from_surv(
      test_fit,
      km_limit = 10,
      cycle = 1:20,
      cycle_length = -1
    ),
    "cycle_length > 0 is not TRUE", fixed = TRUE
  )
  expect_error(
    get_probs_from_surv(
      test_fit,
      km_limit = -1,
      cycle = 1:20
    ),
    "km_limit >= 0 is not TRUE", fixed = TRUE
  )
})
