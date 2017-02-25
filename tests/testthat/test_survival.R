context("Survival analysis-to-Markov model tests")

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


test_that("Probabilities are calculated correctly", {
  expect_equal(km_probs, rep(c(0, 0.5, 0),
                             c(9, 1, 10)))
  expect_equal(round(fit_probs, 4),
               rep(0.0328, 20))
  
  su <- define_survival(
    distribution = "weibull",
    shape = 1.5,
    scale = 5
  )
  
  expect_identical(
    get_probs_from_surv(su, cycle = 1:2),
    c(
      get_probs_from_surv(su, cycle = 1),
      get_probs_from_surv(su, cycle = 2)
    )
  )
  
  times = seq(from=10,to=100,by=10)
  
  llScale = 1.3
  llShape = 0.6
  
  mu = 1.5
  sigma = 0.45
  P = 0.25
  Q = 0.1
  
  gammas = c(-18.3122, 2.7511, 0.2292)
  knots = c(4.276666, 6.470800, 7.806289)
  
  hSplineDist = define_spline_survival(
    gamma = gammas,
    knots = knots,
    scale="hazard"
  )
  
  expect_equal(
    get_probs_from_surv(hSplineDist, times, type="surv"),
    flexsurv::psurvspline(times, gamma = gammas, knots = knots, scale="hazard", lower.tail=F)
  )
  
  nSplineDist = define_spline_survival(
    gamma = gammas,
    knots = knots,
    scale="normal"
  )
  
  expect_equal(
    get_probs_from_surv(nSplineDist, times, type="surv"),
    flexsurv::psurvspline(times, gamma = gammas, knots = knots, scale="normal", lower.tail=F)
  )
  
  oSplineDist = define_spline_survival(
    gamma = gammas,
    knots = knots,
    scale="odds"
  )
  
  expect_equal(
    get_probs_from_surv(oSplineDist, times, type="surv"),
    flexsurv::psurvspline(times, gamma = gammas, knots = knots, scale="odds", lower.tail=F)
  )
  
  genfSurvDist = define_survival(
    distribution = "genf",
    mu = mu,
    sigma = sigma,
    P = P,
    Q = Q
  )
  
  expect_equal(
    get_probs_from_surv(genfSurvDist, times, type="surv"),
    flexsurv::pgenf(times, mu=mu, sigma=sigma, P=P, Q=Q, lower.tail=F)
  )
  
  llSurvDist = define_survival(
    distribution = "llogis",
    shape = llShape,
    scale = llScale
  )
  
  expect_equal(
    get_probs_from_surv(llSurvDist, times, type="surv"),
    flexsurv::pllogis(times, shape=llShape, scale=llScale, lower.tail=F)
  )
  
})

test_that("input errors are caught", {
  expect_error(
    get_probs_from_surv(list(),
                        km_limit = 100,
                        cycle = 1:20)
  )
  expect_error(
    get_probs_from_surv(
      define_survival(distribution = "exp",
                      shape = 2, rate = 0.01),
      km_limit = 100,
      cycle = 1:20)
  )
  expect_error(
    get_probs_from_surv(
      define_survival(
        distribution = "gamma",
        shape = 2, random = 0.01),
      km_limit = 100,
      cycle = 1:20)
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
    )
  )
  expect_error(
    get_probs_from_surv(
      test_fit,
      km_limit = 10,
      cycle = 0:20
    )
  )
  expect_error(
    get_probs_from_surv(
      test_fit,
      km_limit = 10,
      cycle = 1:20,
      cycle_length = -1
    )
  )
  expect_error(
    get_probs_from_surv(
      test_fit,
      km_limit = -1,
      cycle = 1:20
    )
  )
})
