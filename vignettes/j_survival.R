## ----setup, include=FALSE------------------------------------------------
library(heemod)

## ------------------------------------------------------------------------
surv_dist_1 <- define_survival(
  distribution = "exp",
  rate = .5
)

surv_dist_2 <- define_spline_survival(
  scale = "odds",
  gamma = c(-11.643, 1.843, 0.208),
  knots = c(4.077537, 5.883183, 6.458338)
)

## ----fig.width=6, fig.height=6-------------------------------------------
library(flexsurv)

fit_w <- flexsurvreg(
  formula = Surv(futime, fustat) ~ 1,
  data = ovarian, dist = "weibull"
)
plot(fit_w)
fit_spl <- flexsurvspline(
  formula = Surv(futime, fustat) ~ 1,
  data = ovarian,
  scale = "odds",
  k=1
)
plot(fit_spl)

## ----fig.width=6, fig.height=6-------------------------------------------
fit_cov <- flexsurvreg(
  formula = Surv(rectime, censrec) ~ group,
  dist = "weibull",
  data = bc
)
plot(fit_cov)
fitcov_good   <- set_covariates(fit_cov, group = "Good")
fitcov_medium <- set_covariates(fit_cov, group = "Medium")
fitcov_poor   <- set_covariates(fit_cov, group = "Poor")

## ----fig.width=6, fig.height=6-------------------------------------------
library(survival)

km_1 <- survfit(
  formula = Surv(futime, fustat) ~ 1,
  data = ovarian
)
km_cov <- survfit(
  formula = Surv(rectime, censrec) ~ group,
  data = bc
)
plot(km_cov)
km_good   <- set_covariates(km_cov, group = "Good")
km_medium <- set_covariates(km_cov, group = "Medium")
km_poor   <- set_covariates(km_cov, group = "Poor")

## ------------------------------------------------------------------------
km_poor_ph <- apply_hr(km_poor, hr = 0.5)
km_medium_af <- apply_af(km_medium, af = 1.2)

## ------------------------------------------------------------------------
km_poor_join <- join(
  km_poor,
  fitcov_poor,
  at = 365
)
models_all <- mix(
  fitcov_good, fitcov_medium, fitcov_poor,
  weights = c(0.25, 0.25, 0.5)
)
combined_risks <- add_hazards(
  fit_w, fitcov_good
)

## ------------------------------------------------------------------------
compute_surv(surv_dist_2, time = 1:5)

## ------------------------------------------------------------------------
fit_cov %>% 
  set_covariates(group = "Good") %>% 
  apply_hr(hr = 2) %>% 
  join(
    fitcov_poor,
    at = 3
  ) %>%
  mix(
    fitcov_medium,
    weights = c(0.25, 0.75)
  ) %>%
  add_hazards(
    fit_w
  ) %>% 
  compute_surv(time = 1:5)

## ----fig.width=6, fig.height=6-------------------------------------------
param <- define_parameters(
  p1 = compute_surv(
    surv_dist_1,
    time = model_time # can also be state_time
  ),
  p2 = km_1 %>%
    join(fit_w, at = 730) %>%
    compute_surv(
      time = model_time,
      cycle_length = 365  # time is in days in km_medium, in years in model_time
    )
)

tm <- define_transition(
  C, p1 - p2, p2,
  0, C,       p2,
  0, 0,       C
)

plot(tm)

sA <-  define_state(
  cost = 10, ut = 1
)
sB <-  define_state(
  cost = 20, ut = .5
)
sC <-  define_state(
  cost = 0, ut = 0
)

stratTM <- define_strategy(
  transition = tm,
  A = sA, B = sB, C = sC
)

resTM <- run_model(
  parameters = param,
  stratTM,
  cycles = 15,
  cost = cost, effect = ut
)

## ----fig.width=6, fig.height=4-------------------------------------------
plot(resTM)

## ----fig.width=6, fig.height=4-------------------------------------------
ps <- define_part_surv(
  pfs = surv_dist_1,
  os  = km_1 %>%
    join(fit_w, at = 730),
  cycle_length = c(1, 365) # 1 for pfs, 365 for os
)

stratPS <- define_strategy(
  transition = ps,
  A = sA, B = sB, C = sC
)

resPS <- run_model(
  stratPS,
  cycles = 15,
  cost = cost, effect = ut
)

plot(resPS)

