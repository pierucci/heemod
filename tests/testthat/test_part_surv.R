context("Partitioned Survival Model")

test_that(
  "part surv works", {
    
    surv_dist_1 <- define_survival(
      distribution = "exp", rate = 0.5)
    fit_cov <- flexsurv::flexsurvreg(
      formula = survival::Surv(rectime, censrec) ~ group,
      dist = "weibull", 
      data = flexsurv::bc)
    fitcov_medium <- set_covariates(fit_cov, group = "Medium")
    km_cov <- survival::survfit(
      formula = survival::Surv(rectime, censrec) ~ group,
      data = flexsurv::bc)
    km_medium <- set_covariates(km_cov, group = "Medium")
    
    ps <- define_part_surv(
      pfs = surv_dist_1,
      os = km_medium %>%
        project(fitcov_medium, 
                at = 730),
      cycle_length = c(1, 365)  # 1 for pfs, 365 for os
    )
    
    sA <- define_state(cost = 10, ut = 1)
    sB <- define_state(cost = 20, ut = 0.5)
    sC <- define_state(cost = 0, ut = 0)
    
    stratPS <- define_strategy(transition = ps, A = sA, B = sB, C = sC)
    
    param <- define_parameters(
      p1 = compute_surv(
        surv_dist_1,
        time = model_time  # can also be state_time
      ),
      p2 = compute_surv(
        km_medium %>%
          project(fitcov_medium, 
                  at = 730),
        time = model_time, cycle_length = 365  # time is in days in km_medium, in years in model_time
      ))
    
    tm <- define_transition(
      1 - p1, C, p2,
      0, C, p2,
      0, 0, 1)
    #> No named state -> generating names.
    
    stratTM <- define_strategy(
      transition = tm, A = sA, B = sB, C = sC)
    
    suppressWarnings({
      resPS <- run_model(
        stratPS,
        cycles = 10)
    })
    suppressWarnings({
      resTM <- run_model(
        parameters = param,
        stratTM,
        cycles = 10)
    })
    
    expect_equal(
      get_counts(resPS)$count,
      get_counts(resTM)$count
    )
    
    suppressWarnings({
      resPS <- run_model(
        stratPS,
        cycles = 10,
        method = "end")
    })
    suppressWarnings({
      resTM <- run_model(
        parameters = param,
        stratTM,
        cycles = 10,
        method = "end")
    })
    
    expect_equal(
      get_counts(resPS)$count,
      get_counts(resTM)$count
    )
  }
)
