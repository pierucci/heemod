context("Partitioned Survival Model")

surv_dist1 <- define_survival(
  distribution = "exp",
  rate = .003
)
surv_dist2 <- define_survival(
  distribution = "exp",
  rate = .002
)
surv_dist3 <- define_survival(
  distribution = "exp",
  rate = .005
)
surv_dist4 <- define_survival(
  distribution = "exp",
  rate = .004
)


suppressMessages(
ps <- define_part_surv(
  pfs = project(surv_dist3, surv_dist4, at=365),
  os = project(surv_dist1, surv_dist2, at=365),
  cycle_length = c(365, 365)
)
)
suppressMessages(
ps1 <- define_part_surv(
  pfs = project(surv_dist3, surv_dist4, at=365) %>% apply_hr(0.8),
  os = project(surv_dist1, surv_dist2, at=365) %>% apply_hr(0.8),
  cycle_length = c(365, 365)
)
)

sA <-  define_state(
  cost = 10000, ut = 1
)
sB <-  define_state(
  cost = 20000, ut = 1
)
sC <-  define_state(
  cost = 0, ut = 0
)

stratPS <- define_strategy(
  transition = ps,
  A = sA, B = sB, C = sC
)

stratPS1 <- define_strategy(
  transition = ps1,
  A = sA, B = sB, C = sC
)
resPS <- run_model(
  Strat1 = stratPS,
  Strat2 = stratPS1,
  cycles = 10,
  cost = cost,
  effect = ut,
  method = "end"
)

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

ps <- define_part_surv(
  pfs = project(surv_dist3),
  os = project(surv_dist1),
  state_names = c("ProgressionFree", "Progressive", "Death"),
  cycle_length = c(365, 365)
)

test_that("errors with inappropriate state names",
          {
            expect_error(
              ps <- define_part_surv(
                pfs = project(surv_dist3),
                os = project(surv_dist1),
                state_names = c("NoDisease", "Progressive", "Death"),
                cycle_length = c(365, 365)
              ),
              "progression free state (only) must have 'free' in its name",
              fixed = TRUE
            )
            expect_error(
              ps <- define_part_surv(
                pfs = project(surv_dist3),
                os = project(surv_dist1),
                state_names = c("ProgressionFree", "Progressive", "Kaput"),
                cycle_length = c(365, 365)
              ),
              "state representing death",
              fixed = TRUE
            )
            expect_error(
              ps <- define_part_surv(
                pfs = project(surv_dist3),
                os = project(surv_dist1),
                state_names = c("ProgressionFree", "Progressive",
                                "uh-oh", "Death"),
                cycle_length = c(365, 365)
              ),
              "if there are four states, you must have a state called 'terminal'",
              fixed = TRUE
            )
            expect_error(
              ps <- define_part_surv(
                pfs = project(surv_dist3),
                os = project(surv_dist1),
                state_names = c(
                  "ProgressionFree",
                  "Progressivebutfree",
                  "terminal",
                  "Death"
                ),
                cycle_length = c(365, 365)
              ),
              "progression free state (only) must have 'free' in its name",
              fixed = TRUE
            )
          })
