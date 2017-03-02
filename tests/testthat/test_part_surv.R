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
