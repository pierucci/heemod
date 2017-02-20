context("Partitioned Survival Model")

surv_dist1 <- define_survival(
  distribution = "exp",
  rate = .03
)
surv_dist2 <- define_survival(
  distribution = "exp",
  rate = .02
)
surv_dist3 <- define_survival(
  distribution = "exp",
  rate = .05
)
surv_dist4 <- define_survival(
  distribution = "exp",
  rate = .04
)


ps <- define_part_surv(
  pfs = project(surv_dist3, surv_dist4, at=365),
  os = project(surv_dist1, surv_dist2, at=365),
  cycle_length = c(365, 365) # 1 for pfs, 365 for os
)

ps1 <- define_part_surv(
  pfs = project(surv_dist3, surv_dist4, at=365) %>% apply_hr(0.8),
  os = project(surv_dist1, surv_dist2, at=365) %>% apply_hr(0.8),
  cycle_length = c(365, 365) # 1 for pfs, 365 for os
)
sA <-  define_state(
  cost = 10, ut = 1
)
sB <-  define_state(
  cost = 20, ut = .5
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
  stratPS,
  stratPS1,
  cycles = 10,
  cost = sA,
  effect = sB
)
