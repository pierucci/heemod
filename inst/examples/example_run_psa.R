# example for run_psa

mod1 <- define_strategy(
  transition = define_transition(
    .5, .5,
    .1, .9
  ),
  define_state(
    cost = cost_init + age * 5,
    ly = 1
  ),
  define_state(
    cost = cost_init + age,
    ly = 0
  )
)

mod2 <- define_strategy(
  transition = define_transition(
    p_trans, C,
    .1, .9
  ),
  define_state(
    cost = 789 * age / 10,
    ly = 1
  ),
  define_state(
    cost = 456 * age / 10,
    ly = 0
  )
  
)

res2 <- run_model(
  mod1, mod2,
  parameters = define_parameters(
    age_init = 60,
    cost_init = 1000,
    age = age_init + markov_cycle,
    p_trans = .7
  ),
  init = 1:0,
  cycles = 10,
  cost = cost,
  effect = ly
)

rsp <- define_psa(
  age_init ~ normal(60, 10),
  cost_init ~ normal(1000, 100),
  p_trans ~ prop(.7, 100),
  correlation = matrix(c(
    1,  .4, 0,
    .4, 1,  0,
    0,  0,  1
  ), byrow = TRUE, ncol = 3)
)


# with run_model result
# (only 10 resample for speed)
ndt1 <- run_psa(res2, resample = rsp, N = 10)
