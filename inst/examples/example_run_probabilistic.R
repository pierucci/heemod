# example for run_probabilistic

mod1 <-
  define_model(
    parameters = define_parameters(
      age_init = 60,
      cost_init = 1000,
      age = age_init + markov_cycle
    ),
    transition_matrix = define_matrix(
      .5, .5,
      .1, .9
    ),
    define_state(
      cost = cost_init + age * 5
    ),
    define_state(
      cost = cost_init + age
    )
  )

# running several models
mod2 <-
  define_model(
    parameters = define_parameters(
      age_init = 60,
      age = age_init + markov_cycle
    ),
    transition_matrix = define_matrix(
      .5, .5,
      .1, .9
    ),
    define_state(
      cost = 789 * age / 10
    ),
    define_state(
      cost = 456 * age / 10
    )
    
  )

res2 <- run_model(
  mod1, mod2,
  init = 1:0,
  cycles = 10
)

rsp <- define_resample(
  age_init ~ normal(60, 10),
  cost_init ~ normal(1000, 100),
  correlation = matrix(c(
    1, .4,
    .4, 1
  ), byrow = TRUE, ncol = 2)
)


# with run_model result
ndt1 <- run_probabilistic(res2, resample = rsp, N = 100)
