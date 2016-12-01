mod1 <-
  define_strategy(
    transition = define_transition(
      .5, .5,
      .1, .9
    ),
    define_state(
      cost = 543 + age * 5,
      ly = 1
    ),
    define_state(
      cost = 432 + age,
      ly = 1 * age / 100
    )
  )

mod2 <-
  define_strategy(
    transition = define_transition(
      .5, .5,
      .1, .9
    ),
    define_state(
      cost = 789 * age / 10,
      ly = 1
    ),
    define_state(
      cost = 456 * age / 10,
      ly = 1 * age / 200
    )
  )

res <- run_model(
  mod1, mod2,
  parameters = define_parameters(
    age_init = 60,
    age = age_init + markov_cycle
  ),
  init = 1:0,
  cycles = 10,
  cost = cost,
  effect = ly
)

# generating table with new parameter sets
new_tab <- data.frame(
  age_init = 40:45
)

# with run_model result
ndt <- update(res, newdata = new_tab)

summary(ndt)

# using weights

new_tab2 <- data.frame(
  age_init = 40:45,
  .weights = runif(6)
)
ndt2 <- update(res, newdata = new_tab2)

summary(ndt2)
