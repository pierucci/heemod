param <- define_parameters(
  a = markov_cycle + 1 * 2
)

mat <- define_transition(
  1-1/a, 1/a,
  .1,    .9
)

mod <- define_strategy(
  transition = mat,
  A = define_state(cost = 10),
  B = define_state(cost = 2)
)

heemod:::eval_strategy(
  mod,
  param,
  init = c(10, 5),
  cycles = 5,
  method = "end",
  inflow = c(0, 0)
)
