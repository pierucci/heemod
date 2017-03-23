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
  strategy = mod,
  parameters = param,
  init = define_init(A = 10, B = 5),
  cycles = 5,
  method = "end",
  inflow = define_inflow(A = 0, B = 0),
  strategy_name = "A",
  expand_limit = c(A = 5, B = 5)
)
