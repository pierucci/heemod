
param <- define_parameters(p = 0.8)

mat <- define_transition(
  p, C,
  0, 1
)
mod <- define_strategy(
  transition = mat,
  A = define_state(cost=10, effect = 0.5), 
  B = define_state(cost = 5, effect = 0.8)
)

res_mod <- run_model(
  mod = mod,
  parameters = param,
  init = c(1000L, 0L),
  cycles = 10,
  cost = cost,
  effect = effect,
  method = "end"
)

f <- function(x) {
  dplyr::filter(
    get_counts(x),
    state_names == "A" & markov_cycle == 10
  )$count
}
f(res_mod)

calibrate_model(
  res_mod,
  parameter_names = "p",
  fn_values = f,
  target_values = 130,
  initial_values = data.frame(p = c(0.5, 0.9)),
  lower = 0, upper = 1
)
