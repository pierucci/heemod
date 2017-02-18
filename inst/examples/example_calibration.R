
param <- define_parameters(p = 0.8)

mat <- define_transition(
  p, C,
  0, 1
)
mod <- define_strategy(
  transition = mat,
  A = define_state(cost=10, effect = 0.5), 
  B= define_state(cost = 5, effect = 0.8)
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

target.final.count <- get_counts(res_mod, "A")$count[10]   #134.2177

simple_matching_df <- 
  data.frame(group = 1, strategy_name = "mod", cycles_to_sum = 10, states_to_sum = "A")
calibrate_model(res_mod,
                param_names = c("p"),
                matching_df = simple_matching_df,
                target_values = c(134.2177),
                method = c("L-BFGS-B"),
                initial_values = matrix(c(0.1, 0.5, 0.9), 
                                        dimnames = list(NULL, "p")), 
                lower = 0, upper = 1
)

