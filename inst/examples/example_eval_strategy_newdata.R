par1 <- define_parameters(
  a = 1,
  b = 1 / (markov_cycle + a)
)

mat1 <- define_transition(
  1-b, b,
  0, 1
)
mod1 <- define_strategy(
  transition = mat1,
  define_state(var = a),
  define_state(var = a * markov_cycle)
)

res1 <- run_model(
  mod1,
  parameters = par1,
  cycles = 5,
  init = 1:0,
  method = "end"
)

new_tab <- data.frame(
  a = 1:10
)

heemod:::eval_strategy_newdata(
  res1,
  newdata = new_tab
)
