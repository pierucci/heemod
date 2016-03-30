par1 <- define_parameters(
  a = 1,
  b = 1 / (markov_cycle + a)
)

mat1 <- define_matrix(
  1-b, b,
  0, 1
)
mod1 <- define_model(
  transition_matrix = mat1,
  define_state(var = a),
  define_state(var = a * markov_cycle)
)

new_tab <- data.frame(
  a = 1:10
)

heemod:::eval_model_newdata(
  mod1,
  old_parameters = par1,
  cycles = 5,
  init = 1:0,
  newdata = new_tab,
  method = "end"
)
