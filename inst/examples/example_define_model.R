
param <- define_parameters(
  a = 1,
  b = 4 * markov_cycle,
  c = a + b
)

mat <- define_matrix(
  state_names = c("s1", "s2"),
  1 / c, 1 - 1/ c,
  0, 1
)

s1 <- define_state(
  cost = 234,
  utility = 1
  )
s2 <- define_state(
  cost = 421,
  utility = .5
  )

define_model(
  parameters = param,
  transition_matrix = mat,
  s1 = s1,
  s2 = s2
)

# with starting values

define_model(
  parameters = param,
  transition_matrix = mat,
  s1 = s1,
  s2 = s2,
  starting_values = c(cost = 500)
)
