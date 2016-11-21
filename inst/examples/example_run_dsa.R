param <- define_parameters(
  p1 = .5,
  p2 = .2,
  r = .05
)
mod1 <- define_strategy(
  transition = define_transition(
    C, p1,
    p2, C
  ),
  define_state(
    cost = discount(543, r),
    ly = 1
  ),
  define_state(
    cost = discount(432, r),
    ly = .5
  )
)

mod2 <- define_strategy(
  transition = define_transition(
    C, p1,
    p2, C
  ),
  define_state(
    cost = 789,
    ly = 1
  ),
  define_state(
    cost = 456,
    ly = .8
  )
)

res2 <- run_model(
  mod1, mod2,
  parameters = param,
  init = c(100, 0),
  cycles = 10,
  cost = cost,
  effect = ly
)

ds <- define_dsa(
  p1, .1, .9,
  p2, .1, .3,
  r, .05, .1
)
print(ds)

x <- run_dsa(res2, ds)

plot(x, value = "cost")

# can be specified as a function of other parameters


ds2 <- define_dsa(
  p2, p1 - .1, p1 + .1
)

run_dsa(res2, ds2)
