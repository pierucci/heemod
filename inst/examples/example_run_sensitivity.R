# running a single model

param <- define_parameters(
  p1 = .5,
  p2 = .2
)
mod1 <-
  define_model(
    transition_matrix = define_matrix(
      C, p1,
      p2, C
    ),
    define_state(
      cost = 543,
      ly = 1
    ),
    define_state(
      cost = 432,
      ly = .5
    )
  )

mod2 <-
  define_model(
    transition_matrix = define_matrix(
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

res2 <- run_models(
  mod1, mod2,
  parameters = param,
  init = c(100, 0),
  cycles = 10,
  cost = cost,
  effect = ly
)

ds <- define_sensitivity(
  p1 = c(.1, .9),
  p2 = c(.1, .3)
)
print(ds)

x <- run_sensitivity(res2, ds)

plot(x, value = "cost")
