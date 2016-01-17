# running a single model

param <- define_parameters(
  p1 = .5,
  p2 = .2
)
mod1 <-
  define_model(
    parameters = param,
    transition_matrix = define_matrix(
      C, p1,
      p2, C
    ),
    define_state(
      cost = 543
    ),
    define_state(
      cost = 432
    )
  )

# running several models
mod2 <-
  define_model(
    parameters = param,
    transition_matrix = define_matrix(
      C, p1,
      p2, C
    ),
    define_state(
      cost = 789
    ),
    define_state(
      cost = 456
    )
    
  )


res2 <- run_model(
  mod1, mod2,
  init = c(100, 0),
  cycles = 10
)

ds <- define_sensitivity(
  p1 = c(.1, .9),
  p2 = c(.1, .3)
)
print(ds)

x <- run_sensitivity(res2, ds)

plot(x, value = "cost")
