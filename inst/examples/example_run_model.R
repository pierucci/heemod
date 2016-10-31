# running a single model

mod1 <-
  define_strategy(
    transition = define_transition(
      .5, .5,
      .1, .9
    ),
    define_state(
      cost = 543,
      ly = 1
    ),
    define_state(
      cost = 432,
      ly = 1
    )
  )


res <- run_model(
  mod1,
  init = c(100, 0),
  cycles = 2,
  cost = cost,
  effect = ly
)

# running several models
mod2 <-
  define_strategy(
    transition = define_transition(
      .5, .5,
      .1, .9
    ),
    define_state(
      cost = 789,
      ly = 1
    ),
    define_state(
      cost = 456,
      ly = 1
    )
    
  )


res2 <- run_model(
  mod1, mod2,
  init = c(100, 0),
  cycles = 10,
  cost = cost,
  effect = ly
)
