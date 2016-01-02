# running a single model

mod1 <-
  define_model(
    transition_matrix = define_matrix(
      .5, .5,
      .1, .9
    ),
    define_state(
      cost = 543
    ),
    define_state(
      cost = 432
    )
  )


res <- run_model(
  mod1,
  init = c(100, 0),
  cycles = 2
)

# running several models
mod2 <-
  define_model(
    transition_matrix = define_matrix(
      .5, .5,
      .1, .9
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
