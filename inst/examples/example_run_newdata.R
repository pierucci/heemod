mod1 <-
  define_model(
    parameters = define_parameters(
      age_init = 60,
      age = age_init + markov_cycle
    ),
    transition_matrix = define_matrix(
      .5, .5,
      .1, .9
    ),
    define_state(
      cost = 543 + age * 5
    ),
    define_state(
      cost = 432 + age
    )
    
  )

mod2 <-
  define_model(
    parameters = define_parameters(
      age_init = 60,
      age = age_init + markov_cycle
    ),
    transition_matrix = define_matrix(
      .5, .5,
      .1, .9
    ),
    define_state(
      cost = 789 * age / 10
    ),
    define_state(
      cost = 456 * age / 10
    )
    
  )

res2 <- run_model(
  mod1, mod2,
  init = 1:0,
  cycles = 10
)
# generating table with new parameter sets
new_tab <- data.frame(
  age_init = 40:80
)

# with run_model result
ndt1 <- run_newdata(res2, newdata = new_tab)
