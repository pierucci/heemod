context("Demographic")

test_that(
  "Basic inputs", {
    mod1 <-
      define_model(
        transition_matrix = define_matrix(
          .4, .6,
          .1, .9
        ),
        define_state(
          cost = 543 + age * 5,
          ly = 1
        ),
        define_state(
          cost = 432 + age,
          ly = 1 * age / 100
        )
      )
    
    mod2 <-
      define_model(
        transition_matrix = define_matrix(
          .5, .5,
          .1, .9
        ),
        define_state(
          cost = 789 * age / 10,
          ly = 1
        ),
        define_state(
          cost = 456 * age / 10,
          ly = 1 * age / 200
        )
      )
    
    res <- run_models(
      mod1, mod2,
      parameters = define_parameters(
        age_init = 60,
        age = age_init + markov_cycle
      ),
      init = 1:0,
      cycles = 10,
      cost = cost,
      effect = ly
    )
    
    # generating table with demographic data
    new_tab <- data.frame(
      age_init = 40:80
    )
    set.seed(1)
    new_tab2 <- data.frame(
      age_init = 40:80,
      .weights = runif(41)
    )
    
    x <- run_demographics(res, demographics = new_tab2)
    plot(x)
    expect_message(run_demographics(res, demographics = new_tab))
    
    expect_output(
      print(x),
      "       Cost   Effect      ICER
I -28996.37 2.403762 -12062.91",
      fixed = TRUE
    )
  })