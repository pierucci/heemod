context("Heterogeneity & Demographic")

test_that(
  "Demographic analysis", {
    mod1 <-
      define_strategy(
        transition = define_transition(
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
      define_strategy(
        transition = define_transition(
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
    
    res <- run_model(
      mod1, mod2,
      parameters = define_parameters(
        age_init = 60,
        age = age_init + markov_cycle
      ),
      init = 1:0,
      cycles = 10,
      cost = cost,
      effect = ly,
      method = "beginning"
    )
    
    # generating table with demographic data
    new_tab <- data.frame(
      age_init = 40:45
    )
    set.seed(1)
    new_tab2 <- data.frame(
      age_init = 40:45,
      .weights = runif(6)
    )
    
    x <- update(res, newdata = new_tab2)
    
    summary_update <- summary(x)
    summary_combine <- summary(x$model)
    
    expect_message(update(res, newdata = new_tab))
    
    expect_equal(
      round(summary_update$summary_results$Min.[1]), 25104
    )
    
    expect_equal(
      round(summary_combine$res_comp$.icer[2]), -12063
    )
    
    plot(x, type = "counts")
  })


test_that(
  "Heterogeneity analysis", {
    mod1 <-
      define_strategy(
        transition = define_transition(
          .5, .5,
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
      define_strategy(
        transition = define_transition(
          .5, .5,
          .1, .9
        ),
        define_state(
          cost = 789 * age / 100,
          ly = 1
        ),
        define_state(
          cost = 456 * age / 100,
          ly = 1 * age / 200
        )
        
      )
    
    res <- run_model(
      mod1, mod2,
      parameters = define_parameters(
        age_init = 60,
        age = age_init + markov_cycle
      ),
      init = 1:0,
      cycles = 10,
      cost = cost,
      effect = ly,
      method = "beginning"
    )
    
    # generating table with new parameter sets
    new_tab <- data.frame(
      age_init = 40:45
    )
    
    # with run_model result
    ndt <- update(res, newdata = new_tab)
    
    plot(ndt, result = "icer", type = "difference")
    plot(ndt, result = "cost")
    plot(ndt, result = "effect")
    
    expect_error(
      update(mod1, newdata = new_tab)
    )
    
    
    summary_update <- summary(ndt)
    summary_combine <- summary(ndt$model)
    
    expect_output(
      print(ndt),
      'An analysis re-run on 6 parameter sets.',
      fixed= TRUE
    )
    expect_equal(
      round(summary_update$summary_results$Min.)[1], 2404
    )
  }
)
