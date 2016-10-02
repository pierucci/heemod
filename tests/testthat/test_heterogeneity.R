context("Heterogeneity & Demographic")

test_that(
  "Demographic analysis", {
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
      effect = ly,
      method = "beginning"
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
    
    x <- update(res, newdata = new_tab2)
    plot(x, type = "counts", model = 1)
    expect_message(update(res, newdata = new_tab))
    
    expect_output(
      print(x),
      "       Cost   Effect      ICER
I -28996.37 2.403762 -12062.91",
      fixed = TRUE
    )
  })


test_that(
  "Heterogeneity analysis", {
    mod1 <-
      define_model(
        transition_matrix = define_matrix(
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
      effect = ly,
      method = "beginning"
    )
    
    # generating table with new parameter sets
    new_tab <- data.frame(
      age_init = 40:80
    )
    
    # with run_model result
    ndt <- update(res, newdata = new_tab)
    
    plot(ndt, model = 1, type = "icer")
    plot(ndt, model = 1, type = "cost")
    plot(ndt, model = 1, type = "effect")
    
    expect_error(
      plot(ndt, model = "II")
    )
    expect_error(
      update(mod1, newdata = new_tab)
    )
    
    expect_output(
      print(ndt),
      'An analysis re-run on 41 parameter sets.',
      fixed= TRUE
    )
    expect_output(
      str(summary(ndt)),
      '10 obs. of  8 variables:
 $ Model  : Factor w/ 2 levels "II","I": 1 1 1 1 1 2 2 2 2 2
 $ Value  : Factor w/ 5 levels "Cost","Effect",..: 1 2 3 4 5 1 2 3 4 5
 $ Min.   : num  24044 4 NA NA NA ...
 $ 1st Qu.: num  29343.96 4.39 NA NA NA ...
 $ Median : num  34643.94 4.78 NA NA NA ...
 $ Mean   : num  34643.94 4.78 NA NA NA ...
 $ 3rd Qu.: num  39943.92 5.17 NA NA NA ...
 $ Max.   : num  45243.91 5.56 NA NA NA ...',
      fixed= TRUE
    )
  }
)
