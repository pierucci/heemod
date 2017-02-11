context("Running model")

test_that(
  "Strange inputs generate errors", {
    par1 <- define_parameters(
      a = .1,
      b = 1 / (markov_cycle + 1)
    )
    mat1 <- define_transition(
      state_names = c("X1", "X2"),
      1-a, a,
      1-b, b
    )
    mat2 <- define_transition(
      state_names = c("X1", "X3"),
      1-a, a,
      1-b, b
    )
    s1 <- define_state(
      x = 234,
      y = 123
    )
    s2 <- define_state(
      x = 987,
      y = 1726
    )
    
    mod1 <- define_strategy(
      transition = mat1,
      X1 = s1,
      X2 = s2
    )
    s3 <- define_state(
      x = 987,
      y = 876
    )
    s4 <- define_state(
      x = 456,
      y = 1029
    )
    s5 <- define_state(
      a = 456,
      b = 1029
    )
    mod2 <- define_strategy(
      transition = mat1,
      X1 = s3,
      X2 = s4
    )
    mod3 <- define_strategy(
      transition = mat2,
      X1 = s3,
      X3 = s4
    )
    mod4 <- define_strategy(
      transition = mat1,
      X1 = s5,
      X2 = s5
    )
    expect_error(
      run_model(
        mod1, mod2,
        parameters = par1,
        init = c(1, 2, 3)
      )
    )
    expect_error(
      run_model(
        mod1, mod2,
        parameters = par1,
        init = c(X3 = 1, X4 = 2)
      )
    )
    expect_error(
      run_model(
        mod1, mod2,
        parameters = par1,
        init = c(-1, 0)
      )
    )
    expect_error(
      run_model(
        mod1, mod2,
        parameters = par1,
        init = c(1, 0, 0)
      )
    )
    expect_error(
      run_model(
        mod1, mod2,
        parameters = par1,
        init = c(NA, 1)
      )
    )
    expect_error(
      run_model(
        mod1, mod2,
        parameters = par1,
        cycles = 0
      )
    )
    expect_error(
      run_model(
        mod1, mod2,
        parameters = par1,
        cycles = 11.5
      )
    )
    expect_error(
      run_model(
        mod1,
        parameters = par1, list()
      )
    )
    expect_error(
      run_model(
        mod1, mod2,
        parameters = par1,
        method = "zzz"
      )
    )
    expect_error(
      run_model(
        mod1, mod3, parameters = par1
      )
    )
    expect_error(
      run_model(
        mod1, mod4, parameters = par1
      )
    )
    expect_warning(
      run_model(
        mod1, mod2,
        parameters = par1
      )
    )
    expect_warning(
      run_model(
        mod1, mod2,
        parameters = par1,
        cost = x
      )
    )
  }
)

test_that(
  "run_model behaves as expected", {
    par1 <- define_parameters(
      a = .1,
      b = 1 / (markov_cycle + 1)
    )
    mat1 <- define_transition(
      state_names = c("X1", "X2"),
      1-a, a,
      1-b, b
    )
    s1 <- define_state(
      x = 234,
      y = 123
    )
    s2 <- define_state(
      x = 987,
      y = 1726
    )
    mod1 <- define_strategy(
      transition = mat1,
      X1 = s1,
      X2 = s2
    )
    s3 <- define_state(
      x = 987,
      y = 876
    )
    s4 <- define_state(
      x = 456,
      y = 1029
    )
    mod2 <- define_strategy(
      transition = mat1,
      X1 = s3,
      X2 = s4
    )
    
    expect_identical(
      run_model(mod1, mod2,
                 parameters = par1, init = c(1000L, 0L), cost = x, effect = y),
      run_model(mod1, mod2,
                 parameters = par1, cost = x, effect = y)
    )
    expect_identical(
      run_model(mod1, mod2,
                 parameters = par1, cost = x, effect = y),
      run_model(I = mod1, II = mod2,
                 parameters = par1, cost = x, effect = y)
    )
    expect_warning(
      run_model(I = mod1, mod2,
                 parameters = par1, cost = x, effect = y)
    )
    expect_warning(
      summary(run_model(mod1, mod2,
                         parameters = par1))
    )
    expect_equal(
      run_model(mod1, mod2,
                parameters = par1, cost = x, effect = y,
                method = "beginning")$run_model$.cost,
      c(309300, 933900)
    )
    
    s_mod <- summary(
      run_model(
        mod1, mod2,
        parameters = par1, cost = x, effect = y,
        method = "beginning"))
    expect_equal(
      s_mod$res_values$x, c(309300, 933900)
    )
    expect_equal(
      s_mod$res_comp$.cost, c(0, 624.6)
    )
    expect_equal(
      round(s_mod$res_comp$.icer, 3)[2], 1.027
    )
    expect_equal(
      s_mod$frontier, c("I", "II")
    )
    
    res_b <- run_model(mod1, mod2,
                        parameters = par1, cost = x, effect = y,
                        method = "beginning")
    res_e <- run_model(mod1, mod2,
                        parameters = par1, cost = x, effect = y,
                        method = "end")
    res_h <- suppressWarnings(run_model(mod1, mod2,
                        parameters = par1, cost = x, effect = y,
                        method = "half-cycle"))
    res_l <- run_model(mod1, mod2,
                        parameters = par1, cost = x, effect = y,
                        method = "life-table")
    
    plot(res_b, result = "counts")
    plot(res_b, result = "values")
    plot(res_b, result = "ce")
    
    expect_equal(
      round(summary(res_b)$res_comp$.icer[2], 3),
      1.027
    )
    expect_equal(
      round(summary(res_e)$res_comp$.icer[2], 3),
      1
    )
    expect_equal(
      round(summary(res_h)$res_comp$.icer[2], 3),
      1.017
    )
    expect_equal(
      round(summary(res_l)$res_comp$.icer[2], 3),
      1.012
    )
    
    expect_error(
      run_model(mod1, mod2,
                 parameters = par1, cost = x, effect = y,
                 method = "testtest")
    )
    rm <- run_model(mod1, mod2,
                     parameters = par1, cost = x, effect = y,
                     cycles = 5)
    expect_equivalent(
      round(get_counts(rm, 1)$count),
      c(950, 888, 879, 885, 890, 950,
        888, 879, 885, 890, 50, 112, 
        121, 115, 110, 50, 112, 121, 115, 110)
    )
    
    expect_equivalent(
      get_init(rm),
      c(1000, 0)
    )
    
    
    plot(rm, panels = "by_strategy")
    plot(rm, panels = "by_state")
  }
)

test_that("Discounting", {
  
  par1 <- define_parameters(
    a = .1,
    b = 1 / (markov_cycle + 1),
    c1 = 987,
    c2 = 876,
    c3 = 1726
  )
  mat1 <- define_transition(
    state_names = c("X1", "X2"),
    1-a, a,
    1-b, b
  )
  
  s1 <- define_state(
    x = 234,
    y = 123
  )
  s2 <- define_state(
    x = 987,
    y = 1726
  )
  mod1 <- define_strategy(
    transition = mat1,
    X1 = s1,
    X2 = s2
  )
  
  s3 <- define_state(
    x = discount(c1, .1),
    y = discount(c2, .05, TRUE)
  )
  s4 <- define_state(
    x = 456,
    y = 1029
  )
  mod2 <- define_strategy(
    transition = mat1,
    X1 = s3,
    X2 = s4
  )
  
  s5 <- define_state(
    x = discount(c1, 0),
    y = discount(c3, 0)
  )
  mod3 <- define_strategy(
    transition = mat1,
    X1 = s1,
    X2 = s5
  )
  
  s6 <- define_state(
    x = discount(c1, 1.5),
    y = discount(c3, .05, TRUE)
  )
  s4 <- define_state(
    x = 456,
    y = 1029
  )
  mod4 <- define_strategy(
    transition = mat1,
    X1 = s6,
    X2 = s4
  )
  res <- run_model(mod1, mod2, cycles = 10,
                    parameters = par1, cost = x, effect = y,
                    method = "beginning")
  expect_equal(
    round(summary(res)$res_comp$.icer[2], 3), 0.785
  )
  res1 <- run_model(mod1, mod2, cycles = 10,
                     parameters = par1, cost = x, effect = y,
                     method = "beginning")
  res2 <- run_model(mod3, mod2, cycles = 10,
                     parameters = par1, cost = x, effect = y,
                     method = "beginning")
  
  expect_equal(
    round(summary(res1)$res_comp$.icer[2], 3), 0.785
  )
  expect_equal(
    round(summary(res2)$res_comp$.icer[2], 3), 0.785
  )
  
  
  expect_error(
    run_model(mod1, mod4, cycles = 10,
               parameters = par1, cost = x, effect = y)
  )
})

test_that(
  "check_model_index works", {
    
    par1 <- define_parameters(
      a = .1,
      b = 1 / (markov_cycle + 1)
    )
    mat1 <- define_transition(
      state_names = c("X1", "X2"),
      1-a, a,
      1-b, b
    )
    s1 <- define_state(
      x = 234,
      y = 123
    )
    s2 <- define_state(
      x = 987,
      y = 1726
    )
    mod1 <- define_strategy(
      transition = mat1,
      X1 = s1,
      X2 = s2
    )
    s3 <- define_state(
      x = 987,
      y = 876
    )
    s4 <- define_state(
      x = 456,
      y = 1029
    )
    mod2 <- define_strategy(
      transition = mat1,
      X1 = s3,
      X2 = s4
    )
    
    res <- run_model(
      mod1, mod2,
      parameters = par1, init = c(1000L, 0L),
      cost = x, effect = y)
    
    expect_error(
      heemod:::check_strategy_index(res, 1:2)
    )
    expect_error(
      heemod:::check_strategy_index(res, as.factor("I"))
    )
    expect_error(
      heemod:::check_strategy_index(res, "a")
    )
  }
)
