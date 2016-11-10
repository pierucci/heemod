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
    expect_output(
      str(run_model(mod1, mod2,
                     parameters = par1, cost = x, effect = y,
                     method = "beginning")),
      '2 obs. of  5 variables:
 $ x           : num  309300 933900
 $ y           : num  283300 891300
 $ .model_names: chr  "I" "II"
 $ .cost       : num  309300 933900
 $ .effect     : num  283300 891300',
      fixed = TRUE
    )
    s_mod <- summary(
      run_model(
        mod1, mod2,
        parameters = par1, cost = x, effect = y,
        method = "beginning"))
    expect_length(
      s_mod, 6
    )
    expect_identical(
      dim(s_mod$res), c(2L, 2L)
    )
    expect_identical(
      round(s_mod$res$x), c(309300, 933900)
    )
    expect_identical(
      round(s_mod$res$y), c(283300, 891300)
    )
    
    res_b <- run_model(mod1, mod2,
                        parameters = par1, cost = x, effect = y,
                        method = "beginning")
    res_e <- run_model(mod1, mod2,
                        parameters = par1, cost = x, effect = y,
                        method = "end")
    res_h <- run_model(mod1, mod2,
                        parameters = par1, cost = x, effect = y,
                        method = "half-cycle")
    res_l <- run_model(mod1, mod2,
                        parameters = par1, cost = x, effect = y,
                        method = "life-table")
    
    plot(res_b, type = "counts")
    plot(res_b, type = "values", value = c("x", "y"))
    plot(res_b, type = "ce")
    
    expect_output(
      print(res_b),
      "1.027303"
    )
    expect_output(
      print(res_e),
      "753"
    )
    expect_output(
      print(res_h),
      "1.016861"
    )
    expect_output(
      print(res_l),
      "1.012197"
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
      round(unlist(get_counts(rm, 1))),
      c(950, 888, 879, 885, 890, 50, 112, 121, 115, 110)
    )
    expect_identical(
      get_counts(rm, 1),
      get_counts(rm, "I")
    )
    expect_equivalent(
      get_init(rm),
      c(1000, 0)
    )
    
    expect_error(plot(rm, include_states = "C"))
    
    plot(rm, panels = "by_model")
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
  expect_output(
    print(res),
    "3292.352     4193.422 0.7851231"
  )
  res1 <- run_model(mod1, mod2, cycles = 10,
                     parameters = par1, cost = x, effect = y,
                     method = "beginning")
  res2 <- run_model(mod3, mod2, cycles = 10,
                     parameters = par1, cost = x, effect = y,
                     method = "beginning")
  expect_output(
    print(res1),
    "I  3144649 2942952
II 6437001 7136374"
  )
  expect_output(
    print(res2),
    "I  3144649 2942952
II 6437001 7136374"
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
      heemod:::check_model_index(res, 1:2)
    )
    expect_error(
      heemod:::check_model_index(res, as.factor("I"))
    )
    expect_error(
      heemod:::check_model_index(res, "a")
    )
  }
)
