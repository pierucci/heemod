context("Running model")

test_that(
  "Strange inputs generate errors", {
    par1 <- define_parameters(
      a = .1,
      b = 1 / (markov_cycle + 1)
    )
    mat1 <- define_matrix(
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
    
    mod1 <- define_model(
      transition_matrix = mat1,
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
    mod2 <- define_model(
      transition_matrix = mat1,
      X1 = s3,
      X2 = s4
    )
    expect_error(
      run_models(
        mod1, mod2,
        parameters = par1,
        init = c(1, 2, 3)
      )
    )
    expect_error(
      run_models(
        mod1, mod2,
        parameters = par1,
        init = c(X3 = 1, X4 = 2)
      )
    )
    expect_error(
      run_models(
        mod1, mod2,
        parameters = par1,
        init = c(-1, 0)
      )
    )
    expect_error(
      run_models(
        mod1, mod2,
        parameters = par1,
        init = c(NA, 1)
      )
    )
    expect_error(
      run_models(
        mod1, mod2,
        parameters = par1,
        cycles = 0
      )
    )
    expect_error(
      run_models(
        mod1,
        parameters = par1, list()
      )
    )
    expect_error(
      run_models(
        mod1, mod2,
        parameters = par1,
        method = "zzz"
      )
    )
    expect_warning(
      run_models(
        mod1, mod2,
        parameters = par1
      )
    )
    expect_warning(
      run_models(
        mod1, mod2,
        parameters = par1,
        cost = x
      )
    )
  }
)

test_that(
  "run_models behaves as expected", {
    par1 <- define_parameters(
      a = .1,
      b = 1 / (markov_cycle + 1)
    )
    mat1 <- define_matrix(
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
    mod1 <- define_model(
      transition_matrix = mat1,
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
    mod2 <- define_model(
      transition_matrix = mat1,
      X1 = s3,
      X2 = s4
    )
    
    expect_identical(
      run_models(mod1, mod2,
                 parameters = par1, init = c(1000L, 0L), cost = x, effect = y),
      run_models(mod1, mod2,
                 parameters = par1, cost = x, effect = y)
    )
    expect_identical(
      run_models(mod1, mod2,
                 parameters = par1, cost = x, effect = y),
      run_models(I = mod1, II = mod2,
                 parameters = par1, cost = x, effect = y)
    )
    expect_warning(
      run_models(I = mod1, mod2,
                 parameters = par1, cost = x, effect = y)
    )
    expect_output(
      str(run_models(mod1, mod2,
                     parameters = par1, cost = x, effect = y)),
      '2 obs. of  5 variables:
 $ x           : num  309300 933900
 $ y           : num  283300 891300
 $ .model_names: chr  "I" "II"
 $ .cost       : num  309300 933900
 $ .effect     : num  283300 891300',
      fixed = TRUE
    )
    expect_output(
      str(summary(run_models(mod1, mod2,
                             parameters = par1, cost = x, effect = y))),
      "List of 6
 $ res       :'data.frame':	2 obs. of  2 variables:
  ..$ x: num [1:2] 309300 933900
  ..$ y: num [1:2] 283300 891300",
      fixed = TRUE
    )
    
    res_b <- run_models(mod1, mod2,
                        parameters = par1, cost = x, effect = y)
    res_e <- run_models(mod1, mod2,
                        parameters = par1, cost = x, effect = y,
                        method = "end")
    res_h <- run_models(mod1, mod2,
                        parameters = par1, cost = x, effect = y,
                        method = "half-cycle")
    res_l <- run_models(mod1, mod2,
                        parameters = par1, cost = x, effect = y,
                        method = "life-table")
    
    plot(res_b, type = "counts")
    plot(res_b, type = "ce")
    
    expect_output(
      print(res_b),
      "II 624.6    608 1.027303"
    )
    expect_output(
      print(res_e),
      "II  753    753    1"
    )
    expect_output(
      print(res_h),
      "II 1501.65 1476.75 1.016861"
    )
    expect_output(
      print(res_l),
      "II 688.8  680.5 1.012197"
    )
    expect_error(
      run_models(mod1, mod2,
                 parameters = par1, cost = x, effect = y,
                 method = "testtest")
    )
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
  mat1 <- define_matrix(
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
  mod1 <- define_model(
    transition_matrix = mat1,
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
  mod2 <- define_model(
    transition_matrix = mat1,
    X1 = s3,
    X2 = s4
  )
  
  s5 <- define_state(
    x = discount(c1, 0),
    y = discount(c3, 0)
  )
  mod3 <- define_model(
    transition_matrix = mat1,
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
  mod4 <- define_model(
    transition_matrix = mat1,
    X1 = s6,
    X2 = s4
  )
  res <- run_models(mod1, mod2, cycles = 10,
                    parameters = par1, cost = x, effect = y)
  expect_output(
    print(res),
    "II 3292.352 4193.422 0.7851231"
  )
  res1 <- run_models(mod1, mod2, cycles = 10,
                     parameters = par1, cost = x, effect = y)
  res2 <- run_models(mod3, mod2, cycles = 10,
                     parameters = par1, cost = x, effect = y)
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
    run_models(mod1, mod4, cycles = 10,
               parameters = par1, cost = x, effect = y)
  )
})
