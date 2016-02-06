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
      parameters = par1,
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
      parameters = par1,
      transition_matrix = mat1,
      X1 = s3,
      X2 = s4
    )
    expect_error(
      run_models(
        mod1, mod2,
        init = c(1, 2, 3)
      )
    )
    expect_error(
      run_models(
        mod1, mod2,
        init = c(X3 = 1, X4 = 2)
      )
    )
    expect_error(
      run_models(
        mod1, mod2,
        init = c(-1, 0)
      )
    )
    expect_error(
      run_models(
        mod1, mod2,
        init = c(NA, 1)
      )
    )
    expect_error(
      run_models(
        mod1, mod2,
        cycles = 0
      )
    )
    expect_error(
      run_models(
        mod1, list()
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
      parameters = par1,
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
      parameters = par1,
      transition_matrix = mat1,
      X1 = s3,
      X2 = s4
    )
    expect_identical(
      run_model(mod1, mod2, cost = x, effect = y),
      run_models(mod1, mod2, cost = x, effect = y)
    )
    expect_identical(
      run_models(mod1, mod2, init = 1:0, cost = x, effect = y),
      run_models(mod1, mod2, cost = x, effect = y)
    )
    expect_identical(
      run_models(mod1, mod2, cost = x, effect = y),
      run_models(A = mod1, B = mod2, cost = x, effect = y)
    )
    expect_output(
      str(run_models(mod1, mod2, cost = x, effect = y)),
      '2 obs. of  5 variables:
 $ x           : num  309 934
 $ y           : num  283 891
 $ .model_names: chr  "A" "B"
 $ .cost       : num  309 934
 $ .effect     : num  283 891',
      fixed = TRUE
    )
    expect_output(
      str(summary(run_models(mod1, mod2, cost = x, effect = y))),
      "2 obs. of  5 variables:
  ..$ x      : num [1:2] 309 934
  ..$ y      : num [1:2] 283 891
  ..$ .cost  : num [1:2] 0 625
  ..$ .effect: num [1:2] 0 608
  ..$ .icer  : num [1:2] -Inf 1.03",
      fixed = TRUE
    )
  }
)
