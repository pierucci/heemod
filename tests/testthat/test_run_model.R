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
      run_model(mod1, mod2),
      run_models(mod1, mod2)
    )
    expect_identical(
      run_models(mod1, mod2, init = 1:0),
      run_models(mod1, mod2)
    )
    expect_identical(
      run_models(mod1, mod2),
      run_models(A = mod1, B = mod2)
    )
    expect_output(
      str(run_models(mod1, mod2)),
      "List of 2
 $ A:List of 5",
      fixed = TRUE
    )
    expect_output(
      str(summary(run_models(mod1, mod2))),
      'List of 4
 $ res       : num [1:2, 1:2] 309 934 283 891
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:2] "A" "B"
  .. ..$ : chr [1:2] "x" "y"',
      fixed = TRUE
    )
  }
)
