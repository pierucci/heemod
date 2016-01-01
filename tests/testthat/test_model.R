context("Test model")

test_that(
  "Model definition", {
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
    expect_output(
      print(mod1),
      "An unevaluated Markov model:

    2 parameters,
    2 states,
    2 state values,
    No starting values defined.",
      fixed = TRUE
    )
    expect_output(
      str(mod1),
      "List of 4
 $ parameters       :List of 2
  ..$ a:List of 2
  .. ..$ expr: num 0.1",
      fixed = TRUE
    )
    expect_error(
      define_model(
        parameters = par1,
        transition_matrix = mat1,
        X1 = s1,
        X3 = s2
      )
    )
  }
)

test_that(
  "Model evaluation, 1 model", {
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
    e_mod <- run_model(
      mod1,
      init = c(1, 0),
      cycles = 5
    )
    expect_output(
      str(e_mod),
      'List of 1
 $ A:List of 5
  ..$ parameters',
      fixed = TRUE
    )
    expect_output(
      print(e_mod),
      "1 Markov model, run for 5 cycles.

Model names:

A",
      fixed = TRUE
    )
    expect_output(
      str(summary(e_mod)),
      'List of 4
 $ res       : num [1, 1:2] 1593 1515',
      fixed = TRUE
    )
    expect_output(
      print(summary(e_mod)),
      '1 Markov model run for 5 cycles.

Initial states:

   N
X1 1
X2 0
         x        y
A 1592.538 1514.507',
      fixed = TRUE
    )
    expect_error(
      run_model(
        mod1,
        init = c(1, 0, 0),
        cycles = 5
      )
    )
    expect_error(
      run_model(
        mod1,
        init = c(-1, 0),
        cycles = 5
      )
    )
    expect_error(
      run_model(
        mod1,
        init = c(-1, 0),
        cycles = -5
      )
    )
  }
)


test_that(
  "Model evaluation, 2 models", {
    
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
    mod2 <- define_model(
      parameters = par1,
      transition_matrix = mat1,
      X1 = s1,
      X2 = s1
    )
    
    e_mod2 <- run_models(
      mod1, mod2,
      init = c(1, 0),
      cycles = 5
    )
    expect_output(
      str(e_mod2),
      'List of 2
 $ A:List of 5
  ..$ parameters       :',
      fixed = TRUE
    )
    expect_output(
      print(e_mod2),
      "2 Markov models, run for 5 cycles.

Model names:

A
B",
      fixed = TRUE
    )
    expect_output(
      str(summary(e_mod2)),
      'List of 4
 $ res       : num [1:2, 1:2] 1593 1170 1515 615',
      fixed = TRUE
    )
    expect_output(
      print(summary(e_mod2)),
      '2 Markov models run for 5 cycles.

Initial states:

   N
X1 1
X2 0
         x        y
A 1592.538 1514.507
B 1170.000  615.000',
      fixed = TRUE
    )
    expect_output(
      print(
        run_models(
          mod1 = mod1, mod2 = mod2,
          init = c(1, 0),
          cycles = 5
        )
      ),
      "2 Markov models, run for 5 cycles.

Model names:

mod1
mod2",
      fixed = TRUE
    )
  }
)
