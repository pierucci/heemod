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
    mat2 <- define_matrix(
      1-a, a,
      1-b, b
    )
    mod2 <- define_model(
      parameters = par1,
      transition_matrix = mat2,
      s1,
      s2
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
    expect_output(
      print(names(mod2$states)),
      '"A" "B"',
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
    e_mod <- run_models(
      mod1,
      init = c(1, 0),
      cycles = 5,
      cost = x,
      effect = y
    )
    expect_output(
      str(e_mod),
      '1 obs. of  5 variables:
 $ x           : num 1593
 $ y           : num 1515
 $ .model_names: chr "A"
 $ .cost       : num 1593
 $ .effect     : num 1515',
      fixed = TRUE
    )
    expect_output(
      print(e_mod),
      "1 Markov model run for 5 cycles.

Initial states:

   N
X1 1
X2 0
         x        y
A 1592.538 1514.507",
      fixed = TRUE
    )
    expect_output(
      str(summary(e_mod)),
      "List of 5
 $ res       :'data.frame':	1 obs. of  5 variables:
  ..$ x      : num 1593
  ..$ y      : num 1515
  ..$ .cost  : num 0
  ..$ .effect: num 0
  ..$ .icer  : num -Inf",
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
      run_models(
        mod1,
        init = c(1, 0, 0),
        cycles = 5
      )
    )
    expect_error(
      run_models(
        mod1,
        init = c(-1, 0),
        cycles = 5
      )
    )
    expect_error(
      run_models(
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
      cycles = 5,
      cost = x,
      effect = y
    )
    expect_output(
      str(e_mod2),
      '2 obs. of  5 variables:
 $ x           : num  1593 1170
 $ y           : num  1515 615
 $ .model_names: chr  "A" "B"
 $ .cost       : num  1593 1170
 $ .effect     : num  1515 615',
      fixed = TRUE
    )
    expect_output(
      print(e_mod2),
      "2 Markov models run for 5 cycles.

Initial states:

   N
X1 1
X2 0
         x        y
A 1170.000  615.000
B 1592.538 1514.507

Efficiency frontier:

B A

Model difference:

      Cost   Effect      ICER
B 422.5384 899.5074 0.4697442",
      fixed = TRUE
    )
    expect_output(
      str(summary(e_mod2)),
      "List of 5
 $ res       :'data.frame':	2 obs. of  5 variables:
  ..$ x      : num [1:2] 1170 1593
  ..$ y      : num [1:2] 615 1515
  ..$ .cost  : num [1:2] 0 423
  ..$ .effect: num [1:2] 0 900
  ..$ .icer  : num [1:2] -Inf 0.47",
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
A 1170.000  615.000
B 1592.538 1514.507

Efficiency frontier:

B A

Model difference:

      Cost   Effect      ICER
B 422.5384 899.5074 0.4697442',
      fixed = TRUE
    )
    expect_output(
      print(
        run_models(
          mod1 = mod1, mod2 = mod2,
          init = c(1, 0),
          cycles = 5,
          cost = x,
          effect = y
        )
      ),
      "2 Markov models run for 5 cycles.

Initial states:

   N
X1 1
X2 0
            x        y
mod1 1170.000  615.000
mod2 1592.538 1514.507

Efficiency frontier:

mod2 mod1

Model difference:

         Cost   Effect      ICER
mod2 422.5384 899.5074 0.4697442",
      fixed = TRUE
    )
  }
)
