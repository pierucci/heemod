context("Test model")

test_that(
  "Model definition", {
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
    mat2 <- define_matrix(
      1-a, a,
      1-b, b
    )
    mod2 <- define_model(
      transition_matrix = mat2,
      s1,
      s2
    )
    expect_output(
      print(mod1),
      "An unevaluated Markov model:

    2 states,
    2 state values",
      fixed = TRUE
    )
    expect_output(
      str(mod1),
      "List of 2
 $ transition_matrix:List of 4",
      fixed = TRUE
    )
    expect_output(
      print(names(mod2$states)),
      '"A" "B"',
      fixed = TRUE
    )
    expect_error(
      define_model(
        transition_matrix = mat2,
        s1,
        s2,
        s2
      )
    )
    expect_error(
      define_model(
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
      transition_matrix = mat1,
      X1 = s1,
      X2 = s2
    )
    e_mod <- run_models(
      mod1,
      parameters = par1,
      init = c(1, 0),
      cycles = 5,
      cost = x,
      effect = y,
      method = "beginning"
    )
    expect_output(
      str(e_mod),
      '1 obs. of  5 variables:
 $ x           : num 1593
 $ y           : num 1515
 $ .model_names: chr "I"
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

Counting method: 'beginning'.

         x        y
I 1592.538 1514.507",
      fixed = TRUE
    )
    
    s_mod <- summary(e_mod)
    expect_length(
      s_mod, 6
    )
    expect_identical(
      dim(s_mod$res), c(1L, 2L)
    )
    expect_identical(
      round(s_mod$res$x),  1593
    )
    expect_identical(
      round(s_mod$res$y), 1515
    )
    expect_output(
      print(summary(e_mod)),
      "1 Markov model run for 5 cycles.

Initial states:

   N
X1 1
X2 0

Counting method: 'beginning'.

         x        y
I 1592.538 1514.507",
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
      transition_matrix = mat1,
      X1 = s1,
      X2 = s2
    )
    mod2 <- define_model(
      transition_matrix = mat1,
      X1 = s1,
      X2 = s1
    )
    
    e_mod2 <- run_models(
      mod1, mod2,
      parameters = par1,
      init = c(1, 0),
      cycles = 5,
      cost = x,
      effect = y,
      method = "beginning"
    )
    expect_output(
      str(e_mod2),
      '2 obs. of  5 variables:
 $ x           : num  1593 1170
 $ y           : num  1515 615
 $ .model_names: chr  "I" "II"
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

Counting method: 'beginning'.

          x        y
II 1170.000  615.000
I  1592.538 1514.507

Efficiency frontier:

II I

Model difference:

      Cost   Effect      ICER
I 422.5384 899.5074 0.4697442",
      fixed = TRUE
    )
    s_mod2 <- summary(e_mod2)
    expect_length(
      s_mod2, 6
    )
    expect_identical(
      dim(s_mod2$res), c(2L, 2L)
    )
    expect_identical(
      round(s_mod2$res$x), c(1170, 1593)
    )
    expect_identical(
      round(s_mod2$res$y), c(615, 1515)
    )
    
    expect_output(
      print(summary(e_mod2)),
      "2 Markov models run for 5 cycles.

Initial states:

   N
X1 1
X2 0

Counting method: 'beginning'.

          x        y
II 1170.000  615.000
I  1592.538 1514.507

Efficiency frontier:

II I

Model difference:

      Cost   Effect      ICER
I 422.5384 899.5074 0.4697442",
      fixed = TRUE
    )
    expect_output(
      print(
        run_models(
          mod1 = mod1, mod2 = mod2,
          parameters = par1,
          init = c(1, 0),
          cycles = 5,
          cost = x,
          effect = y,
          method = "beginning"
        )
      ),
      "2 Markov models run for 5 cycles.

Initial states:

   N
X1 1
X2 0

Counting method: 'beginning'.

            x        y
mod2 1170.000  615.000
mod1 1592.538 1514.507

Efficiency frontier:

mod2 mod1

Model difference:

         Cost   Effect      ICER
mod1 422.5384 899.5074 0.4697442",
      fixed = TRUE
    )
  }
)

test_that(
  "eval_matrix works", {
    par <- tibble::tibble(
      markov_cycle = 2:3,
      a = c(.1, .2)
    )
    mat <- define_matrix(
      C, 1/markov_cycle,
      a, 1-a
    )
    
    res <- heemod:::eval_matrix(mat, par)
    
    expect_identical(
      round(res[[1]], 2),
      structure(c(0.5, 0.1, 0.5, 0.9), .Dim = c(2L, 2L))
    )
    expect_identical(
      round(res[[2]], 2),
      structure(c(0.67, 0.2, 0.33, 0.8), .Dim = c(2L, 2L))
    )
    
    mat2 <- define_matrix(
      C, C,
      a, 1-a
    )
    expect_error(
      heemod:::eval_matrix(mat2, par)
    )
  }
)

test_that(
  "compute_counts fails when needed", {
    lm <- structure(list(
      structure(c(0.5, 0.1, 0.5, 0.9),
                .Dim = c(2L, 2L)),
      structure(c(0.67, 0.2, 0.33, 0.8),
                .Dim = c(2L, 2L))),
      class = c("eval_matrix", "list"),
      state_names = c("A", "B"))
    
    expect_error(
      heemod:::compute_counts(
        lm, init = c(10, 0, 0), method = "end")
    )
    expect_error(
      heemod:::compute_counts(
        lm, init = c(10), method = "end")
    )
    expect_error(
      heemod:::compute_counts(
        lm, init = c(10, 0), method = "endzzz")
    )
  }
)

test_that(
  "compute_counts works", {
    lm <- structure(list(
      structure(c(0.5, 0.1, 0.5, 0.9),
                .Dim = c(2L, 2L)),
      structure(c(0.67, 0.2, 0.33, 0.8),
                .Dim = c(2L, 2L))),
      class = c("eval_matrix", "list"),
      state_names = c("A", "B"))
    
    expect_identical(
      dim(heemod:::compute_counts(
        lm, init = c(10, 0), method = "end")),
      c(2L, 2L)
    )
    expect_identical(
      dim(heemod:::compute_counts(
        lm, init = c(10, 0), method = "beginning")),
      c(2L, 2L)
    )
    expect_identical(
      dim(heemod:::compute_counts(
        lm, init = c(10, 0), method = "life-table")),
      c(2L, 2L)
    )
    expect_identical(
      dim(heemod:::compute_counts(
        lm, init = c(10, 0), method = "half-cycle")),
      c(2L, 2L)
    )
    
    expect_equivalent(
      unlist(heemod:::compute_counts(
        lm, init = c(10, 0), method = "end")),
      c(10, 5, 0, 5)
    )
    expect_equivalent(
      unlist(heemod:::compute_counts(
        lm, init = c(10, 0), method = "beginning")),
      c(5.00, 4.35, 5.00, 5.65)
    )
    expect_equivalent(
      unlist(heemod:::compute_counts(
        lm, init = c(10, 0), method = "life-table")),
      c(7.500, 4.675, 2.500, 5.325)
    )
    expect_equivalent(
      unlist(heemod:::compute_counts(
        lm, init = c(10, 0), method = "half-cycle")),
      c(10.000,  6.525,  5.000,  8.475)
    )
  }
)
