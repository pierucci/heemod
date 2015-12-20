context("Parameters definition")

test_that(
  "Parameter definition and update", {
    par1 <- define_parameters(
      a = 1234,
      b = a + 543
    )
    expect_output(
      str(par1),
      'List of 2
 $ a:List of 2
  ..$ expr: num 1234',
      fixed = TRUE
    )
    expect_output(
      print(par1),
      '2 unevaluated parameters.

a = 1234
b = a + 543',
      fixed = TRUE
    )
    expect_error(
      define_parameters(
        markov_cycle = 432
      )
    )
    expect_output(
      str(
        modify(
          par1,
          a = 4321,
          c = 333
        )
      ),
      'List of 3
 $ a:List of 2
  ..$ expr: num 4321',
      fixed = TRUE
    )
    expect_output(
      str(
        modify(
          par1,
          a = 4321,
          c = 333,
          BEFORE = a
        )
      ),
      'List of 3
 $ c:List of 2
  ..$ expr: num 333',
      fixed = TRUE
    )
    expect_equal(
      length(
        modify(
          par1,
          a = 4321,
          c = 333,
          BEFORE = a
        )
      ),
      3
    )
    expect_error(
      modify(
        par1,
        markov_cycle = 474
      )
    )
    expect_equal(
      get_parameter_names(par1),
      c("a", "b")
    )
  }
)

test_that(
  "Parameter evaluation", {
    par1 <- define_parameters(
      a = 2,
      b = a * markov_cycle
    )
    e_par1 <- eval_parameters(
      par1, 10
    )
    expect_output(
      print(e_par1),
      "2 evaluated parameters, 10 Markov cycles.

Source: local data frame [10 x 3]

   markov_cycle     a     b
          (int) (dbl) (dbl)
1             0     2     0
2             1     2     2
3             2     2     4",
      fixed = TRUE
    )
    expect_output(
      str(e_par1),
      "10 obs\\. of  3 variables"
    )
    expect_equal(
      get_parameter_names(e_par1),
      c("a", "b")
    )
  }
)