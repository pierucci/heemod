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
    e_par1 <- eval_parameters(
      par1, 10
    )
    expect_output(
      print(e_par1),
      "2 evaluated parameters, 10 Markov cycles.

Source: local data frame [10 x 3]

   markov_cycle     a     b
          (int) (dbl) (dbl)
1             0    12    12
2             1    12    16
3             2    12    20
4             3    12    24
5             4    12    28
6             5    12    32
7             6    12    36
8             7    12    40
9             8    12    44
10            9    12    48",
      fixed = TRUE
    )
    expect_output(
      str(e_par1),
      "Classes ‘eval_parameters’ and 'data.frame':	10 obs. of  3 variables:
 $ markov_cycle: int  0 1 2 3 4 5 6 7 8 9
 $ a",
      fixed = TRUE
    )
    expect_equal(
      get_parameter_names(e_par1),
      c("a", "b")
    )
  }
)
