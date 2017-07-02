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
          a = 4321
        )
      ),
      'List of 2
 $ a:List of 2
  ..$ expr: num 4321',
      fixed = TRUE
    )
    expect_identical(
      names(modify(
        par1,
        a = 4321,
        c = 333
      )),
      letters[1:3]
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
    expect_output(
      print(define_parameters()),
      "0 unevaluated parameter.",
      fixed = TRUE
    )
    
    expect_identical(
      to_text_dots(par1),
      to_text_dots(modify(par1))
    )
  }
)

test_that(
  "Parameter evaluation", {
    par1 <- define_parameters(
      a = 2,
      b = a * markov_cycle
    )
    e_par1 <- heemod:::eval_parameters(
      par1, 10
    )
    expect_output(
      str(e_par1),
      "100 obs\\. of  6 variables"
    )
    expect_equal(
      get_parameter_names(e_par1),
      c("a", "b")
    )
  }
)

test_that(
  "Reserved names", {
    expect_error(
      heemod:::check_names(NULL)
    )
    expect_error(
      heemod:::check_names(NA)
    )
    expect_error(
      heemod:::check_names(c("a", NA))
    )
    expect_error(
      heemod:::check_names(c("a", ""))
    )
    expect_error(
      heemod:::check_names(c("a", "markov_cycle"))
    )
    expect_error(
      heemod:::check_names(c("a", "C"))
    )
    expect_error(
      heemod:::check_names(c("a", ".b"))
    )
  }
)

test_that(
  "we catch infinite parameters", {
    par1 <- define_parameters(
      a = 2,
      b = 1 / (markov_cycle - 3)
    )
    options(heemod.inf_parameter = "ignore")
    e_par1 <- heemod:::eval_parameters(
      par1, 5
    )
    expect_equal(
      as.numeric(unlist(e_par1[,"b"])),
      rep(1/(-2:2),5)
    )
    options(heemod.inf_parameter = "warning")
    expect_warning(
      e_par1 <- heemod:::eval_parameters(
        par1, 5
      )
    )
    options(heemod.inf_parameter = "stop")
    expect_error(
      e_par1 <- heemod:::eval_parameters(
        par1, 5
      )
    )
  }
)

test_that(
  "we correctly report parameter evaluation errors", {
    par1 <- define_parameters(
      a = 2,
      b = 3,
      d = A + b,
      g = a + b
    )
    expect_error(
      eval_parameters(par1),
      "Error in parameter: d",
      fixed = TRUE)
  }
)
