context("Sensitivity analysis")

test_that(
  "define sensitivity", {
    se1 <- define_dsa(
      a, 10, 45,
      b, .5, 1.5
    )
    expect_identical(
      dim(se1$dsa),
      c(4L, 2L)
    )
    expect_is(
      se1$dsa$a,
      "list"
    )
    expect_s3_class(
      se1$dsa$a[[1]],
      "lazy"
    )
    expect_error(
      define_dsa(
        a, 10, 45, 20,
        b, .5, 1.5
      )
    )
    expect_error(
      define_dsa(
        10, 45,
        b, .5, 1.5
      )
    )
    expect_error(
      define_dsa(
        b, 10, 45,
        b, .5, 1.5
      )
    )
    expect_error(
      define_dsa(
        C, 10, 45,
        b, .5, 1.5
      )
    )
  })

test_that(
  "run sensitivity", {
    param <- define_parameters(
      p1 = .5,
      p2 = .2
    )
    mod1 <-
      define_strategy(
        transition = define_transition(
          C, p1,
          p2, C
        ),
        define_state(
          cost = 543,
          ly = 1
        ),
        define_state(
          cost = 432,
          ly = .5
        )
      )
    
    mod2 <-
      define_strategy(
        transition = define_transition(
          C, p1,
          p2, C
        ),
        define_state(
          cost = 789,
          ly = 1
        ),
        define_state(
          cost = 456,
          ly = .5
        )
      )
    
    res2 <- run_model(
      mod1, mod2,
      parameters = param,
      init = c(100, 0),
      cycles = 10,
      cost = cost,
      effect = ly,
      method = "beginning"
    )
    res3 <- suppressWarnings(run_model(
      mod1, mod2,
      parameters = param,
      init = c(100, 0),
      cycles = 10
    ))
    
    ds <- define_dsa(
      p1, .1, .9,
      p2, .1, .3
    )
    
    x <- run_dsa(res2, ds)
    
    expect_equal(
      round(x$dsa$.cost),
      c(514389, 451356, 703168, 514069,
        456666, 475359, 529998, 586078)
    )
    
    expect_error(run_dsa(res3, ds))
    
    sx <- summary(x)
    
    expect_equal(
      round(sx$res_comp$.cost),
      c(0, 1888, 0, 627, 0, 733, 0, 1107)
    )

    plot(x, result = "cost")
    plot(x, result = "effect")
  })

test_that(
  "discount rate as a parameter works", {
    param <- define_parameters(
      p1 = .5,
      p2 = .2,
      r = .05
    )
    mod1 <- define_strategy(
      transition = define_transition(
        C, p1,
        p2, C
      ),
      define_state(
        cost = discount(543, r),
        ly = 1
      ),
      define_state(
        cost = discount(432, r),
        ly = .5
      )
    )
    
    mod2 <- define_strategy(
      transition = define_transition(
        C, p1,
        p2, C
      ),
      define_state(
        cost = 789,
        ly = 1
      ),
      define_state(
        cost = 456,
        ly = .8
      )
    )
    
    res2 <- run_model(
      mod1, mod2,
      parameters = param,
      init = c(100, 0),
      cycles = 10,
      cost = cost,
      effect = ly
    )
    
    ds <- define_dsa(
      p1, .1, .9,
      p2, .1, .3,
      r, .05, .1
    )
    
    
    x <- summary(run_dsa(res2, ds))
    
    .icer <- c(NA, 3988, NA, 668, NA, 761, NA, 1195,
               NA, 978, NA, 1300)
    
    expect_identical(round(x$res_comp$.icer), .icer)
  }
)
