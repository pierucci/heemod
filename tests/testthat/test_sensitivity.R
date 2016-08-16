context("Sensitivity analysis")

test_that(
  "define sensitivity", {
    se1 <- define_sensitivity(
      a = c(10, 45),
      b = c(.5, 1.5)
    )
    expect_output(
      str(se1),
      "4 obs. of  2 variables:
 $ a: num  10 45 NA NA
 $ b: num  NA NA 0.5 1.5",
      fixed = TRUE
    )
    expect_error(
      define_sensitivity(
        a = c(10, 45, 20),
        b = c(.5, 1.5)
      )
    )
    expect_error(
      define_sensitivity(
        c(10, 45),
        b = c(.5, 1.5)
      )
    )
    expect_error(
      define_sensitivity(
        b = c(10, 45),
        b = c(.5, 1.5)
      )
    )
    expect_error(
      define_sensitivity(
        C = c(10, 45),
        b = c(.5, 1.5)
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
      define_model(
        transition_matrix = define_matrix(
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
      define_model(
        transition_matrix = define_matrix(
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
    
    res2 <- run_models(
      mod1, mod2,
      parameters = param,
      init = c(100, 0),
      cycles = 10,
      cost = cost,
      effect = ly
    )
    res3 <- suppressWarnings(run_models(
      mod1, mod2,
      parameters = param,
      init = c(100, 0),
      cycles = 10
    ))
    
    ds <- define_sensitivity(
      p1 = c(.1, .9),
      p2 = c(.1, .3)
    )
    
    x <- run_sensitivity(res2, ds)
    
    expect_output(
      str(head(as.data.frame(x))),
      '6 obs. of  8 variables:
 $ cost        : num  514389 451356 456666 475359 703168 ...
 $ ly          : num  871 587 611 695 871 ...
 $ .mod        :List of 6',
      fixed = TRUE
    )
    
    expect_error(run_sensitivity(res3, ds))
    
    expect_output(
      str(summary(x)),
      '8 obs. of  8 variables:
 $ .model_names: chr  "I" "II" "I" "II" ...
 $ .par_names  : chr  "p1" "p1" "p1" "p1" ...
 $ .par_value  : num  0.1 0.1 0.9 0.9 0.1 0.1 0.3 0.3
 $ .cost       : num  514389 703168 451356 514069 456666 ...
 $ .effect     : num  871 871 587 587 611 ...
 $ .dcost      : num  NA 188779 NA 62712 NA ...
 $ .deffect    : num  NA 0 NA 0 NA 0 NA 0
 $ .icer       : num  -Inf Inf -Inf Inf -Inf ...',
      fixed = TRUE
    )
    
    expect_output(
      print(x),
      "p1 = 0.1 (I)  514389.5 871.1237             -",
      fixed = TRUE
    )
    
  })

