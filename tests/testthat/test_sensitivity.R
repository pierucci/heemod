context("Sensitivity analysis")

test_that(
  "define sensitivity", {
    se1 <- define_sensitivity(
      a, 10, 45,
      b, .5, 1.5
    )
    expect_identical(
      dim(se1),
      c(4L, 2L)
    )
    expect_is(
      se1$a,
      "list"
    )
    expect_s3_class(
      se1$a[[1]],
      "lazy"
    )
    expect_output(
      print(se1),
      "  a  b  
1 10 -  
2 45 -  
3 -  0.5
4 -  1.5"
    )
    expect_error(
      define_sensitivity(
        a, 10, 45, 20,
        b, .5, 1.5
      )
    )
    expect_error(
      define_sensitivity(
        10, 45,
        b, .5, 1.5
      )
    )
    expect_error(
      define_sensitivity(
        b, 10, 45,
        b, .5, 1.5
      )
    )
    expect_error(
      define_sensitivity(
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
      effect = ly,
      method = "beginning"
    )
    res3 <- suppressWarnings(run_models(
      mod1, mod2,
      parameters = param,
      init = c(100, 0),
      cycles = 10
    ))
    
    ds <- define_sensitivity(
      p1, .1, .9,
      p2, .1, .3
    )
    
    x <- run_sensitivity(res2, ds)
    
    expect_output(
      str(head(as.data.frame(x))),
      '6 obs. of  4 variables:
 $ .mod        :List of 6',
      fixed = TRUE
    )
    
    expect_error(run_sensitivity(res3, ds))
    
    expect_output(
      str(summary(x)),
      '8 obs. of  8 variables:
 $ .model_names: chr  "I" "II" "I" "II" ...
 $ .par_names  : chr  "p1" "p1" "p1" "p1" ...
 $ .par_value  : chr  "0.1" "0.1" "0.9" "0.9" ...
 $ .cost       : num  514389 703168 451356 514069 456666 ...
 $ .effect     : num  871 871 587 587 611 ...
 $ .dcost      : num  NA 188779 NA 62712 NA ...
 $ .deffect    : num  NA 0 NA 0 NA 0 NA 0
 $ .icer       : num  -Inf Inf -Inf Inf -Inf ...',
      fixed = TRUE
    )
    
    expect_output(
      print(x),
      "p1 = 0.1 (I)  514389.5 871.1237",
      fixed = TRUE
    )
    
    plot(x, type = "simple", result = "cost")
    plot(x, type = "simple", result = "effect")
    
    expect_error(
      plot(x, type = "difference", result = "cost")
    )
    
    plot(x, type = "difference", result = "cost", model = 2)
    plot(x, type = "difference", result = "effect", model = 2)
    plot(x, type = "difference", result = "icer", model = 2)
  })

test_that(
  "discount rate as a parameter works", {
    param <- define_parameters(
      p1 = .5,
      p2 = .2,
      r = .05
    )
    mod1 <- define_model(
      transition_matrix = define_matrix(
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
    
    mod2 <- define_model(
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
        ly = .8
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
    
    ds <- define_sensitivity(
      p1, .1, .9,
      p2, .1, .3,
      r, .05, .1
    )
    
    
    x <- summary(run_sensitivity(res2, ds))
    
    .icer <- c(-Inf, 3988, -Inf, 668, -Inf, 761, -Inf, 1195,
               -Inf, 978, -Inf, 
               1300)
    
    expect_identical(round(x$.icer), .icer)
  }
)
