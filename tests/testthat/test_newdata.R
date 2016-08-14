context("Heterogeneity & probabilistic")

test_that(
  "Heterogeneity analysis", {
    mod1 <-
      define_model(
        transition_matrix = define_matrix(
          .5, .5,
          .1, .9
        ),
        define_state(
          cost = 543 + age * 5,
          ly = 1
        ),
        define_state(
          cost = 432 + age,
          ly = 1 * age / 100
        )
        
      )
    
    mod2 <-
      define_model(
        transition_matrix = define_matrix(
          .5, .5,
          .1, .9
        ),
        define_state(
          cost = 789 * age / 10,
          ly = 1
        ),
        define_state(
          cost = 456 * age / 10,
          ly = 1 * age / 200
        )
        
      )
    
    res <- run_models(
      mod1, mod2,
      parameters = define_parameters(
        age_init = 60,
        age = age_init + markov_cycle
      ),
      init = 1:0,
      cycles = 10,
      cost = cost,
      effect = ly
    )
    
    # generating table with new parameter sets
    new_tab <- data.frame(
      age_init = 40:80
    )
    
    # with run_model result
    ndt <- run_heterogeneity(res, newdata = new_tab)
    
    expect_output(
      print(ndt),
      'An heterogeneity analysis on 41 parameter sets.',
      fixed= TRUE
    )
    expect_error(
      summary(ndt, model = "II")
    )
    expect_output(
      str(summary(ndt, model = "I")),
      'num [1:3, 1:6] -39070 1.78 -11710 -33960 2.17 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:3] "Cost" "Effect" "ICER"
  ..$ : chr [1:6] "Min." "1st Qu." "Median" "Mean" ...',
      fixed= TRUE
    )
  }
)

test_that(
  "Probabilistic analysis works", {
    
    # example for run_probabilistic
    
    mod1 <-
      define_model(
        transition_matrix = define_matrix(
          .5, .5,
          .1, .9
        ),
        define_state(
          cost = cost_init + age * 5,
          ly = 1
        ),
        define_state(
          cost = cost_init + age,
          ly = 1
        )
        
      )
    mod2 <-
      define_model(
        transition_matrix = define_matrix(
          .5, .5,
          .1, .9
        ),
        define_state(
          cost = 789 * age / 10,
          ly = 1
        ),
        define_state(
          cost = 456 * age / 10,
          ly = 1
        )
        
      )
    
    res2 <- run_models(
      mod1, mod2,
      parameters = define_parameters(
        age_init = 60,
        cost_init = 1000,
        age = age_init + markov_cycle
      ),
      init = 1:0,
      cycles = 10,
      cost = cost,
      effect = ly
    )
    
    rsp1 <- define_distrib(
      age_init ~ normal(60, 10),
      cost_init ~ normal(1000, 100),
      
      correlation = matrix(c(
        1, .4,
        .4, 1
      ), byrow = TRUE, ncol = 2)
    )
    rsp2 <- define_distrib(
      age_init ~ normal(60, 10),
      cost_init ~ normal(1000, 100),
      
      correlation = define_correlation(age_init, cost_init, .4)
    )
    
    set.seed(1)
    # with run_model result
    ndt1 <- run_probabilistic(res2, resample = rsp1, N = 10)
    ndt2 <- run_probabilistic(res2, resample = rsp1, N = 1)
    
    plot(ndt1, type = "ce")
    plot(ndt1, type = "ac")
    
    set.seed(1)
    ndt3 <- run_probabilistic(res2, resample = rsp2, N = 10)
    
    x <- define_distrib(
      rate1 + rate2 + rate3 ~ multinom(10, 50, 40),
      a + b ~ multinom(15, 30)
    )
    
    set.seed(1)
    
    res2 <- heemod:::eval_resample(x, 2)
    
    expect_output(
      str(head(as.data.frame(ndt2))),
      '2 obs. of  8 variables:
 $ cost        : num  10653 38163
 $ ly          : num  10 10
 $ age_init    : num  66.6 66.6
 $ cost_init   : num  930 930
 $ .model_names: chr  "I" "II"
 $ .index      : int  1 1
 $ .cost       : num  10653 38163
 $ .effect     : num  10 10',
      fixed = TRUE
    )

    expect_equal(
      res2,
      structure(
        list(
          rate1 = c(0.101959623961425, 0.137000448427529),
          rate2 = c(0.492102780837178, 0.517550777974819),
          rate3 = c(0.405937595201397, 0.345448773597652),
          a = c(0.25010006361869, 0.319608420079022),
          b = c(0.74989993638131, 0.680391579920978)
        ),
        .Names = c("rate1", "rate2", "rate3", "a", "b"),
        row.names = c(NA, -2L),
        class = "data.frame")
    )
    expect_identical(ndt1, ndt3)
    expect_output(
      str(head(as.data.frame(ndt2))),
      '2 obs. of  8 variables:
 $ cost        : num  10653 38163
 $ ly          : num  10 10
 $ age_init    : num  66.6 66.6
 $ cost_init   : num  930 930
 $ .model_names: chr  "I" "II"
 $ .index      : int  1 1
 $ .cost       : num  10653 38163
 $ .effect     : num  10 10',
      fixed = TRUE
    )
    
    rsp3 <- define_distrib(
      age_init ~ lognormal(60, 10),
      cost_init ~ make_gamma (1000, 100),
      p_trans ~ prop(.5, 100),
      a ~ logitnormal(1, 1)
    )
    set.seed(1)
    
    res3 <- heemod:::eval_resample(rsp3, 2)
    
    expect_output(
      print(res3),
      "  age_init cost_init p_trans         a
1 64.82732  1105.112    0.56 0.4842654
2 77.79468  1164.304    0.57 0.6539179"
    )
    res2 <- suppressWarnings(run_models(
      mod1, mod2,
      parameters = define_parameters(
        age_init = 60,
        cost_init = 1000,
        age = age_init + markov_cycle
      ),
      init = 1:0,
      cycles = 10
    ))
    expect_error(run_probabilistic(res3, resample = rsp2, N = 10))
  }
)
