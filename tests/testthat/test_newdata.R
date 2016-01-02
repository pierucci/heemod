context("Testing newdata and probabilistic analysis")

test_that(
  "run_newdata works", {
    
    mod1 <-
      define_model(
        parameters = define_parameters(
          age_init = 60,
          age = age_init + markov_cycle
        ),
        transition_matrix = define_matrix(
          .5, .5,
          .1, .9
        ),
        define_state(
          cost = 543 + age * 5
        ),
        define_state(
          cost = 432 + age
        )
        
      )
    
    mod2 <-
      define_model(
        parameters = define_parameters(
          age_init = 60,
          age = age_init + markov_cycle
        ),
        transition_matrix = define_matrix(
          .5, .5,
          .1, .9
        ),
        define_state(
          cost = 789 * age / 10
        ),
        define_state(
          cost = 456 * age / 10
        )
      )
    
    res2 <- run_model(
      mod1, mod2,
      init = 1:0,
      cycles = 10
    )
    # generating table with new parameter sets
    new_tab <- data.frame(
      age_init = 40:50
    )
    
    # with run_model result
    ndt1 <- run_newdata(res2, newdata = new_tab)
    
    expect_output(
      str(ndt1),
      "List of 2
 $ A:Classes",
      fixed= TRUE
    )
    
    expect_output(
      str(ndt1),
      "and 'data.frame':	11 obs. of  2 variables:
  ..$ age_init: int [1:11] 40 41 42 43 44 45 46 47 48 49 ...
  ..$ cost    : num [1:11] 5418 5436 5455 5474 5493 ..",
      fixed= TRUE
    )
  }
)

test_that(
  "Probabilistic analysis works", {
    
    # example for run_probabilistic
    
    mod1 <-
      define_model(
        parameters = define_parameters(
          age_init = 60,
          cost_init = 1000,
          age = age_init + markov_cycle
        ),
        transition_matrix = define_matrix(
          .5, .5,
          .1, .9
        ),
        define_state(
          cost = cost_init + age * 5
        ),
        define_state(
          cost = cost_init + age
        )
        
      )
    
    # running several models
    mod2 <-
      define_model(
        parameters = define_parameters(
          age_init = 60,
          age = age_init + markov_cycle
        ),
        transition_matrix = define_matrix(
          .5, .5,
          .1, .9
        ),
        define_state(
          cost = 789 * age / 10
        ),
        define_state(
          cost = 456 * age / 10
        )
        
      )
    
    res2 <- run_model(
      mod1, mod2,
      init = 1:0,
      cycles = 10
    )
    
    rsp <- define_resample(
      age_init ~ normal(60, 10),
      cost_init ~ normal(1000, 100),
      
      correlation = matrix(c(
        1, .4,
        .4, 1
      ), byrow = TRUE, ncol = 2)
    )
    
    set.seed(1)
    # with run_model result
    ndt1 <- run_probabilistic(res2, resample = rsp, N = 10)
    ndt2 <- run_probabilistic(res2, resample = rsp, N = 1)
    
    x=define_resample(
      rate1 + rate2 + rate3 ~ multinom(10, 50, 40),
      a + b ~ multinom(15, 30)
    )
    
    set.seed(1)
    
    res2 <- heemod:::eval_resample(x, 2)
    
    expect_output(
      str(ndt1),
      "List of 2
 $ A:Classes",
      fixed = TRUE
    )
    
    expect_output(
      str(ndt1),
      "and 'data.frame':	10 obs. of  3 variables:
  ..$ age_init : num [1:10] 65.5 71.3 76.5 74.4 59.9 ...
  ..$ cost_init: num [1:10] 1118 948 1212 1024 1045 ...
  ..$ cost     : num [1:10] 12515 10923 13662 11742 11",
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
    
    expect_output(
      str(ndt2),
      "and 'data.frame':	1 obs. of  3 variables:
  ..$ age_init : num 66.6
  ..$ cost_init: num 930
  ..$ cost     : num 10653",
      fixed = TRUE
    )
    
  }
)
