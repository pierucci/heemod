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
        states = define_state_list(
          define_state(
            cost = 543 + age * 5
          ),
          define_state(
            cost = 432 + age
          )
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
        states = define_state_list(
          define_state(
            cost = 789 * age / 10
          ),
          define_state(
            cost = 456 * age / 10
          )
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
 $ A:Classes 'tbl_df', 'tbl' and 'data.frame':	11 obs. of  2 variables:
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
        states = define_state_list(
          define_state(
            cost = cost_init + age * 5
          ),
          define_state(
            cost = cost_init + age
          )
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
        states = define_state_list(
          define_state(
            cost = 789 * age / 10
          ),
          define_state(
            cost = 456 * age / 10
          )
        )
      )
    
    res2 <- run_model(
      mod1, mod2,
      init = 1:0,
      cycles = 10
    )
    
    rsp <- define_resample(
      age_init = r_norm(60, 10),
      cost_init = r_norm(1000, 100),
      correlation = matrix(c(
        1, .4,
        .4, 1
      ), byrow = TRUE, ncol = 2)
    )
    
    set.seed(1)
    # with run_model result
    ndt1 <- run_probabilistic(res2, resample = rsp, N = 10)
    
    expect_output(
      str(ndt1),
      "List of 2
 $ A:Classes 'tbl_df', 'tbl' and 'data.frame':	10 obs. of  3 variables:
  ..$ age_init : num [1:10] 65.5 71.3 76.5 74.4 59.9 ...
  ..$ cost_init: num [1:10] 1118 948 1212 1024 1045 ...
  ..$ cost     : num [1:10] 12515 10923 13662 11742 11",
      fixed = TRUE
    )
    
  }
)
