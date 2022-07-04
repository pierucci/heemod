context("Probabilistic analysis")


test_that(
  "Probabilistic analysis works", {
    mod1 <-
      define_strategy(
        transition = define_transition(
          .5, .5,
          .1, .9
        ),
        define_state(
          cost = cost_init + age * 5,
          ly = 1
        ),
        define_state(
          cost = cost_init + age,
          ly = 1 * 1 / age
        )
        
      )
    mod2 <-
      define_strategy(
        transition = define_transition(
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
    
    res2 <- run_model(
      mod1, mod2,
      parameters = define_parameters(
        age_init = 60,
        cost_init = 1000,
        age = age_init + markov_cycle
      ),
      init = 1:0,
      cycles = 10,
      cost = cost,
      effect = ly,
      method = "end"
    )
    
    rsp1 <- define_psa(
      age_init ~ normal(60, 10),
      cost_init ~ normal(1000, 100),
      
      correlation = matrix(c(
        1, .4,
        .4, 1
      ), byrow = TRUE, ncol = 2)
    )
    
    rsp2 <- define_psa(
      age_init ~ normal(60, 10),
      cost_init ~ normal(1000, 100),
      
      correlation = define_correlation(age_init, cost_init, .4)
    )
    
    rsp2_lazy <- define_psa_(
      rlang::quos(
        age_init ~ normal(60, 10),
        cost_init ~ normal(1000, 100)
      ),
      correlation = define_correlation(age_init, cost_init, .4)
    ) 
    
    expect_equal(
      rsp2, 
      rsp2_lazy
    )
    set.seed(1)
    # with run_model result
    ndt1 <- run_psa(res2, psa = rsp1, N = 10)
    ndt2 <- run_psa(res2, psa = rsp1, N = 1)
    
    expect_error(
      run_psa(res2, psa = rsp1, N = NULL)
    )
    
    plot(ndt1, type = "ce")
    plot(ndt1, type = "ac")
    
    set.seed(1)
    ndt3 <- run_psa(res2, psa = rsp2, N = 10)
    
    x <- define_psa(
      rate1 + rate2 + rate3 ~ multinomial(10, 50, 40),
      a + b ~ multinomial(15, 30)
    )
    
    x_lazy <- define_psa_(
      rlang::quos(
        rate1 + rate2 + rate3 ~ multinomial(10, 50, 40),
        a + b ~ multinomial(15, 30)
      )
    )
    
    expect_equal(
      x, 
      x_lazy
    )
    
    set.seed(1)
    
    res2 <- heemod:::eval_resample(x, 2)
    
    expect_equal(
      nrow(ndt2$psa), 2
    )
    
    expect_equal(
      round(ndt2$psa$.cost), c(10653, 38163)
    )
    
    expect_equal(
      round(summary(ndt2)$res_comp$.cost[2]), 27511
    )
    
    expect_equal(
      res2,
      structure(list(
        rate1 = c(0.0988103464248714, 0.136581011164098),
        rate2 = c(0.494447779167122, 0.533418525880708),
        rate3 = c(0.406741874408007, 0.330000462955194),
        a = c(0.217007557277859, 0.311016070577393),
        b = c(0.782992442722141, 0.688983929422607)),
        class = "data.frame",
        row.names = c(NA, -2L),
        .Names = c("rate1", "rate2", "rate3", "a", "b"))
    )
    
    expect_identical(ndt1, ndt3)
    
    expect_equal(
      nrow(ndt1$psa), 20
    )
    
    expect_equal(
      round(ndt1$psa$.cost)[1:3], c(12515, 10923, 13662)
    )
    
    expect_equal(
      round(summary(ndt1)$res_comp$.cost[2]), 25085
    )
    
    rsp3 <- define_psa(
      age_init ~ lognormal(60, 10),
      cost_init ~ gamma (1000, 100),
      p_trans ~ binomial(.5, 100),
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
    res2 <- suppressWarnings(run_model(
      mod1, mod2,
      parameters = define_parameters(
        age_init = 60,
        cost_init = 1000,
        age = age_init + markov_cycle
      ),
      init = 1:0,
      cycles = 10,
      method = "end"
    ))
    expect_error(run_psa(res3, psa = rsp2, N = 10))
    expect_error(
      define_psa(
        age_init ~ normal(60, 10),
        age_init ~ normal(1000, 100),
        
        correlation = matrix(c(
          1, .4,
          .4, 1
        ), byrow = TRUE, ncol = 2)
      )
    )
    expect_error(
      define_psa(
        x ~ normal(60, 10),
        y ~ 0
      )
    )
    expect_error(
      define_psa(
        ~ normal(60, 10)
      )
    )
    expect_error(
      define_correlation(age_init, cost_init, .4, .5)
    )
    expect_error(
      define_correlation(age_init, cost_init, 2)
    )
    expect_error(
      define_correlation(age_init, cost_init, .4, age_init, cost_init, .5)
    )
    expect_error(
      define_psa(
        x ~ normal(1, 2),
        b + c ~ multinomial(20, 30),
        c + e + f ~ multinomial(12, 34, 56)
      )
    )
    expect_error(
      define_psa(
        x ~ normal(1, 2),
        c + c ~ multinomial(20, 30),
        d + e + f ~ multinomial(12, 34, 56)
      )
    )
  }
)
