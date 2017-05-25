context("Running model")

test_that(
  "Strange inputs generate errors", {
    par1 <- define_parameters(
      a = .1,
      b = 1 / (markov_cycle + 1)
    )
    mat1 <- define_transition(
      state_names = c("X1", "X2"),
      1-a, a,
      1-b, b
    )
    mat2 <- define_transition(
      state_names = c("X1", "X3"),
      1-a, a,
      1-b, b
    )
    s1 <- define_state(
      x = 234,
      y = 123
    )
    s2 <- define_state(
      x = 987,
      y = 1726
    )
    
    mod1 <- define_strategy(
      transition = mat1,
      X1 = s1,
      X2 = s2
    )
    s3 <- define_state(
      x = 987,
      y = 876
    )
    s4 <- define_state(
      x = 456,
      y = 1029
    )
    s5 <- define_state(
      a = 456,
      b = 1029
    )
    mod2 <- define_strategy(
      transition = mat1,
      X1 = s3,
      X2 = s4
    )
    mod3 <- define_strategy(
      transition = mat2,
      X1 = s3,
      X3 = s4
    )
    mod4 <- define_strategy(
      transition = mat1,
      X1 = s5,
      X2 = s5
    )
    expect_error(
      run_model(
        mod1, mod2,
        parameters = par1,
        init = c(1, 2, 3)
      )
    )
    expect_error(
      run_model(
        mod1, mod2,
        parameters = par1,
        init = c(X3 = 1, X4 = 2)
      )
    )
    expect_error(
      run_model(
        mod1, mod2,
        parameters = par1,
        init = c(-1, 0)
      )
    )
    expect_error(
      run_model(
        mod1, mod2,
        parameters = par1,
        init = c(1, 0, 0)
      )
    )
    expect_error(
      run_model(
        mod1, mod2,
        parameters = par1,
        init = c(NA, 1)
      )
    )
    expect_error(
      run_model(
        mod1, mod2,
        parameters = par1,
        cycles = 0
      )
    )
    expect_error(
      run_model(
        mod1, mod2,
        parameters = par1,
        cycles = 11.5
      )
    )
    expect_error(
      run_model(
        mod1,
        parameters = par1, list()
      )
    )
    expect_error(
      run_model(
        mod1, mod2,
        parameters = par1,
        method = "zzz"
      )
    )
    expect_error(
      run_model(
        mod1, mod3, parameters = par1
      )
    )
    expect_error(
      run_model(
        mod1, mod4, parameters = par1
      )
    )
    expect_warning(
      run_model(
        mod1, mod2,
        parameters = par1
      )
    )
    expect_warning(
      run_model(
        mod1, mod2,
        parameters = par1,
        cost = x
      )
    )
  }
)

test_that(
  "run_model behaves as expected", {
    par1 <- define_parameters(
      a = .1,
      b = 1 / (markov_cycle + 1)
    )
    mat1 <- define_transition(
      state_names = c("X1", "X2"),
      1-a, a,
      1-b, b
    )
    s1 <- define_state(
      x = 234,
      y = 123
    )
    s2 <- define_state(
      x = 987,
      y = 1726
    )
    mod1 <- define_strategy(
      transition = mat1,
      X1 = s1,
      X2 = s2
    )
    s3 <- define_state(
      x = 987,
      y = 876
    )
    s4 <- define_state(
      x = 456,
      y = 1029
    )
    mod2 <- define_strategy(
      transition = mat1,
      X1 = s3,
      X2 = s4
    )
    
    expect_identical(
      run_model(mod1, mod2,
                 parameters = par1, init = c(1000L, 0L), cost = x, effect = y),
      run_model(mod1, mod2,
                 parameters = par1, cost = x, effect = y)
    )
    expect_identical(
      run_model(mod1, mod2,
                 parameters = par1, cost = x, effect = y),
      run_model(I = mod1, II = mod2,
                 parameters = par1, cost = x, effect = y)
    )
    expect_warning(
      run_model(I = mod1, mod2,
                 parameters = par1, cost = x, effect = y)
    )
    expect_warning(
      summary(run_model(mod1, mod2,
                         parameters = par1))
    )
    expect_equal(
      run_model(mod1, mod2,
                parameters = par1, cost = x, effect = y,
                method = "beginning")$run_model$.cost,
      c(309300, 933900)
    )
    
    s_mod <- summary(
      run_model(
        mod1, mod2,
        parameters = par1, cost = x, effect = y,
        method = "beginning"))
    expect_equal(
      s_mod$res_values$x, c(309300, 933900)
    )
    expect_equal(
      s_mod$res_comp$.cost, c(0, 624.6)
    )
    expect_equal(
      round(s_mod$res_comp$.icer, 3)[2], 1.027
    )
    expect_equal(
      s_mod$frontier, c("I", "II")
    )
    
    res_b <- run_model(mod1, mod2,
                        parameters = par1, cost = x, effect = y,
                        method = "beginning")
    res_e <- run_model(mod1, mod2,
                        parameters = par1, cost = x, effect = y,
                        method = "end")
    res_l <- run_model(mod1, mod2,
                        parameters = par1, cost = x, effect = y,
                        method = "life-table")
    
    plot(res_b, result = "counts")
    plot(res_b, result = "values")
    plot(res_b, result = "ce")
    
    expect_equal(
      round(summary(res_b)$res_comp$.icer[2], 3),
      1.027
    )
    expect_equal(
      round(summary(res_e)$res_comp$.icer[2], 3),
      1
    )
    expect_equal(
      round(summary(res_l)$res_comp$.icer[2], 3),
      1.012
    )
    
    expect_error(
      run_model(mod1, mod2,
                 parameters = par1, cost = x, effect = y,
                 method = "testtest")
    )
    rm <- run_model(mod1, mod2,
                     parameters = par1, cost = x, effect = y,
                     cycles = 5)
    expect_equivalent(
      round(get_counts(rm, 1)$count),
      c(950, 888, 879, 885, 890, 950,
        888, 879, 885, 890, 50, 112, 
        121, 115, 110, 50, 112, 121, 115, 110)
    )
    
    expect_equivalent(
      get_eval_init(get_eval_strategy_list(rm)[[1]]),
      c(1000, 0)
    )
    
    
    plot(rm, panels = "by_strategy")
    plot(rm, panels = "by_state")
  }
)

test_that("Discounting", {
  
  par1 <- define_parameters(
    a = .1,
    b = 1 / (markov_cycle + 1),
    c1 = 987,
    c2 = 876,
    c3 = 1726
  )
  mat1 <- define_transition(
    state_names = c("X1", "X2"),
    1-a, a,
    1-b, b
  )
  
  s1 <- define_state(
    x = 234,
    y = 123
  )
  s2 <- define_state(
    x = 987,
    y = 1726
  )
  mod1 <- define_strategy(
    transition = mat1,
    X1 = s1,
    X2 = s2
  )
  
  s3 <- define_state(
    x = discount(c1, .1),
    y = discount(c2, .05, TRUE)
  )
  s4 <- define_state(
    x = 456,
    y = 1029
  )
  mod2 <- define_strategy(
    transition = mat1,
    X1 = s3,
    X2 = s4
  )
  
  s5 <- define_state(
    x = discount(c1, 0),
    y = discount(c3, 0)
  )
  mod3 <- define_strategy(
    transition = mat1,
    X1 = s1,
    X2 = s5
  )
  
  s6 <- define_state(
    x = discount(c1, 1.5),
    y = discount(c3, .05, TRUE)
  )
  s4 <- define_state(
    x = 456,
    y = 1029
  )
  mod4 <- define_strategy(
    transition = mat1,
    X1 = s6,
    X2 = s4
  )
  res <- run_model(mod1, mod2, cycles = 10,
                    parameters = par1, cost = x, effect = y,
                    method = "beginning")
  expect_equal(
    round(summary(res)$res_comp$.icer[2], 3), 0.785
  )
  res1 <- run_model(mod1, mod2, cycles = 10,
                     parameters = par1, cost = x, effect = y,
                     method = "beginning")
  res2 <- run_model(mod3, mod2, cycles = 10,
                     parameters = par1, cost = x, effect = y,
                     method = "beginning")
  
  expect_equal(
    round(summary(res1)$res_comp$.icer[2], 3), 0.785
  )
  expect_equal(
    round(summary(res2)$res_comp$.icer[2], 3), 0.785
  )
  
  
  expect_error(
    run_model(mod1, mod4, cycles = 10,
               parameters = par1, cost = x, effect = y)
  )
})

test_that(
  "check_model_index works", {
    
    par1 <- define_parameters(
      a = .1,
      b = 1 / (markov_cycle + 1)
    )
    mat1 <- define_transition(
      state_names = c("X1", "X2"),
      1-a, a,
      1-b, b
    )
    s1 <- define_state(
      x = 234,
      y = 123
    )
    s2 <- define_state(
      x = 987,
      y = 1726
    )
    mod1 <- define_strategy(
      transition = mat1,
      X1 = s1,
      X2 = s2
    )
    s3 <- define_state(
      x = 987,
      y = 876
    )
    s4 <- define_state(
      x = 456,
      y = 1029
    )
    mod2 <- define_strategy(
      transition = mat1,
      X1 = s3,
      X2 = s4
    )
    
    res <- run_model(
      mod1, mod2,
      parameters = par1, init = c(1000L, 0L),
      cost = x, effect = y)
    
    expect_error(
      heemod:::check_strategy_index(res, 1:2)
    )
    expect_error(
      heemod:::check_strategy_index(res, as.factor("I"))
    )
    expect_error(
      heemod:::check_strategy_index(res, "a")
    )
  }
)

test_that("exactly match HIV model",
          ## compare Decision Modelling for Health Economic Evaluation 
          ##  (Handbooks in Health Economic Evaluation)  1st ed. 2006
          ##  by Briggs Claxton Sculpher 
          ##  Example 2.5 and related online Excel model
          {
            par_mod <- define_parameters(
              rr = ifelse(markov_cycle <= 2, .509, 1),
              cost_lami = ifelse(markov_cycle <= 2, 2086.5, 0),
              cost_zido = 2278
            )
            
            mat_mono <- define_transition(
              1251/1734, 350/1734, 116/1734,  17/1734,
              0,         731/1258, 512/1258,  15/1258,
              0,         0,        1312/1749, 437/1749,
              0,         0,        0,         1.00
            )
            
            mat_comb <- define_transition(
              C, 350/1734*rr, 116/1734*rr, 17/1734*rr,
              0, C,           512/1258*rr, 15/1258*rr,
              0, 0,           C,           437/1749*rr,
              0, 0,           0,           1.00
            )
            
            A_mono <- define_state(
              cost_health = 2756,
              cost_drugs = cost_zido,
              cost_total = discount(
                cost_health + cost_drugs, .06, first = T),
              life_year = 1
            )
            B_mono <- define_state(
              cost_health = 3052,
              cost_drugs = cost_zido,
              cost_total = discount(
                cost_health + cost_drugs, .06, first = T),
              life_year = 1
            )
            C_mono <- define_state(
              cost_health = 9007,
              cost_drugs = cost_zido,
              cost_total = discount(
                cost_health + cost_drugs, .06, first = T),
              life_year = 1
            )
            D_mono <- define_state(
              cost_health = 0,
              cost_drugs = 0,
              cost_total = discount(
                cost_health + cost_drugs, .06, first = T),
              life_year = 0
            )
            
            A_comb <- define_state(
              cost_health = 2756,
              cost_drugs = cost_zido + cost_lami,
              cost_total = discount(
                cost_health + cost_drugs, .06, first = T),
              life_year = 1
            )
            B_comb <- define_state(
              cost_health = 3052,
              cost_drugs = cost_zido + cost_lami,
              cost_total = discount(
                cost_health + cost_drugs, .06, first = T),
              life_year = 1
            )
            C_comb <- define_state(
              cost_health = 9007,
              cost_drugs = cost_zido + cost_lami,
              cost_total = discount(
                cost_health + cost_drugs, .06, first = T),
              life_year = 1
            )
            D_comb <- define_state(
              cost_health = 0,
              cost_drugs = 0,
              cost_total = discount(
                cost_health + cost_drugs, .06, first = T),
              life_year = 0
            )
            
            mod_mono <- define_strategy(
              transition = mat_mono,
              A_mono,
              B_mono,
              C_mono,
              D_mono
            )
            mod_comb <- define_strategy(
              transition = mat_comb,
              A_comb,
              B_comb,
              C_comb,
              D_comb
            )
            
            hiv <- res_mod <- run_model(
              mono = mod_mono,
              comb = mod_comb,
              parameters = par_mod,
              cycles = 20,
              cost = cost_total,
              effect = life_year,
              method = "beginning",
              init = c(1, 0, 0, 0)
            )
            briggs_hiv <- list(
              cost_mono = 44663.4535637,
              cost_comb = 50601.6513312,
              ly_mono = 7.9912066,
              ly_comb = 8.9373889
            )
            briggs_hiv_icer <- 6275.95560
            from_heemod <- 
              summary(hiv)$res_values[, c("cost_total", "life_year")]
            expect_equal(as.numeric(unlist(from_heemod)),
                         as.numeric(unlist(briggs_hiv))
            )
            expect_equal(summary(hiv)$res_comp[2, ".icer"],
                         briggs_hiv_icer
            )
            
          }
)

test_that("Exactly match THR model",
          ## compare Decision Modelling for Health Economic Evaluation 
          ##  (Handbooks in Health Economic Evaluation) 1st ed. 2006
          ##  by Briggs Claxton Sculpher 
          ##  Example 3.5 and related online Excel model
          
          {
            death_prob <- data.frame(
              age = rep(seq(35, 85, 10), each = 2),
              sex = rep(1:0, 6),
              value = c(
                1.51, .99, 3.93,
                2.6, 10.9, 6.7,
                31.6, 19.3, 80.1,
                53.5, 187.9, 154.8
              )/ 1000
            )
            death_prob
            
            param <- define_parameters(
              age_init = 60,
              sex = 0,
              # age increases with cycles
              age = age_init + markov_cycle,
              
              # operative mortality rates
              omrPTHR = .02,
              omrRTHR = .02,
              
              # re-revision mortality rate
              rrr = .04,
              
              # parameters for calculating primary revision rate
              cons = -5.490935,
              ageC = -.0367022,
              maleC = .768536,
              lambda = exp(cons + ageC * age_init + maleC * sex),
              lngamma = 0.3740968,
              gamma = exp(lngamma),
              lnrrNP1 = -1.344474,
              rrNP1 = exp(lnrrNP1),
              
              # revision probability of primary procedure
              standardRR = 1 - exp(lambda * ((markov_cycle - 1) ^ gamma -
                                               markov_cycle ^ gamma)),
              np1RR = 1 - exp(lambda * rrNP1 * ((markov_cycle - 1) ^ gamma - 
                                                  markov_cycle ^ gamma)),
              
              # age-related mortality rate
              sex_cat = ifelse(sex == 0, "FMLE", "MLE"),
              mr = look_up(death_prob, age = age, sex = sex, bin = "age"),
              
              u_successP = .85,
              u_revisionTHR = .30,
              u_successR = .75,
              c_revisionTHR = 5294
            )
            
            mat_standard <- define_transition(
              state_names = c(
                "PrimaryTHR",
                "SuccessP",
                "RevisionTHR",
                "SuccessR",
                "Death"
              ),
              0, C, 0,          0, omrPTHR,
              0, C, standardRR, 0, mr,
              0, 0, 0,          C, omrRTHR+mr,
              0, 0, rrr,        C, mr,
              0, 0, 0,          0, 1
            )
            
            mat_np1 <- define_transition(
              state_names = c(
                "PrimaryTHR",
                "SuccessP",
                "RevisionTHR",
                "SuccessR",
                "Death"
              ),
              0, C, 0,     0, omrPTHR,
              0, C, np1RR, 0, mr,
              0, 0, 0,     C, omrRTHR+mr,
              0, 0, rrr,   C, mr,
              0, 0, 0,     0, 1
            )
            
            mod_standard <- define_strategy(
              transition = mat_standard,
              PrimaryTHR = define_state(
                utility = 0,
                cost = 394
              ),
              SuccessP = define_state(
                utility = discount(u_successP, .015),
                cost = 0
              ),
              RevisionTHR = define_state(
                utility = discount(u_revisionTHR, .015),
                cost = discount(c_revisionTHR, .06)
              ),
              SuccessR = define_state(
                utility = discount(u_successR, .015),
                cost = 0
              ),
              Death = define_state(
                utility = 0,
                cost = 0
              )
            )
            
            mod_np1 <- define_strategy(
              transition = mat_np1,
              PrimaryTHR = define_state(
                utility = 0,
                cost = 579
              ),
              SuccessP = define_state(
                utility = discount(u_successP, .015),
                cost = 0
              ),
              RevisionTHR = define_state(
                utility = discount(u_revisionTHR, .015),
                cost = discount(c_revisionTHR, .06)
              ),
              SuccessR = define_state(
                utility = discount(u_successR, .015),
                cost = 0
              ),
              Death = define_state(
                utility = 0,
                cost = 0
              )
            )
            
            thr <- run_model(
              standard = mod_standard,
              np1 = mod_np1,
              parameters = param,
              cycles = 61,
              cost = cost,
              effect = utility,
              method = "end",
              init = c(1, 0, 0, 0, 0)
            )
            
            briggs_std_counts <- 
              matrix(c(
                0.98000000, 	 0.0000000,	   0.0000000,	   0.0200000,
                0.97265720, 	 0.0007768,	   0.0000000,	   0.02656600, 
                0.96516548, 	 0.00097491, 	 0.00075606, 	 0.03310354, 
                0.95757109, 	 0.00115802, 	 0.00166964, 	 0.03960125, 
                0.93783561, 	 0.00132115, 	 0.00268314, 	 0.05816010, 
                0.91838963, 	 0.00145307, 	 0.00379326, 	 0.07636404, 
                0.89924305, 	 0.00157339, 	 0.00496429, 	 0.09421927, 
                0.88040225, 	 0.00168398, 	 0.00618146, 	 0.11173231 
              ),
              nc = 4, byrow = TRUE,
              dimnames = list(1:8, c("SuccessP",	"RevisionTHR",	"SuccessR", "Death")))
            expect_equal(round(get_counts(thr$eval_strategy_list$standard)[2:9, -1],
                               8),
                         briggs_std_counts)
            
            briggs_new_counts <- 
              matrix(c(
                0.98000000, 	 0.00000000,	0.00000000,	   0.02000000, 
                0.97323145, 	 0.00020255, 	0.00000000,	   0.02656600, 
                0.96645641, 	 0.00025438, 	 0.00019715, 	 0.03309206, 
                0.95968665, 	 0.00030239, 	 0.00043553, 	 0.03957543, 
                0.94083683, 	 0.00034529, 	 0.00070021, 	 0.05811767, 
                0.92232657, 	 0.00038012, 	 0.00099040, 	 0.07630291, 
                0.90415327, 	 0.00041201, 	 0.00129686, 	 0.09413786, 
                0.88631355, 	 0.00044144, 	 0.00161577, 	 0.11162924 
              ), nc = 4, byrow = TRUE,
              dimnames = list(1:8, c("SuccessP",	"RevisionTHR",	"SuccessR", "Death")))
            expect_equal(round(get_counts(thr$eval_strategy_list$np1)[2:9, -1],
                               8),
                         briggs_new_counts)
            
            
            vals_briggs_std <- 
              matrix(c(
                0,    	    	0.82068966,
                3.66001356,	0.80272917,
                4.33343863, 0.78537550,
                4.85597577,	0.76838428,
                5.22645232,	0.74220815,
                5.42296428,	0.71692065,
                5.53962343,	0.69248578,
                5.59337469,	0.66887542
              ),
              ncol = 2, byrow = TRUE,
              dimnames = list(2:9, c("cost", "utility")))
            
            vals_briggs_std <- data.frame(vals_briggs_std)
            row.names(vals_briggs_std) <- 2:9
            vals <- get_values(thr$eval_strategy_list$standard)[2:9, c("cost", "utility")]
            expect_equal(vals, as.data.frame(vals_briggs_std))
            
            briggs_thr <- 
              list(std_cost = 512.434658,
                   std_qaly = 14.6531896,
                   new_cost = 610.311818,
                   new_qaly =  14.69770986
              )
            briggs_thr_icer <- 2198.486665
            zz <- get_values(thr$eval_strategy_list$standard)
            expect_equal(colSums(zz)[c("cost", "utility")],
                         c(cost = briggs_thr$std_cost, 
                           utility = briggs_thr$std_qaly)
            )
            zz <- get_values(thr$eval_strategy_list$np1)
            expect_equal(colSums(zz)[c("cost", "utility")],
                         c(cost = briggs_thr$new_cost, 
                           utility = briggs_thr$new_qaly)
            )
            expect_equal(summary(thr)$res_comp[2, ".icer"],
                         briggs_thr_icer
            )
            
          }
)

