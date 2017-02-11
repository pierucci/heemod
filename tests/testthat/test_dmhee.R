context("DMHEE reproduction")

test_that("HIV model works", {
  
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
  
  res_mod <- run_model(
    mono = mod_mono,
    comb = mod_comb,
    parameters = par_mod,
    cycles = 20,
    cost = cost_total,
    effect = life_year,
    method = "beginning",
    init = c(1, 0, 0, 0)
  )
  x <- summary(res_mod)
  
  expect_equal(
    round(x$res_comp$.icer[2]),
    6276
  )
  
})

test_that("HRT model works", {
  
  # a function to return age-related mortality rate
  # given age and sex
  death_prob <- data.frame(
    age = rep(seq(35, 85, 10), each = 2),
    sex = rep(1:0, 6),
    value = c(
      1.51e-3, .99e-3, 3.93e-3,
      2.6e-3, 10.9e-3, 6.7e-3,
      31.6e-3, 19.3e-3, 80.1e-3,
      53.5e-3, 187.9e-3, 154.8e-3
    )
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
    cons = -5.49094,
    ageC = -.0367,
    maleC = .768536,
    lambda = exp(cons + ageC * age_init + maleC * sex),
    gamma = 1.45367786,
    
    rrNP1 = .260677,
    
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
  
  res_mod <- run_model(
    standard = mod_standard,
    np1 = mod_np1,
    parameters = param,
    cycles = 60,
    cost = cost,
    effect = utility,
    method = "end",
    init = c(1, 0, 0, 0, 0)
  )
  x <- summary(res_mod)
  
  expect_equal(
    round(x$res_comp$.icer[2]),
    2198
  )
})
