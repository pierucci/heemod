context("State Transition Values")

test_that("Without state time expansion", {
  
  par <- define_parameters(
    a_b = 0.3,
    a_c = 0.3
  )
  
  trans <- define_transition(
    C, a_b, a_c,
    C, 0.8, 0.05,
    C, 0.05, 0.9
  )
  
  strat <- define_strategy(
    transition = trans,
    "A" = define_state(
      cost_med = discount(1000, 0.035),
      cost_health = 0,
      ly = discount(1, 0.035)
    ),
    "B" = define_state(
      cost_med = 0,
      cost_health = 0,
      ly = discount(1, 0.035)
    ),
    "C" = define_state(
      cost_med = 0,
      cost_health = 0,
      ly = 0
    ),
    define_state_transition(
      from = "A",
      to = c("B","C"),
      cost_med = 0,
      cost_health = discount(10000, 0.035),
      ly = 0
    )
  )
  
  strat2 <- define_strategy(
    transition = trans,
    "A" = define_state(
      cost_med = discount(1000, 0.035),
      cost_health = 0,
      ly = discount(1, 0.035)
    ),
    "B" = define_state(
      cost_med = 0,
      cost_health = 0,
      ly = discount(1, 0.035)
    ),
    "C" = define_state(
      cost_med = 0,
      cost_health = 0,
      ly = 0
    ),
    define_state_transition(
      from = "A",
      to = NA,
      cost_med = 0,
      cost_health = discount(10000, 0.035),
      ly = 0
    )
  )
  
  strat3 <- define_strategy(
    transition = trans,
    "A" = define_state(
      cost_med = discount(1000, 0.035),
      cost_health = 0,
      ly = discount(1, 0.035)
    ),
    define_state_transition(
      from = "A",
      to = "B",
      cost_med = 0,
      cost_health = discount(10000, 0.035),
      ly = 0
    ),
    define_state_transition(
      from = "A",
      to = "C",
      cost_med = 0,
      cost_health = discount(10000, 0.035),
      ly = 0
    ),
    "B" = define_state(
      cost_med = 0,
      cost_health = 0,
      ly = discount(1, 0.035)
    ),
    "C" = define_state(
      cost_med = 0,
      cost_health = 0,
      ly = 0
    )
  )
  
  strat4 <- define_strategy(
    transition = trans,
    "A" = define_state(
      cost_med = discount(1000, 0.035),
      cost_health = 0,
      ly = discount(1, 0.035)
    ),
    define_state_transition(
      from = NA,
      to = "B",
      cost_med = 0,
      cost_health = discount(10000, 0.035),
      ly = 0
    ),
    "B" = define_state(
      cost_med = 0,
      cost_health = 0,
      ly = discount(1, 0.035)
    ),
    "C" = define_state(
      cost_med = 0,
      cost_health = 0,
      ly = 0
    )
  )
  
  res_mod <- run_model(
    strat = strat,
    strat2 = strat2,
    strat3 = strat3,
    strat4 = strat4,
    parameters = par,
    cycles = 10,
    cost = cost_med + cost_health,
    effect = ly,
    method = "life-table",
    init = c(0.400, 0.300, 0.300),
    state_time_limit = c(A = 3)
  )
  
  expect_equal(res_mod$run_model$.cost, c(10667.15, 10667.15, 10667.15, 8032.17), tolerance=1e-2)
  
})

test_that("With state time expansion", {
  
  par <- define_parameters(
    a_b = ifelse(state_time == 1, 0.2, ifelse(state_time==2, 0.3, 0.4)),
    a_c = ifelse(state_time == 1, 0.2, ifelse(state_time==2, 0.3, 0.4))
  )
  
  trans <- define_transition(
    C, a_b, a_c,
    C, 0.8, 0.05,
    C, 0.05, 0.9
  )
  
  strat <- define_strategy(
    transition = trans,
    "A" = define_state(
      cost_med = discount(1000, 0.035),
      cost_health = 0,
      ly = discount(1, 0.035)
    ),
    "B" = define_state(
      cost_med = 0,
      cost_health = 0,
      ly = discount(1, 0.035)
    ),
    "C" = define_state(
      cost_med = 0,
      cost_health = 0,
      ly = 0
    ),
    define_state_transition(
      from = "A",
      to = c("B","C"),
      cost_med = 0,
      cost_health = discount(10000, 0.035),
      ly = 0
    )
  )
  
  strat2 <- define_strategy(
    transition = trans,
    "A" = define_state(
      cost_med = discount(1000, 0.035),
      cost_health = 0,
      ly = discount(1, 0.035)
    ),
    "B" = define_state(
      cost_med = 0,
      cost_health = 0,
      ly = discount(1, 0.035)
    ),
    "C" = define_state(
      cost_med = 0,
      cost_health = 0,
      ly = 0
    ),
    define_state_transition(
      from = "A",
      to = NA,
      cost_med = 0,
      cost_health = discount(10000, 0.035),
      ly = 0
    )
  )
  
  strat3 <- define_strategy(
    transition = trans,
    define_state_transition(
      from = "A",
      to = "B",
      cost_med = 0,
      cost_health = discount(10000, 0.035),
      ly = 0
    ),
    "A" = define_state(
      cost_med = discount(1000, 0.035),
      cost_health = 0,
      ly = discount(1, 0.035)
    ),
    "B" = define_state(
      cost_med = 0,
      cost_health = 0,
      ly = discount(1, 0.035)
    ),
    "C" = define_state(
      cost_med = 0,
      cost_health = 0,
      ly = 0
    ),
    define_state_transition(
      from = "A",
      to = "C",
      cost_med = 0,
      cost_health = discount(10000, 0.035),
      ly = 0
    )
  )
  
  res_mod <- run_model(
    strat = strat,
    strat2 = strat2,
    strat3 = strat3,
    parameters = par,
    cycles = 10,
    cost = cost_med + cost_health,
    effect = ly,
    method = "life-table",
    init = c(0.400, 0.300, 0.300),
    state_time_limit = c(A = 3)
  )
  
  expect_equal(res_mod$run_model$.cost, c(10510.47, 10510.47, 10510.47), tolerance=1e-2)
  
})

test_that("Error when same state in from/to", {
  expect_error(
    define_state_transition(
      from = "A",
      to = "A",
      cost_med = 0,
      cost_health = discount(10000, 0.035),
      ly = 0
    )
  )
})