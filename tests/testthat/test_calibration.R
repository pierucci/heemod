context("calibration functions")


test_that("one-dimensional calibration",
          {          
param <- define_parameters(p = 0.8)

mat <- define_transition(
  p, C,
  0, 1
)
mod <- define_strategy(
  transition = mat,
  A = define_state(cost=10, effect = 0.5), 
  B = define_state(cost = 5, effect = 0.8)
)

mod2 <- define_strategy(
  transition = mat,
  A = define_state(cost = 1, effect = 2),
  B = define_state(cost = 2, effect = 1)
)

res_mod <- run_model(
  I = mod,
  II = mod2,
  parameters = param,
  init = c(1000L, 0L),
  cycles = 10,
  cost = cost,
  effect = effect,
  method = "beginning"
)

f <- function(x) {
  dplyr::filter(
    get_counts(x),
    .strategy_names == "I" & state_names == "A" & markov_cycle == 10
  )$count
}
f(res_mod)
set.seed(150)
cal_params <- calibrate_model(
  res_mod,
  parameter_names = "p",
  fn_values = f,
  target_values = 130,
  initial_values = data.frame(p = c(0.5, 0.9)),
  lower = 0, upper = 1
)

expect_equal(f(res_mod), 134.217728)
expect_equal(cal_params,
                 data.frame(p = c(0.79715293, 0.79715293),
                            value = c(0.00042089455, 0.00042089455),
                            convcode = c(NA, NA))
                 )
expect_error(
  cal_params <- calibrate_model(
    res_mod,
    parameter_names = "p",
    fn_values = f,
    target_values = 130,
    initial_values = data.frame(p = c("hello", 0.9)),
    lower = 0, upper = 1
  ),
  "initial parameter values are not numeric"
)

expect_error(
  cal_params <- calibrate_model(
    res_mod,
    parameter_names = "q",
    fn_values = f,
    target_values = 130,
    initial_values = data.frame(p = c(0.5, 0.9)),
    lower = 0, upper = 1
  ),
  "column names of initial values do not match parameter names"
)

expect_error(
  cal_params <- calibrate_model(
    res_mod,
    parameter_names = "q",
    fn_values = f,
    target_values = 130,
    initial_values = data.frame(q = c(0.5, 0.9)),
    lower = 0, upper = 1
  ),
  "Parameter q not present in model parameters."
)
expect_error(
  calibrate_model(
    res_mod,
    parameter_names = "p",
    fn_values = f,
    target_values = 130,
    initial_values = c(p = 0.5, p = 0.9),
    lower = 0, upper = 1
    ),
  "length of initial values is not the same as length of parameter names"
)


  cal_params_2 <- calibrate_model(
    res_mod,
    parameter_names = "p",
    fn_values = f,
    target_values = 130,
    initial_values = data.frame(p = c(0.5, 0.9)),
    lower = 0, upper = 1
  )

expect_identical(cal_params, cal_params_2)

          }
)


test_that("defining calibration function works",
          {
            calib_fn <- define_calibration_fn(
              type = c("count", "value"),
              strategy_names = c("standard", "np1"),
              element_names = c("RevisionTHR", "cost"),
              cycles = c(20, 20)
            )
            expect_identical(evalq(cycles, environment(calib_fn)),
                             c(20, 20))
            expect_identical(evalq(element_names, 
                                   environment(calib_fn)),
                             c("RevisionTHR", "cost"))
            expect_identical(evalq(strategy_names,
                                   environment(calib_fn)),
                             c("standard", "np1"))
            expect_identical(evalq(type,
                                   environment(calib_fn)),
                             c("count", "value"))
          }
          )

test_that("multi-dimensional calibration",
{
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
    mr = get_who_mr(age, sex_cat,
                    country = "GBR", local = TRUE),
    
    # state values
    u_SuccessP = .85,
    u_RevisionTHR = .30,
    u_SuccessR = .75,
    c_RevisionTHR = 5294
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
    0, C, 0,          0, omrPTHR,
    0, C, np1RR,      0, mr,
    0, 0, 0,          C, omrRTHR+mr,
    0, 0, rrr,        C, mr,
    0, 0, 0,          0, 1
  )
  
  mod_standard <- define_strategy(
    transition = mat_standard,
    PrimaryTHR = define_state(
      utility = 0,
      cost = 394
    ),
    SuccessP = define_state(
      utility = discount(u_SuccessP, .015),
      cost = 0
    ),
    RevisionTHR = define_state(
      utility = discount(u_RevisionTHR, .015),
      cost = discount(c_RevisionTHR, .06)
    ),
    SuccessR = define_state(
      utility = discount(u_SuccessR, .015),
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
      utility = discount(u_SuccessP, .015),
      cost = 0
    ),
    RevisionTHR = define_state(
      utility = discount(u_RevisionTHR, .015),
      cost = discount(c_RevisionTHR, .06)
    ),
    SuccessR = define_state(
      utility = discount(u_SuccessR, .015),
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
    method = "beginning"
  )
  extract_values <- function(x) {
    dplyr::filter(
      get_counts(x),
      markov_cycle == 20 & state_names == "RevisionTHR"
    )$count
  }
  expect_warning(
    res_cal <- calibrate_model(
      res_mod,
      parameter_names = c("gamma", "rrNP1"),
      fn_values = extract_values,
      target_values = c(2.5, 0.8),
      method = "L-BFGS-B",
      itnmax = 4, lower = c(0, 0), upper = c(2,1)
    ),
    "Not all optimizations converged")

  expect_equivalent(res_cal,
                   data.frame(gamma = 1.442250332, 
                     rrNP1 = 0.3144441542,
                     value = 1.710576e-10,
                     convcode = 1)
  )
}
)
