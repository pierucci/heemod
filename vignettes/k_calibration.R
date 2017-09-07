## ---- echo=FALSE, include=FALSE------------------------------------------
library(heemod)

## ---- define, include = FALSE--------------------------------------------
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
  method = "end"
)

## ----get_counts, message=FALSE-------------------------------------------
library(dplyr)
get_counts(res_mod) %>% 
  dplyr::filter(markov_cycle == 20 & state_names == "RevisionTHR")

## ----extract_values------------------------------------------------------
extract_values <- function(x) {
  dplyr::filter(
    get_counts(x),
    markov_cycle == 20 & state_names == "RevisionTHR"
  )$count
}
extract_values(res_mod)

## ----define_calib_fn-----------------------------------------------------
calib_fn <- define_calibration_fn(
  type = "count",
  strategy_names = c("standard", "np1"),
  element_names = c("RevisionTHR", "RevisionTHR"),
  cycles = c(20, 20)
)
calib_fn(res_mod)

## ----calibrate_no_init---------------------------------------------------
res_cal <- calibrate_model(
  res_mod,
  parameter_names = c("gamma", "rrNP1"),
  fn_values = extract_values,
  target_values = c(2.5, 0.8)
)
res_cal

## ----calibrate_init, eval = FALSE----------------------------------------
#  start <- data.frame(
#    gamma = c(1.0, 1.5, 2.0),
#    rrNP1 = c(0.2, 0.3, 0.4)
#  )
#  
#  res_cal_2 <- calibrate_model(
#    res_mod,
#    parameter_names = c("gamma", "rrNP1"),
#    fn_values = extract_values,
#    target_values = c(3, 1),
#    initial_values = start,
#    lower = c(0, 0), upper = c(2, 1)
#  )

