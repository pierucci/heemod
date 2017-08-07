## ---- echo=FALSE, include=FALSE------------------------------------------
library(heemod)

## ---- include = FALSE----------------------------------------------------

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

## ----include = FALSE-----------------------------------------------------
N <- 100

tab_indiv <- tibble::tibble(
  age = round(rnorm(N, mean = 60, sd = 10)),
  sex = sample(0:1, N, TRUE)
)
tab_indiv_w <- tibble::tibble(
  age = round(rnorm(N, mean = 60, sd = 10)),
  sex = sample(0:1, N, TRUE),
  .weights = runif(N)
)

## ---- fig.align='center', fig.height=4, fig.width=6----------------------
tab_indiv

library(ggplot2)
ggplot(tab_indiv, aes(x = age)) +
  geom_histogram(binwidth = 2)

## ------------------------------------------------------------------------
res_h <- update(res_mod, newdata = tab_indiv)

## ------------------------------------------------------------------------
summary(res_h)

## ---- fig.align='center', fig.height=4, fig.width=6----------------------
plot(res_h, result = "effect", binwidth = 5)
plot(res_h, result = "cost", binwidth = 50)

## ---- fig.align='center', fig.height=4, fig.width=6----------------------
plot(res_h, result = "icer", type = "difference",
     binwidth = 500)
plot(res_h, result = "effect", type = "difference",
     binwidth = .1)
plot(res_h, result = "cost", type = "difference",
     binwidth = 30)

## ---- fig.align='center', fig.height=4, fig.width=6----------------------
plot(res_h, type = "counts")

## ------------------------------------------------------------------------
tab_indiv_w
res_w <- update(res_mod, newdata = tab_indiv_w)
res_w

