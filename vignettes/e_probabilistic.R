## ---- echo=FALSE, include=FALSE------------------------------------------
library(heemod)

## ------------------------------------------------------------------------
param <- define_parameters(
  rr = .509,
  
  p_AA_mono = .721,
  p_AB_mono = .202,
  p_AC_mono = .067,
  p_AD_mono = .010,
  
  p_BC_mono = .407,
  p_BD_mono = .012,
  
  p_CD_mono = .250,
  
  
  p_AB_comb = p_AB_mono * rr,
  p_AC_comb = p_AC_mono * rr,
  p_AD_comb = p_AD_mono * rr,
  
  p_BC_comb = p_BC_mono * rr,
  p_BD_comb = p_BD_mono * rr,
  
  p_CD_comb = p_CD_mono * rr,
  
  p_AA_comb = 1 - (p_AB_comb + p_AC_comb + p_AD_comb),
  
  
  cost_zido = 2278,
  cost_lami = 2086,
  
  cost_A = 2756,
  cost_B = 3052,
  cost_C = 9007
)

## ------------------------------------------------------------------------
mat_trans_mono <- define_transition(
  p_AA_mono, p_AB_mono, p_AC_mono, p_AD_mono,
  0,         C,         p_BC_mono, p_BD_mono,
  0,         0,         C,         p_CD_mono,
  0,         0,         0,         1
)
mat_trans_comb <- define_transition(
  p_AA_comb, p_AB_comb, p_AC_comb, p_AD_comb,
  0,         C,         p_BC_comb, p_BD_comb,
  0,         0,         C,         p_CD_comb,
  0,         0,         0,         1
)

## ------------------------------------------------------------------------
state_A <- define_state(
    cost_health = 2756,
    cost_drugs = dispatch_strategy(
      mono = cost_zido,
      comb = cost_zido + cost_lami
    ),
    cost_total = discount(cost_health + cost_drugs, .06),
    life_year = 1
  )
state_B <- define_state(
    cost_health = 3052,
    cost_drugs = dispatch_strategy(
      mono = cost_zido,
      comb = cost_zido + cost_lami
    ),
    cost_total = discount(cost_health + cost_drugs, .06),
    life_year = 1
  )
state_C <- define_state(
    cost_health = 9007,
    cost_drugs = dispatch_strategy(
      mono = cost_zido,
      comb = cost_zido + cost_lami
    ),
    cost_total = discount(cost_health + cost_drugs, .06),
    life_year = 1
  )
state_D <- define_state(
    cost_health = 0,
    cost_drugs = 0,
    cost_total = discount(cost_health + cost_drugs, .06),
    life_year = 0
  )

## ------------------------------------------------------------------------
strat_mono <- define_strategy(
  transition = mat_trans_mono,
  state_A,
  state_B,
  state_C,
  state_D
)

strat_comb <- define_strategy(
  transition = mat_trans_comb,
  state_A,
  state_B,
  state_C,
  state_D
)

res_mod <- run_model(
  mono = strat_mono,
  comb = strat_comb,
  parameters = param,
  cycles = 50,
  cost = cost_total,
  effect = life_year
)

## ------------------------------------------------------------------------
rsp <- define_psa(
  rr ~ lognormal(mean = .509, sdlog = .173),
  
  cost_A ~ gamma(mean = 2756, sd = sqrt(2756)),
  cost_B ~ gamma(mean = 3052, sd = sqrt(3052)),
  cost_C ~ gamma(mean = 9007, sd = sqrt(9007)),
  
  p_CD_mono ~ binomial(prob = .25, size = 40),
  
  p_AA_mono + p_AB_mono + p_AC_mono + p_AD_mono ~ multinomial(721, 202, 67, 10)
)

## ------------------------------------------------------------------------
pm <- run_psa(
  model = res_mod,
  psa = rsp,
  N = 100
)

## ------------------------------------------------------------------------
summary(pm, 
        threshold = c(1000, 5000, 6000, 1e4))

## ---- fig.width = 6, fig.height=4, fig.align='center'--------------------
plot(pm, type = "ce")

## ---- fig.width = 6, fig.align='center'----------------------------------
plot(pm, type = "ac", max_wtp = 10000, log_scale = FALSE)
plot(pm, type = "evpi", max_wtp = 10000, log_scale = FALSE)

## ---- fig.width = 6, fig.height = 4, fig.align='center'------------------
plot(pm, type = "cov")

## ---- fig.width = 4, fig.height = 4, fig.align='center'------------------
plot(pm, type = "cov", diff = TRUE, threshold = 5000)

## ---- fig.align='center', fig.height=4, fig.width=6, message=FALSE-------
library(ggplot2)

plot(pm, type = "ce") +
  xlab("Life-years gained") +
  ylab("Additional cost") +
  scale_color_brewer(
    name = "Strategy",
    palette = "Set1"
  ) +
  theme_minimal()

