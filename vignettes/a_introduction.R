## ---- echo=FALSE, include=FALSE------------------------------------------
library(heemod)

## ------------------------------------------------------------------------
mat_trans <- define_transition(
  .9, .1,
  .2, .8
)
mat_trans

## ------------------------------------------------------------------------
state_A <- define_state(
  cost = 1234,
  utility = 0.85
)
state_A

state_B <- define_state(
  cost = 4321,
  utility = 0.50
)
state_B

## ------------------------------------------------------------------------
strat <- define_strategy(
  transition = mat_trans,
  state_A,
  state_B
)
strat

## ------------------------------------------------------------------------
res_mod <- run_model(
  strat,
  cycles = 10,
  cost = cost,
  effect = utility
)
res_mod

## ---- fig.width = 6, fig.height=4, fig.align='center'--------------------
plot(res_mod)

## ---- fig.align='center', fig.height=4, fig.width=6, message=FALSE-------
library(ggplot2)

plot(res_mod) +
  xlab("Time") +
  ylab("N") +
  theme_minimal() +
  scale_color_brewer(
    name = "State",
    palette = "Set1"
  )

## ---- fig.align='center', fig.height=4, fig.width=6, message=FALSE-------
library(ggplot2)

plot(res_mod, bw = TRUE)

## ------------------------------------------------------------------------
head(get_counts(res_mod))
head(get_values(res_mod))

## ------------------------------------------------------------------------
rate_to_prob(r = 162, per = 1000, to = 5)

