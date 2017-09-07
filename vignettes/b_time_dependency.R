## ---- echo=FALSE, include=FALSE------------------------------------------
library(heemod)

## ------------------------------------------------------------------------
define_parameters(
  mr = exp(- state_time * lambda),
  age = 50 + model_time
)

define_state(
  cost = 100 - state_time,
  effect = 10
)

f <- function(x) abs(sin(x))

define_transition(
  C,  f(state_time),
  .1, .9
)

