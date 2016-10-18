mc <- define_correlation(
  age_init, cost_init, .4
)

define_psa(
    age_init ~ normal(60, 10),
    cost_init ~ normal(1000, 100),
    correlation = mc
)

# example with multinomial parameters

define_psa(
  rate1 + rate2 + rate3 ~ multinom(10, 50, 40),
  a + b ~ multinom(15, 30)
)
