mc <- define_correlation(
  age_init, cost_init, .4
)

define_resample(
    age_init ~ norm(60, 10),
    cost_init ~ norm(1000, 100),
    correlation = mc
)

# example with multinomial parameters

define_resample(
  rate1 + rate2 + rate3 ~ multinom(10, 50, 40),
  a + b ~ multinom(15, 30)
)
