mc <- define_correlation(
  age_init, cost_init, .4
)

define_resample(
    age_init = r_norm(60, 10),
    cost_init = r_norm(1000, 100),
    correlation = mc
)

# example with multinomial parameters

define_resample(
  rate1 = r_multinom(10, 100),
  rate2 = r_multinom(50, 100),
  rate3 = r_multinom(40, 100),
  
  a = r_multinom(15, 45),
  b = r_multinom(30, 45),
  
  define_multinom(rate1, rate2, rate3),
  define_multinom(a, b)
)
