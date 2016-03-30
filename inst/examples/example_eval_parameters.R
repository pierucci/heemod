param <- define_parameters(
  age_start = 60,
  age = age_start + markov_cycle
)

heemod:::eval_parameters(param, cycles = 15)
