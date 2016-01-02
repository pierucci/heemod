
# parameter 'age' depends on time:
# simulating a cohort starting at 60 yo

define_parameters(
  age_start = 60,
  age = age_start + markov_cycle
)

# other uses of markov_cycle are possible

define_parameters(
  top_time = ifelse(markov_cycle < 10, 1, 0)
)

# more elaborate: risk function

define_parameters(
  rate = 1 - exp(- markov_time * .5)
)

# dont explicitely state lengths
# define_parameters(
#   var = seq(1, 15, 2)
# )


# instead rely on markov_cycle or dplyr 
# functions such as n() or row_number()

define_parameters(
  var = seq(from = 1, length.out = n(), by = 3),
  var2 = seq(1, length(markov_cycle), 2)
)

param <- define_parameters(
  age_start = 60,
  age = age_start + markov_cycle
)

# modify existing parameters

modify(
  param,
  age_start = 40
)

# cannot add new parameters

# modify(
#   param,
#   const = 4.4,
#   age_2 = age ^ 2
# )

