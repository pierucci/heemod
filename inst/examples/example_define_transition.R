
# simple 3x3 transition matrix

mat_1 <- define_transition(
  .2, 0, .8,
  0, .1, .9,
  0, 0, 1
)
mat_1

plot(mat_1)

# referencing parameters
# rr must be present in a parameter object
# that must later be linked with define_strategy

define_transition(
  .5 - rr, rr,
  .4, .6
)

# can also use C

define_transition(
  C, rr,
  .4, .6
)

# updating cells from mat_1

modify(
  mat_1,
  cell_2_1 = .2,
  cell_2_3 = .7
)

# only matrix size is check, it is thus possible
# to define an incorrect matrix

# this matrix will generate an error later,
# during model evaluation

define_transition(
  .5, 3,
  -1, 2
)
