context("Starting values")

par1 <- define_parameters(
  a = .1,
  b = 1 / (markov_cycle + 1)
)

mat1 <- define_transition(
  state_names = c("X1", "X2"),
  1-a, a,
  1-b, b
)
s1 <- define_state(
  x = 234,
  y = 123
)
s2 <- define_state(
  x = 987,
  y = 1726
)

mod1 <- define_strategy(
  transition = mat1,
  X1 = s1,
  X2 = s2
)

mod2 <- define_strategy(
  transition = mat1,
  X1 = s1,
  X2 = s2,
  starting_values = define_starting_values(
    x = 10,
    y = 20
  )
)

test_that("define_strategy works as expected without starting_values", {
  expect_equal(class(mod1), "uneval_model")
  expect_equal(names(mod1), c("transition", "states", "starting_values"))
  sv <- mod1$starting_values
  expect_equal(class(sv), c("starting_values", "uneval_starting_values", "lazy_dots"))
  expect_equal(sv$x$expr, 0)
  expect_equal(sv$y$expr, 0)
})

test_that("define_strategy works as expected with starting_values", {
  sv <- mod2$starting_values
  expect_equal(class(sv), c("starting_values", "uneval_starting_values", "lazy_dots"))
  expect_equal(sv$x$expr, 10)
  expect_equal(sv$y$expr, 20)
})

test_that("starting_values is consistant", {
  ru <- run_model(mod1, mod2, parameters = par1, cost = x, effect = y)
  val1 <- ru$eval_strategy_list[[1]]$values
  val2 <- ru$eval_strategy_list[[2]]$values
  expect_equal(val2$x - val1$x, 10 * 1000)
  expect_equal(val2$y - val1$y, 20 * 1000)
  
})
