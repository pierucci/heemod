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
  y = 123,
  z = 369
)
s2 <- define_state(
  x = 987,
  y = 1726,
  z = 963
)
s3 <- define_state(
  x = 987,
  y = 1726,
  z = 963,
  starting_values = define_starting_values(
    y = 10
  )
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

mod3 = define_strategy(
  transition = mat1,
  X1 = s1,
  X2 = s3
)

mod4 = define_strategy(
  transition = mat1,
  X1 = s1,
  X2 = s3,
  starting_values = define_starting_values(
    x = 10,
    y = 20
  ))

test_that("define_strategy works as expected without starting_values", {
  expect_equal(class(mod1), "uneval_model")
  expect_equal(names(mod1), c("transition", "states", "starting_values"))
  sv <- mod1$starting_values
  expect_equal(class(sv), c("starting_values", "uneval_starting_values", "lazy_dots"))
  expect_equal(sv$x$expr, 0)
  expect_equal(sv$y$expr, 0)
})

test_that("starting_values return errors",{
  expect_error(define_state(
    x = 987,
    y = 1726,
    z = 963,
    starting_values = define_starting_values(
      w = 10
    )
  ))
  expect_error(mod1 <- define_strategy(
    transition = mat1,
    X1 = s1,
    X2 = s2,
    starting_values = define_starting_values(w = 10)
  ))
  expect_error(mod1 <- define_strategy(
    transition = mat1,
    X1 = s1,
    X2 = s2,
    starting_values = define_starting_values(z = 10)
  ), NA)
  
})

test_that("define_strategy works as expected with starting_values", {
  sv <- mod2$starting_values
  expect_equal(class(sv), c("starting_values", "uneval_starting_values", "lazy_dots"))
  expect_equal(sv$x$expr, 10)
  expect_equal(sv$y$expr, 20)
})

test_that("starting_values only adds values at first cycle", {
  ru <- run_model(mod1, mod2, parameters = par1, cost = x, effect = y, cycles = 10)
  val1 <- ru$eval_strategy_list[[1]]$values
  val2 <- ru$eval_strategy_list[[2]]$values
  expect_equal(val2$x - val1$x, c(10 * 1000, rep(0,9)))
  expect_equal(val2$y - val1$y, c(20 * 1000, rep(0,9)))
})


test_that("starting_values is consistant", {
  ru <- run_model(mod1, mod2, parameters = par1, cost = x, effect = y)
  val1 <- ru$eval_strategy_list[[1]]$values
  val2 <- ru$eval_strategy_list[[2]]$values
  expect_equal(val2$x - val1$x, 10 * 1000)
  expect_equal(val2$y - val1$y, 20 * 1000)
})

test_that("starting_values works with parameters", {
  s3 <- define_state(
    x = 987,
    y = 1726,
    z = 963,
    starting_values = define_starting_values(
      y = a * 100
    )
  )
  mod3 = define_strategy(
    transition = mat1,
    X1 = s1,
    X2 = s3
  )
  mod4 = define_strategy(
    transition = mat1,
    X1 = s1,
    X2 = s3,
    starting_values = define_starting_values(
      x = a * 100,
      y = a * 200
    ))
  ru0 <- run_model(mod1, parameters = par1, cost = x, effect = y)
  ru1 <- run_model(mod3, parameters = par1, cost = x, effect = y)
  ru2 <- run_model(mod4, parameters = par1, cost = x, effect = y)
  expect_equal(ru1$eval_strategy_list[[1]]$values, ru0$eval_strategy_list[[1]]$values + c(0,0,50 * 10), 0)
  expect_equal(ru2$eval_strategy_list[[1]]$values, ru0$eval_strategy_list[[1]]$values + c(0, 1000 * 10, 1000* 20) + c(0, 0, 50 * 10))
})


test_that("starting_values uses patient flows", {
  mat2 <- define_transition(
    state_names = c("E1", "E2", "E3"),
    0.5, 0.2, C,
    0.4, 0.5, C,
    1  , 0  , 0
  )
  
  s3 <- define_state(
    x = 0,
    y = 0,
    z = 0
  )
  
  modx <- define_strategy(
    transition = mat2,
    E1 = s1,
    E2 = s2,
    E3 = s3
  )
  
  rux <- run_model(modx, parameters = par1, cost = x, effect = y, cycles = 10)
})

test_that("starting_values works with define_state and define_strategy at the same time", {
  ru0 <- run_model(mod4, mod3, mod1, parameters = par1, cost = x, effect = y, cycles = 5)
  new <- c(50, 95, 88.8333333333333, 87.9083333333333, 88.4825)
  expect_equal(ru0$eval_strategy_list[[1]]$values, ru0$eval_strategy_list[[2]]$values + 
                  data.frame(rep(0,5), c(1000 * 10, rep(0,4)), c(1000 * 20, rep(0,4)), rep(0,5)))
  expect_equal(ru0$eval_strategy_list[[2]]$values$y, ru0$eval_strategy_list[[3]]$values$y + new * 10)
})

test_that("starting_values works with discount_hack", {
  s5 <- define_state(
    x = 987,
    y = 1726,
    z = 963,
    starting_values = define_starting_values(
       y = discount(10, 0.06)
    )
  )
  mod5 = define_strategy(
    transition = mat1,
    X1 = s1,
    X2 = s5
  )
  ru0 <- run_model(mod5, mod1, parameters = par1, cost = x, effect = y, cycles = 5)
  
  expect_equal(ru0$eval_strategy_list[[1]]$states$starting_values$X2$y, 10/(1+0.06)^seq(0,4))
  new <- c(50, 95, 88.8333333333333, 87.9083333333333, 88.4825)
  expect_equal(ru0$eval_strategy_list[[1]]$values$y, 
               ru0$eval_strategy_list[[2]]$values$y + new * 10/(1+0.06)^seq(0,4))
})

test_that("starting_values works with expanded states", {
  par2 <- define_parameters(
    a = 1 / (markov_cycle + 1),
    b = exp(-state_time * 0.2)
  )

  mod3 = define_strategy(
    transition = mat1,
    X1 = s1,
    X2 = s3
  )
  
  ru <- run_model(mod1, mod3, parameters = par2, cost = x, effect = y, cycles = 10)
  val1 <- ru$eval_strategy_list[[1]]$values
  val2 <- ru$eval_strategy_list[[2]]$values
  expect_equal(val2$x, val1$x)
  expect_equal(floor(val2$y - val1$y), c(2500, 3333, 1363, 1013, 961, 942, 898, 838, 775, 719))
})
