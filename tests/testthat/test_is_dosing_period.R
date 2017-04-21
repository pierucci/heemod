context("check that dosing periods are correctly identified")

test_that("errors on incorrect input",
          {
            expect_error(is_dosing_period(N = 1:13, init = 4, pattern = 3),
                         "all elements of init and pattern must be FALSE or 0 or TRUE or 1")
            expect_error(is_dosing_period(N = 1:13, init = c(1,1,1,1), first = 4),
                         "must specify either pattern or then_every") 
            expect_error(is_dosing_period(N = 1:13, pattern = c(0, 1), then_every = 2),
                         "must specify either init or first")
            expect_error(is_dosing_period(N = 1:10, first = -2, then_every = 3),
                         "first must be 0 or positive")
          })

test_that("correct answers on correct input",
          {expect_equal(is_dosing_period(N = 1:13, first = 4, then_every = 3, cap = 40),
                    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, 
                      TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE))
            
           expect_equal(is_dosing_period(N = 37:46, first = 4, then_every = 3, cap = 40),
                         c(TRUE, FALSE, FALSE, TRUE, FALSE, 
                           FALSE, FALSE, FALSE, FALSE, FALSE))
            
             expect_equal(is_dosing_period(N = 1:20, init = c(1,0,1,0,1,0,1,1), pattern = c(1, 0, 1, 1, 0)),
                          c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE))
             expect_equal(is_dosing_period(N = 1:10, first = 4, then_every = -1, cap = 40),
                          c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
             
             ## demonstrating argument precedence rules
             ## note position 2 in the first example
             expect_equal(is_dosing_period(N = 1:10, init = c(1,0,1), first = 3, then_every = 5),
                          c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
             ## note position 1
             expect_equal(is_dosing_period(N = 1:10, init = numeric(0), pattern = c(1, 1, 0, 1, 0), then_every = 3),
                          c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE))
})