context("test that we can find lowest-cost combinations of units")

## sample data
units1 <- data.frame(size = c(7, 3, 1), cost = c(7, 3, 1))

units2 <- data.frame(size = c(1000, 250, 200, 100, 50), cost = c(40, 11.5, 8.5, 5.5, 4.4))
units2 <- rbind(units2, units2[-1,])
units2$selector <- rep(c("A", "B"), c(5, 4))

test_that("errors when mis-specifying selectors",
          {
          expect_error(find_least_cost_partition(total = 10,
                                                 available_units = units1,
                                                 max_num_units = 3,
                                                 max_excess = 0,
                                                 subset_col = "selector"),
                       "subset_col and subset_val should either both be NULL, or both be set")
          expect_error(find_least_cost_partition(total = 10,
                                                 available_units = units1,
                                                 max_num_units = 3,
                                                 max_excess = 0,
                                                 subset_val = "A"),
                       "subset_col and subset_val should either both be NULL, or both be set")
          expect_error(find_least_cost_partition(total = 10,
                                                 available_units = units1,
                                                 max_num_units = 3,
                                                 max_excess = 0,
                                                 subset_col = "selector",
                                                 subset_val = "A"),
                       "selector is not a column of available_units")
          expect_error(find_least_cost_partition(total = 1000,
                                                 available_units = units2,
                                                 max_num_units = 3,
                                                 max_excess = 0,
                                                 subset_col = c("selector", "selector2"),
                                                 subset_val = "A"),
                       "must specify exactly one column to select on")
          
          expect_error(find_least_cost_partition(total = 1000,
                                                 available_units = units2,
                                                 max_num_units = 3,
                                                 max_excess = 0,
                                                 subset_col = "selector",
                                                 subset_val = c("A", "B")),
                       "must specify exactly one value to select")
          expect_error(find_least_cost_partition(total = 1000,
                                                 available_units = units2,
                                                 max_num_units = 3,
                                                 max_excess = 0,
                                                 subset_col = "selector",
                                                 subset_val = "C"),
                        "column selector does not contain the value C")
           expect_error(find_least_cost_partition(total = 1000,
                                                  available_units = units2,
                                                  max_num_units = 8,
                                                  max_excess = 0,
                                                  subset_col = "selector",
                                                  subset_val = "A"),
                        "too many combinations")
          }
)

test_that("correct answers in some simple cases",
          {
          expect_equal(find_least_cost_partition(10, units1, max_num_units = 2, 
                                                 max_excess = 0),
                       list('10' = data.frame(X1 = 3, X2 = 7, cost = 10)))
          expect_equal(find_least_cost_partition(10, units1, 3, max_excess = 2, 
                                                 only_least_cost = FALSE),
                       list('10' = data.frame(X1 = c(7,7), X2 = c(3,3), X3 = c(0, 1), cost = c(10, 11))))
          expect_equal(find_least_cost_partition(1000, units2, 4, max_excess = 0, 
                                                 subset_col = "selector", subset_val = "A"),
                       list('1000' = data.frame(X1 = 1000, cost = 40)))
          expect_equal(find_least_cost_partition(1000, units2, 4, max_excess = 0, 
                                                 subset_col = "selector", subset_val = "B"),
                       list('1000' = data.frame(X1 = 250, X2 = 250, X3 = 250, X4 = 250, cost = 46)))
          expect_equal(find_least_cost_partition(1000, units2, 5, max_excess = 0, 
                                                 subset_col = "selector", subset_val = "B"),
                       list('1000' = data.frame(X1 = 200, X2 = 200, X3 = 200, X4 = 200, X5 = 200, cost = 42.5)))
          }
)

test_that("returns NA answer if there is no solution",
          expect_equal(find_least_cost_partition(15, units1, 2),
                       list('15' = data.frame(cost = as.numeric(NA))))
)
          