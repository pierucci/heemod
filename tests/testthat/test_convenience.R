context("test convenience functions")

test_that("functions for AE values work",
          {
AEs <- data.frame(treatment = c("A", "A", "B", "B"),
                  ae = c("ae1", "ae2", "ae1", "ae2"),
                  prob = c(0.1, 0.1, 0.2, 0),
                  cost = c(100, 200, 100, 200),
                  disutility = c(0, .1, 0, 0.1),
                  stringsAsFactors = FALSE)

res1 <- compute_vals_for_adv_ev(AEs)
correct <- tibble::tribble(
  ~treatment, ~cost, ~disutility,
  "A", 30, 1/100,
  "B", 20, 0
)
expect_equivalent(as.data.frame(res1), 
                  as.data.frame(correct)
)

AEs2 <- AEs
AEs2$prob <- c(0.1, 0.1, 0.2, NA)

expect_warning(res2 <- compute_vals_for_adv_ev(AEs2),
               "some AE probabilities are missing in the table"
)
expect_identical(res1, res2)

AEs3 <- AEs
AEs3$cost <- c(1, 2, 3, 4)

expect_error(compute_vals_for_adv_ev(AEs3),
             "multiple values defined")

AEs4 <- AEs
names(AEs4) <- c("treatment", "AE", "prob", "cost")
expect_error(compute_vals_for_adv_ev(AEs4),
             "adverse events table must have columns")

expect_equal(ae_val(AEs, "A", "cost"), 30)
expect_equal(ae_val(AEs, "B", "cost"), 20)
expect_equal(ae_val(AEs, "A", "disutility"), 0.01)
expect_equal(ae_val(AEs, "B", "disutility"), 0)
expect_error(ae_val(AEs, "C", "cost"),
             "no AE information returned")
expect_error(ae_val(AEs, "A", "novalue"),
             "no column")
  }
)

test_that("is_dosing_period works",
  {
    expect_identical(is_dosing_period(N = 1:13, first = 4, 
                                      then_every = 3, cap = 40),
                     c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE,
                       TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)
    )
    expect_identical(is_dosing_period(N = 37:46, first = 4, 
                                      then_every = 3, cap = 40),
                     c(TRUE, FALSE, FALSE, TRUE, FALSE, 
                       FALSE, FALSE, FALSE, FALSE, FALSE)
    )
    expect_identical(is_dosing_period(N = 1:8, first = 4, 
                                      pattern = 0, cap = 40),
                     rep(c(TRUE, FALSE), each = 4)
    )
    expect_error(is_dosing_period(N = 1:8, first = 4, 
                                  then_every = -1, cap = 40),
                 "then_every cannot be negative"
    )
    ## demonstrating argument precedence rules
    expect_identical(is_dosing_period(N = 1:10, init = c(1,0,1), 
                                      first = 3, then_every = 5),
                     c(TRUE, FALSE, TRUE, FALSE, FALSE, 
                       FALSE, FALSE, TRUE, FALSE, FALSE)
    )
    expect_identical(is_dosing_period(N = 1:10, init = numeric(0), 
                     pattern = c(1, 1, 0, 1, 0), then_every = 2),
                     c(TRUE, TRUE, FALSE, TRUE, FALSE, 
                       TRUE, TRUE, FALSE, TRUE, FALSE)
    )
    }
)
