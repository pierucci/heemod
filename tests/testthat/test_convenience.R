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

  }
)

