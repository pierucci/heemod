context("calculation of weighted adverse event costs")

ae_df <- data.frame(treatment = rep(c("T1", "T2", "T3"), each = 3),
                    ae = rep(c("AE1", "AE2", "AE3"), 3),
                    prob = c(0.25, 0, 0.5, 0.2, 0.2, 0.2, 0, 0, 0),
                    cost = rep(c(1000, 500, 200), 3))
empty_df <- data.frame(treatment = character(0),
                       ae = character(0),
                       prob = numeric(0),
                       cost = numeric(0)
                       )


test_that("calculation of adverse event costs works",
          {
            expect_equal(weighted_ae_costs_all(ae_df),
                         data.frame(treatment = c("T1", "T2", "T3"),
                                    weighted_cost = c(350, 340, 0)))
            
          }
)
test_that("catching input errors",
          {
            expect_error(weighted_ae_costs_all(as.matrix(ae_df)),
                         "ae_df must be a data.frame")
            expect_error(weighted_ae_costs_all(empty_df),
                         "ae_df should not be empty")
            expect_error(weighted_ae_costs_all(ae_df,
                                               treatment_col = "Treatment"),
                         "unknown variable to group by")
            expect_error(weighted_ae_costs(ae_df, "T1",
                                           treatment_col = "Treatment"),
                         "treatment_col must name a column of ae_df")
            expect_error(weighted_ae_costs_all(ae_df,
                                               prob_col = "not_prob"),
                         "prob_col and cost_col must name columns of ae_df")
            expect_error(weighted_ae_costs_all(ae_df,
                                               cost_col = "not_cost"),
                         "prob_col and cost_col must name columns of ae_df")
          }
          )          
          