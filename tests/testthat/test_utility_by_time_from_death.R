context("test assigning utilities by number of cycles until death")

aa1 <- data.frame(until_lag = c(1,2,3), util = c(0.1, 0.3, 0.5))
aa2 <- aa1
aa2$util <- 0
aa3 <- aa2
aa3$util <- 1
aa4 <- aa1
aa4$util <- c(0.1, 0.1, 0.1)
aa6 <- aa5 <- aa4
aa5$util <- c(1, 0, 0)
aa6$util <- c(0, 1, 0)

ProgFree <- round(1000 * exp(-0.2 * 0:24))
Progressive <- round((1000 - ProgFree) * exp(-0.1 * 0:24))
Death <- 1000 - ProgFree - Progressive
state_names <- rep(c("ProgFree", "Progressive", "Death"), each = 25)

counts <- data.frame(.strategy = rep("s1", 25),
                     markov_cycle = 0:24,
                     state_names = state_names,
                     count = c(ProgFree, Progressive, Death)
                     )
class(counts) <- c("cycle_counts", class(counts))

no_util <- utility_by_time_from_death(counts, aa2, 
                                      util_long_before_death = 0, 
                                      death_state = "Death")
all_same_util <- utility_by_time_from_death(counts, aa3, 
                                            util_long_before_death = 1, 
                                            death_state = "Death")
no_util_near_death <- utility_by_time_from_death(counts, aa2, 
                                      util_long_before_death = 1, 
                                      death_state = "Death")


only_just_before_death_util <- utility_by_time_from_death(counts, aa4, 
                                            util_long_before_death = 0, 
                                            death_state = "Death")

lag_1_util <- utility_by_time_from_death(counts, aa5, 
                                         util_long_before_death = 0, 
                                         death_state = "Death")
lag_2_util <- utility_by_time_from_death(counts, aa6, 
                                         util_long_before_death = 0, 
                                         death_state = "Death")

lag_1_discounted_util <- utility_by_time_from_death(counts, aa5, 
                                         util_long_before_death = 0, 
                                         discount_rate = 0.03,
                                         death_state = "Death")


thr_result <- run_model_tabular(
  location = system.file("tabular/thr", package = "heemod"),
  save = FALSE, overwrite = FALSE, run_demo = FALSE, run_psa = FALSE)

thr_utils <- utility_by_time_from_death(thr_result$model_runs, 
                                        m = "new",  
                                        util_before_death = aa1, 
                                        util_long_before_death = 1)

death_counts <- dplyr::filter(counts, state_names == "Death")$count
not_death_counts <- 
  dplyr::filter(counts, state_names != "Death") %>%
    dplyr::group_by(., markov_cycle) %>%
      dplyr::summarize(alive_counts = sum(count))

test_that("correct results",
          {
            expect_equal(no_util, rep(0, 25))
            expect_equal(lag_1_util[1:10], diff(death_counts)[1:10])
            expect_equal(lag_1_discounted_util[1:10],
                         discount(diff(death_counts), 0.03, first = TRUE)[1:10])
            expect_equal(lag_2_util[1:10], diff(death_counts)[2:11])

            expect_equal(all_same_util, 
                         not_death_counts$alive_counts)
             expect_equal(no_util_near_death,
                          c(883, 818, 751, 685, 621, 561, 504, 453, 
                            407, 365, 326, 293, 262, 235, 210, 188, 
                            168, 151, 136, 121, 109, 98, 98, 98, 98))
             
            expect_equal(only_just_before_death_util,
                        c(11.7, 16.5, 18.9, 19.8, 19.7, 19, 18.1, 16.8, 15.4,
                          13.9, 12.7, 11.4, 10.3, 9.1, 8.3, 7.4, 6.7, 5.9, 5.2,
                          4.7, 4.2, 3.8, 2.3, 1.1, 0.0))
            expect_equal(thr_utils[1:10],
                         c(974.140771, 966.280673, 953.751066, 939.000721, 
                           922.089410, 904.285458, 886.824475, 869.699900, 
                           852.905282, 836.434288))
          }
)

test_that("catch errors in input",
          {
            names(aa1) <- c("cycle", "util")
            expect_error(utility_by_time_from_death(counts, aa1, 
                                                    util_long_before_death = 0, 
                                                    death_state = "Death"),
                         "util_before_death must have columns until_lag and util")
            names(aa1) <- c("until_lag", "utility")
            expect_error(utility_by_time_from_death(counts, aa1, 
                                                    util_long_before_death = 0, 
                                                    death_state = "Death"),
                         "util_before_death must have columns until_lag and util")
            expect_error(utility_by_time_from_death(counts, aa2, 
                                                    util_long_before_death = 0, 
                                                    death_state = "random"),
                         "death_state must be the name of one of the states")
            expect_error(utility_by_time_from_death(counts, aa1, 
                                                    util_long_before_death = 0, 
                                                    death_state = 12),
                         "is.character(death_state) is not TRUE", fixed = TRUE)
            expect_error(utility_by_time_from_death(as.data.frame(counts),
                                                    aa2, util_long_before_death = 0,
                                                    death_state = "Death"),
                         "no applicable method")
            counts[1, "count"] <- counts[1, "count"] + 1
            expect_error(utility_by_time_from_death(counts, aa2, 
                                       util_long_before_death = 0, 
                                       death_state = "Death"),
                         "distinct_num_subjects == 1 is not TRUE")
            }
          
          )


