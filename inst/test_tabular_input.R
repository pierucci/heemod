context("test tabular input")

## path2files = "./"   # where we'll find test files to use

# test input for state files ----------------------------------------------

## we'll test most of it assuming files have been read,
##   using the states from the total hip replacement module

state_spec_file <- system.file("extdata", "THR_test_states.csv", package = "heemod")
# state_spec_file <- file.path(path2files, "THR_test_states.csv")

state_spec <- data.frame(state = c("PrimaryTHR", "SuccessfulPrimary",
                                   "RevisionTHR", "SuccessfulRevision",
                                   "Death"),
                         cost = c(0,0,5294, 0, 0),
                         qaly = c(0.0, 0.85, 0.30, 0.75, 0.00),
                         .discount.qaly = c(0.01, NA, NA, NA, NA),
                         stringsAsFactors = FALSE)

test_that("proper state file input provides the expected output",
          {
            states1 <- f_create_state_definitions_from_tabular(state_spec)
            
            expect_equal(states1$state_def_string,
                         "PrimaryTHR = define_state(cost=0, qaly=discount(0, 0.01)), SuccessfulPrimary = define_state(cost=0, qaly=discount(0.85, 0.01)), RevisionTHR = define_state(cost=5294, qaly=discount(0.3, 0.01)), SuccessfulRevision = define_state(cost=0, qaly=discount(0.75, 0.01)), Death = define_state(cost=0, qaly=discount(0, 0.01))")
            expect_equal(states1$state_command,
                         "define_state_list(PrimaryTHR = define_state(cost=0, qaly=discount(0, 0.01)), SuccessfulPrimary = define_state(cost=0, qaly=discount(0.85, 0.01)), RevisionTHR = define_state(cost=5294, qaly=discount(0.3, 0.01)), SuccessfulRevision = define_state(cost=0, qaly=discount(0.75, 0.01)), Death = define_state(cost=0, qaly=discount(0, 0.01)))")
            expect_equal(states1$state_names, 
                         c("PrimaryTHR", "SuccessfulPrimary",
                           "RevisionTHR", "SuccessfulRevision", "Death"))

            ## compound test of f_parse_multi_spec and f_define_states_from_tabular
            parsed_spec <- f_parse_multi_spec(state_spec_file, split_on = ".model",
                               group_vars = "state")
            states2 <- f_create_state_definitions_from_tabular(parsed_spec[[1]])
            
            expect_equal(states2$state_def_string,
                         "PrimaryTHR = define_state(cost=0, qaly=discount(0, 0.01)), SuccessfulPrimary = define_state(cost=0, qaly=discount(0.85, 0.01)), RevisionTHR = define_state(cost=5294, qaly=discount(0.3, 0.01)), SuccessfulRevision = define_state(cost=0, qaly=discount(0.75, 0.01)), Death = define_state(cost=0, qaly=discount(0, 0.01))")
            expect_equal(states2$state_command,
                         "define_state_list(PrimaryTHR = define_state(cost=0, qaly=discount(0, 0.01)), SuccessfulPrimary = define_state(cost=0, qaly=discount(0.85, 0.01)), RevisionTHR = define_state(cost=5294, qaly=discount(0.3, 0.01)), SuccessfulRevision = define_state(cost=0, qaly=discount(0.75, 0.01)), Death = define_state(cost=0, qaly=discount(0, 0.01)))")
            expect_equal(states2$state_names, 
                         c("PrimaryTHR", "SuccessfulPrimary",
                           "RevisionTHR", "SuccessfulRevision", "Death"))
          })


test_that("state file discounting checks work properly",
          {
            discount_problem_spec <- state_spec
            discount_problem_spec$.discount.qaly[2] <- 0.05
            expect_error(f_create_state_definitions_from_tabular(discount_problem_spec), "more than one distinct value in .discount.qaly")
            discount_problem_spec <- state_spec
            discount_problem_spec$.discount.qaly[1] <- -0.02
            expect_error(f_create_state_definitions_from_tabular(discount_problem_spec), "negative discount value is invalid: .discount.qaly")
            discount_problem_spec <- state_spec
            discount_problem_spec$.discount.qaly[1] <- NA
            expect_error(f_create_state_definitions_from_tabular(discount_problem_spec), "no valid discount value in .discount.qaly")
          })

test_that("bad state file input is caught",
          {
            expect_error(f_create_state_definitions_from_tabular(NULL),
                         "state_info must be a non-empty data frame")
            expect_error(f_create_state_definitions_from_tabular(1:10),
                         "state_info must be a non-empty data frame")
            expect_error(f_create_state_definitions_from_tabular(data.frame(x = 1:5, y = 1:5)),
                         "should be one of the names of the input")
          })


# check transition probability input --------------------------------------

# we'll test most of this assuming the file has already been read in
# we'll use an example from the total hip replacement model

tm_spec <- data.frame(from = c(rep("PrimaryTHR", 2),
                               rep("SuccessfulPrimary", 3),
                               rep("RevisionTHR", 2),
                               rep("SuccessfulRevision", 3),
                               "Death"),
                      to = c("SuccessfulPrimary", "Death",
                             "SuccessfulPrimary", "RevisionTHR",
                             "Death", "SuccessfulRevision",
                             "Death", "SuccessfulRevision",
                             "RevisionTHR", "Death", "Death"),
                      prob = c("C", 0.02, "C", "pHRFailStandard",
                               "mr", "C", 0.02, "C", 0.04, "mr", 1),
                      stringsAsFactors = FALSE)

state_names <- c("PrimaryTHR", "SuccessfulPrimary", "RevisionTHR",
                 "SuccessfulRevision", "Death")

test_that("proper output on proper transition file input",
          {
            tm <- f_transition_prob_matrix_from_tabular(tm_spec, state_names)
            expect_equal(tm$string, "define_matrix( 0,C,0,0,0.02,0,C,pHRFailStandard,0,mr,0,0,0,C,0.02,0,0,0.04,C,mr,0,0,0,0,1 , state_names= c(\"PrimaryTHR\",\"SuccessfulPrimary\",\"RevisionTHR\",\"SuccessfulRevision\",\"Death\") )")
          })

test_that("improper transition file column names cause error",
          {
            bad_tm_spec <- tm_spec
            names(bad_tm_spec)[1] <- "forr"
            expect_error(f_transition_prob_matrix_from_tabular(bad_tm_spec, state_names))
            bad_tm_spec <- tm_spec
            bad_tm_spec <- bad_tm_spec[-11,]
            expect_error(f_transition_prob_matrix_from_tabular(bad_tm_spec, state_names),
                         "not all specified states have a transition probability out")
            expect_error(f_transition_prob_matrix_from_tabular(tm_spec, NULL))
            expect_error(f_transition_prob_matrix_from_tabular(NULL, state_names))
          })


# check parsing of multi-spec files (multiple models in a single file) --------


# THRmulti_prob_file_1 <- file.path(path2files, "THR_test_transition_probs.csv")
# THRmulti_prob_file_2 <- file.path(path2files, "THR_test_transition_probs_2.csv")
# THRmulti_prob_file_bad <- file.path(path2files, "THR_test_transition_probs_bad.csv")

THRmulti_prob_file_1 <- system.file("extdata", "THR_test_transition_probs.csv", package = "heemod")
THRmulti_prob_file_2 <- system.file("extdata", "THR_test_transition_probs_2.csv", package = "heemod")
THRmulti_prob_file_bad <- system.file("extdata", "THR_test_transition_probs_bad.csv", package = "heemod")


THRmulti_prob <-
  structure(list(.model = c("standard", "standard", "standard", 
                            "standard", "new", "standard", "standard", "standard",
                            "standard", "standard", "standard", "standard"), 
                 from = c("PrimaryTHR", "PrimaryTHR", "SuccessfulPrimary", 
                          "SuccessfulPrimary", "SuccessfulPrimary", 
                          "SuccessfulPrimary", "RevisionTHR", "RevisionTHR", 
                          "SuccessfulRevision", "SuccessfulRevision", 
                          "SuccessfulRevision", "Death"), 
                 to = c("SuccessfulPrimary", "Death", "SuccessfulPrimary", 
                        "RevisionTHR", "RevisionTHR", "Death", "SuccessfulRevision",
                        "Death", "SuccessfulRevision", "RevisionTHR", 
                        "Death", "Death"), 
                 prob = c("C", "0.02", "C", "pHRFailStandard", "pHRFailNew", 
                          "mr", "C", "0.02", "C", "0.04", "mr", "1")), 
            class = "data.frame", row.names = c(NA,-12L), 
            .Names = c(".model", "from", "to", "prob"))


THRmulti_prob_2 <- THRmulti_prob
THRmulti_prob_2[12, ".model"] <- "new"

THRmulti_prob_result <-
  structure(list(standard = 
                   structure(list(.model = c("standard", "standard", "standard",
                                             "standard", "standard", "standard",
                                             "standard", "standard", "standard",
                                             "standard", "standard"), 
                                  from = c("PrimaryTHR", "PrimaryTHR", 
                                           "SuccessfulPrimary", "SuccessfulPrimary",
                                           "SuccessfulPrimary", "RevisionTHR", 
                                           "RevisionTHR", "SuccessfulRevision", 
                                           "SuccessfulRevision", "SuccessfulRevision",
                                           "Death"), 
                                  to = c("SuccessfulPrimary", "Death", 
                                         "SuccessfulPrimary", "Death", 
                                         "RevisionTHR", "Death", "SuccessfulRevision",
                                         "Death", "RevisionTHR", "SuccessfulRevision",
                                         "Death"), 
                                  prob = c("C", "0.02", "C", "mr", "pHRFailStandard", 
                                           "0.02", "C", "mr", "0.04", "C", "1")), 
                             .Names = c(".model", "from", "to", "prob"), 
                             row.names = c(1L, 3L, 5L, 7L, 21L, 11L, 9L, 17L, 15L, 13L, 19L), class = "data.frame"), 
                 new = structure(list(.model = c("new", "new", "new", "new", 
                                                 "new", "new", "new", "new", 
                                                 "new", "new", "new"), 
                                      from = c("PrimaryTHR", "PrimaryTHR", 
                                               "SuccessfulPrimary", "SuccessfulPrimary",
                                               "SuccessfulPrimary", "RevisionTHR",
                                               "RevisionTHR", "SuccessfulRevision",
                                               "SuccessfulRevision", "SuccessfulRevision",
                                               "Death"), 
                                      to = c("SuccessfulPrimary","Death", 
                                             "SuccessfulPrimary", "Death", "RevisionTHR",
                                             "Death", "SuccessfulRevision", "Death", 
                                             "RevisionTHR", "SuccessfulRevision","Death"),
                                      prob = c("C", "0.02", "C", "mr", "pHRFailNew",
                                               "0.02", "C", "mr", "0.04", "C", "1")), 
                                 .Names = c(".model", "from", "to", "prob"), 
                                 row.names = c(2L, 4L, 6L, 8L, 22L, 12L, 10L, 18L, 16L, 14L, 20L), 
                                 class = "data.frame")), 
            .Names = c("standard", "new"))

bad_spec <- data.frame(.model = c("M1", "M2", "M3"), g1 = c(1,1,1), 
                       g2 = c(2,2,1), x = c(0.5, 0.5, 3.0),
                       stringsAsFactors = FALSE)

test_that("f_parse_multi_spec working",
          {
            expect_equal(f_parse_multi_spec(THRmulti_prob, ".model", c("from", "to")),
                         THRmulti_prob_result)
            expect_equal(f_parse_multi_spec(THRmulti_prob_2, ".model", c("from", "to")),
                         THRmulti_prob_result)
            expect_equal(f_parse_multi_spec(THRmulti_prob_file_1, ".model", c("from", "to")),
                         THRmulti_prob_result)
            expect_equal(f_parse_multi_spec(THRmulti_prob_file_2, ".model", c("from", "to")),
                         THRmulti_prob_result)
            ## compound test - error comes from the input to multi_spec,
            ##    gets caught in f_transition_prob_matrix_from_tabular
            expect_error(f_transition_prob_matrix_from_tabular(
              trans_probs = f_parse_multi_spec(THRmulti_prob_file_bad, 
                         ".model", c("from", "to"))[[1]],
              state_names = state_names),
              "not all specified states have a transition probability out")
            expect_error(f_parse_multi_spec(THRmulti_prob, ".model", c("from", "too")),
                         "split_on and group_vars must be column names of the input multi_spec")
            expect_error(f_parse_multi_spec(THRmulti_prob, ".modell", c("from", "to")),
                         "split_on and group_vars must be column names of the input multi_spec")
            ## this will print a couple of explanatory lines to the output
            ##   as well as throwing the error
            expect_error(f_parse_multi_spec(bad_spec, ".model", c("g1", "g2")),
                        "group_var combinations must be specified either once for all splits or once for each split")

            expect_error(f_parse_multi_spec(bad_spec, c(".model", "g1") , "g2"),
                          "split_on must have length exactly 1")
             expect_error(f_parse_multi_spec(bad_spec, character(0), c("g1", "g2")),
                          "split_on must have length exactly 1")
          })


# test ignoring of comment columns ----------------------------------------


tCSV <- f_read_file(system.file("extdata", "testing_CSV_file_with_comment_col.csv", package = "heemod"))
tXLS <- f_read_file(system.file("extdata", "testing_XLS_file_with_comment_col.xls", package = "heemod"))
tXLSX <- f_read_file(system.file("extdata", "testing_XLSX_file_with_comment_col.xlsx", package = "heemod"))

# xls_withcomment= paste(path2files, "testing_XLS_file_with_comment_col.xls", sep="")
# tXLS <- f_read_file(xls_withcomment)
# 
# xlsx_withcomment= paste(path2files, "testing_XLSX_file_with_comment_col.xlsx", sep="")
# tXLSX <- f_read_file(xlsx_withcomment)

test_that("Columns that contain 'comment' in their header are ignored",
          {expect_that(names(tCSV),
                       equals(c(".model", "state", "cost", "qaly", ".discount.cost", ".discount.qaly", "valid.column")))
            expect_that(names(tXLS),
                        equals(c(".model", "state", "cost", "qaly", ".discount.cost", ".discount.qaly", "valid.column")))
            expect_that(names(tXLSX),
                        equals(c(".model", "state", "cost", "qaly", ".discount.cost", ".discount.qaly", "valid.column"))
            )
          })



