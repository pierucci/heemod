context("Test tabular input")

state_spec_file <- system.file(
  "tabular/test",
  "THR_test_states.csv",
  package = "heemod"
) %>% 
  heemod:::read_file()

state_spec <- data.frame(
  .state = c("PrimaryTHR", "SuccessfulPrimary",
             "RevisionTHR", "SuccessfulRevision",
             "Death"),
  cost = c(0,0,5294, 0, 0),
  qaly = c(0.0, 0.85, 0.30, 0.75, 0.00),
  .discount.qaly = c(0.01, NA, NA, NA, NA),
  stringsAsFactors = FALSE
)

test_that(
  "Normal state file input works.",
  {
    states1 <- heemod:::create_states_from_tabular(state_spec)
    
    expect_output(
      print(states1),
      "A list of 5 states with 2 values each.

State names:

PrimaryTHR
SuccessfulPrimary
RevisionTHR
SuccessfulRevision
Death

State values:

cost
qaly",
      fixed = TRUE
    )
    
    expect_equal(
      heemod:::get_state_names(states1), 
      c("PrimaryTHR", "SuccessfulPrimary",
        "RevisionTHR", "SuccessfulRevision", "Death")
    )
    
    ## compound test of f_parse_multi_spec and f_define_states_from_tabular
    parsed_spec <- heemod:::parse_multi_spec(
      state_spec_file,
      split_on = ".model",
      group_vars = ".state"
    )
    states2 <- heemod:::create_states_from_tabular(parsed_spec[[1]])
    
    expect_output(
      print(states2),
      "A list of 5 states with 2 values each.

State names:

PrimaryTHR
SuccessfulPrimary
RevisionTHR
SuccessfulRevision
Death

State values:

cost
qaly",
      fixed = TRUE
    )
    expect_equal(
      heemod:::get_state_names(states2), 
      c("PrimaryTHR", "SuccessfulPrimary",
        "RevisionTHR", "SuccessfulRevision", "Death")
    )
  }
)

test_that(
  "Discounting checks work.", {
    discount_problem_spec <- state_spec
    discount_problem_spec$.discount.qaly[2] <- 0.05
    expect_error(
      heemod:::create_states_from_tabular(discount_problem_spec),
      "Multiple discount values for '.discount.qaly'.",
      fixed = TRUE
    )
    
    discount_problem_spec <- state_spec
    discount_problem_spec$.discount.qaly[1] <- -0.02
    
    expect_error(
      heemod:::create_states_from_tabular(discount_problem_spec),
      "Discount values out of range [0 - 1].",
      fixed = TRUE
    )
    
    discount_problem_spec <- state_spec
    discount_problem_spec$.discount.qaly[1] <- NA
    expect_error(
      heemod:::create_states_from_tabular(discount_problem_spec),
      "No discount values found for '.discount.qaly'.",
      fixed = TRUE
    )
  }
)

test_that(
  "Bad state file input is caught.", {
    expect_error(
      heemod:::create_states_from_tabular(NULL),
      "'state_info' must be a data frame.",
      fixed = TRUE
    )
    expect_error(
      heemod:::create_states_from_tabular(1:10),
      "'state_info' must be a data frame.",
      fixed = TRUE
    )
    expect_error(
      heemod:::create_states_from_tabular(
        data.frame(x = 1:5, y = 1:5)
      ),
      "'.state' should be a column name.",
      fixed = TRUE
    )
  }
)

tm_spec <- data.frame(
  from = c(rep("PrimaryTHR", 2),
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
  stringsAsFactors = FALSE
)

state_names <- c("PrimaryTHR", "SuccessfulPrimary", "RevisionTHR",
                 "SuccessfulRevision", "Death")

test_that(
  "Proper transition file input works.", {
    tm <- heemod:::create_matrix_from_tabular(tm_spec, state_names)
    
    expect_output(
      print(tm),
      "An unevaluated matrix, 5 states\\.

                   PrimaryTHR SuccessfulPrimary.*
PrimaryTHR                    C                .*
SuccessfulPrimary             C                .*
RevisionTHR                                    .*
SuccessfulRevision                             .*
Death                                          .*"
    )
  }
)

test_that(
  "Improper transition file cause error.", {
    bad_tm_spec <- tm_spec
    names(bad_tm_spec)[1] <- "forr"
    expect_error(
      heemod:::create_matrix_from_tabular(bad_tm_spec, state_names)
    )
    
    bad_tm_spec <- tm_spec
    bad_tm_spec <- bad_tm_spec[-11, ]
    expect_error(
      heemod:::create_matrix_from_tabular(bad_tm_spec, state_names),
      "Some states do not have an exit probability: Death.",
      fixed = TRUE
    )
    expect_error(
      heemod:::create_matrix_from_tabular(tm_spec, NULL)
    )
    expect_error(
      heemod:::create_matrix_from_tabular(NULL, state_names)
    )
  }
)

THRmulti_prob_file_1 <- system.file("tabular/test", "THR_test_transition_probs.csv", package = "heemod")
THRmulti_prob_file_2 <- system.file("tabular/test", "THR_test_transition_probs_2.csv", package = "heemod")
THRmulti_prob_file_bad <- system.file("tabular/test", "THR_test_transition_probs_bad.csv", package = "heemod")


THRmulti_prob <- structure(list(
  .model = c("standard", "standard", "standard", 
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

THRmulti_prob_result <- structure(list(
  standard = structure(list(
    .model = c("standard", "standard", "standard",
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
  new = structure(list(
    .model = c("new", "new", "new", "new", 
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

bad_spec <- data.frame(
  .model = c("M1", "M2", "M3"), g1 = c(1,1,1), 
  g2 = c(2,2,1), x = c(0.5, 0.5, 3.0),
  stringsAsFactors = FALSE
)

test_that(
  "parse_multi_spec works.", {
    expect_equal(
      heemod:::parse_multi_spec(
        THRmulti_prob, ".model", c("from", "to")
      ),
      THRmulti_prob_result
    )
    
    expect_equal(
      heemod:::parse_multi_spec(
        THRmulti_prob_2, ".model", c("from", "to")
      ),
      THRmulti_prob_result
    )
    
    expect_equal(
      heemod:::parse_multi_spec(
        heemod:::read_file(THRmulti_prob_file_1),
        ".model", c("from", "to")
      ),
      THRmulti_prob_result
    )
    
    expect_equal(
      heemod:::parse_multi_spec(
        heemod:::read_file(THRmulti_prob_file_2),
        ".model", c("from", "to")
      ),
      THRmulti_prob_result
    )
    
    ## compound test - error comes from the input to multi_spec,
    ##    gets caught in f_transition_prob_matrix_from_tabular
    expect_error(
      heemod:::create_matrix_from_tabular(
        trans_probs = heemod:::parse_multi_spec(
          heemod:::read_file(THRmulti_prob_file_bad), 
          ".model", c("from", "to")
        )[[1]],
        state_names = state_names
      ),
      "Some states do not have an exit probability: Death."
    )
    
    expect_error(
      heemod:::parse_multi_spec(
        THRmulti_prob, ".model", c("from", "too")
      ),
      "'split_on' and 'group_vars' must be column names of the input 'multi_spec'.",
      fixed = TRUE
    )
    
    expect_error(
      heemod:::parse_multi_spec(
        THRmulti_prob, ".modell", c("from", "to")
      ),
      "'split_on' and 'group_vars' must be column names of the input 'multi_spec'.",
      fixed = TRUE
    )
    
    ## this will print a couple of explanatory lines to the output
    ##   as well as throwing the error
    expect_error(
      heemod:::parse_multi_spec(
        bad_spec, ".model", c("g1", "g2")
      ),
      "'group_var' combinations must be specified either once for all splits or once for each split.",
      fixed = TRUE
    )
    
    expect_error(
      heemod:::parse_multi_spec(
        bad_spec, c(".model", "g1") , "g2"
      ),
      "'split_on' must have a length of exactly 1.",
      fixed = TRUE
    )
    
    expect_error(
      heemod:::parse_multi_spec(
        bad_spec, character(0), c("g1", "g2")
      ),
      "'split_on' must have a length of exactly 1.",
      fixed = TRUE
    )
  }
)

tCSV <- heemod:::read_file(system.file(
  "tabular/test", "testing_CSV_file_with_comment_col.csv",
  package = "heemod"))
tXLS <- heemod:::read_file(system.file(
  "tabular/test", "testing_XLS_file_with_comment_col.xls",
  package = "heemod"))
tXLSX <- heemod:::read_file(system.file(
  "tabular/test", "testing_XLSX_file_with_comment_col.xlsx",
  package = "heemod"))

test_that(
  "Columns that start with '.comment' in their header are ignored.", {
    expect_that(
      names(tCSV),
      equals(c(".model", "state", "cost", "qaly", ".discount.cost", ".discount.qaly", "valid.column")))
    expect_that(
      names(tXLS),
      equals(c(".model", "state", "cost", "qaly", ".discount.cost", ".discount.qaly", "valid.column")))
    expect_that(
      names(tXLSX),
      equals(c(".model", "state", "cost", "qaly", ".discount.cost", ".discount.qaly", "valid.column"))
    )
  }
)

test_that(
  "Running model from files works.", {
    result <- run_models_tabular(
      location = system.file("tabular/thr", package = "heemod")
    )
    
    expect_identical(
      names(result),
      c("models", "model_runs", "dsa",      
        "psa","demographics")
    )
    
    expect_output(
      print(result$model_runs),
      "new -223.5199 0.04497522 -4969.845",
      fixed = TRUE
    )
    
    expect_output(
      print(result$dsa),
      "A sensitivity analysis on 6 parameters.",
      fixed = TRUE
    )
    
    expect_output(
      print(result$demographics),
      "An analysis re-run on 62 parameter sets.",
      fixed = TRUE
    )
  }
)
