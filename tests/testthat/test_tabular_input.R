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
    
    expect_error(
      heemod:::gather_model_info(
        system.file("tabular/test", package = "heemod"),
        "bad_REFERENCE.csv")
    )
    expect_error(
      heemod:::gather_model_info(
        system.file("tabular/test/test_diff_mod_name", package = "heemod"),
        "REFERENCE.csv"),
      "newzzz"
    )
    expect_error(
      heemod:::gather_model_info(
        system.file("tabular/test", package = "heemod"),
        "REFERENCE_1probmissing.csv"),
      "Undefined probabilities"
    )
    
    dup_state <- structure(list(
      .model = c("standard", "standard", "standard", 
                 "standard", "standard"),
      .state = c("PrimaryTHR", "PrimaryTHR", 
                 "RevisionTHR", "SuccessfulRevision", "Death"),
      cost = c(0L, 0L, 
               5294L, 0L, 0L),
      qaly = c(0, 0.85, 0.3, 0.75, 0),
      .discount.qaly = c(0.015, 
                         NA, NA, NA, NA)),
      .Names = c(".model", ".state", "cost", "qaly", 
                 ".discount.qaly"),
      row.names = c(9L, 1L, 3L, 5L, 7L), class = "data.frame")
    
    expect_error(
      heemod:::create_states_from_tabular(dup_state)
    )
    
    pb_disc_state <- structure(list(
      .model = c("standard", "standard", "standard", 
                 "standard", "standard"),
      .state = c("PrimaryTHR", "SuccessfulPrimary", 
                 "RevisionTHR", "SuccessfulRevision", "Death"),
      cost = c(0L, 0L, 
               5294L, 0L, 0L),
      qaly = c(0, 0.85, 0.3, 0.75, 0),
      .discount.qaly = c(0.015, 
                         NA, NA, NA, NA)),
      .Names = c(".model", ".state", "cost", "qaly", 
                 ".discount.qalyz"),
      row.names = c(9L, 1L, 3L, 5L, 7L), class = "data.frame")
    
    expect_error(
      heemod:::create_states_from_tabular(pb_disc_state)
    )
    
    mult_disc_state <- structure(list(
      .model = c("standard", "standard", "standard", 
                 "standard", "standard"),
      .state = c("PrimaryTHR", "SuccessfulPrimary", 
                 "RevisionTHR", "SuccessfulRevision", "Death"),
      cost = c(0L, 0L, 
               5294L, 0L, 0L),
      qaly = c(0, 0.85, 0.3, 0.75, 0),
      .discount.qaly = c(0.015, 
                         0.016, NA, NA, NA)),
      .Names = c(".model", ".state", "cost", "qaly", 
                 ".discount.qaly"),
      row.names = c(9L, 1L, 3L, 5L, 7L), class = "data.frame")
    
    expect_error(
      heemod:::create_states_from_tabular(mult_disc_state)
    )
    
    bad_tm <- structure(list(
      .model = c("standard", "standard", "standard", 
                 "standard", "standard", "standard", "standard", "standard", "standard", 
                 "standard", "standard"),
      from = c("PrimaryTHR", "PrimaryTHR", 
               "SuccessfulPrimary", "SuccessfulPrimary", "SuccessfulPrimary", 
               "RevisionTHR", "RevisionTHR", "SuccessfulRevision", "SuccessfulRevision", 
               "SuccessfulRevision", "Death"),
      to = c("SuccessfulPrimary", "Death", 
             "SuccessfulPrimary", "Death", "RevisionTHR", "Death", "SuccessfulRevision", 
             "Death", "RevisionTHR", "SuccessfulRevision", "Death"),
      prob = c("C", 
               "0.02", "C", "mr", "pHRFailStandard", "0.02+mr", "C", "mr", "0.04", 
               "C", "1")),
      .Names = c(".model", "from", "to", "prob"),
      row.names = c(1L, 
                    3L, 5L, 7L, 21L, 11L, 9L, 17L, 15L, 13L, 19L),
      class = "data.frame")
    
    expect_error(
      heemod:::create_matrix_from_tabular(
        bad_tm, 
        c("Death", "PrimaryTHR", "RevisionTHR", "SuccessfulPrimary", 
          "SuccessfulRevisionzzz")
      )
    )
    
    pb_par <- structure(list(
      parameter = c("lngamma", "gamma"),
      value = c("0.3740968", 
                "exp(lngamma)"),
      low = c(0.2791966, NA),
      high = c(0.468997, 1),
      psa = c("normal(0.27, 0.001)", NA)),
      .Names = c("parameter", 
                 "value", "low", "high", "psa"),
      row.names = 1:2,
      class = "data.frame")
    
    expect_error(
      heemod:::create_parameters_from_tabular(pb_par)
    )
    
    pb_par2 <- pb_par
    names(pb_par2)[1] <- "param"
    expect_error(
      heemod:::create_parameters_from_tabular(pb_par2),
      "parameter file must include the column 'parameter'"
    )
    
    pb_par <- structure(list(
      parameter = c("lngamma", "gamma"),
      value = c("0.3740968", 
                "exp(lngamma)"),
      low = c(0.2791966, NA),
      psa = c("normal(0.27, 0.001)", NA)),
      .Names = c("parameter", 
                 "value", "low", "psa"),
      row.names = 1:2,
      class = "data.frame")
    
    expect_error(
      heemod:::create_parameters_from_tabular(pb_par)
    )
    
    pb_par <- structure(list(
      parameter = c("lngamma", "gamma"),
      value = c("0.3740968", 
                "exp(lngamma)"),
      low = c(NA, NA),
      high = c(NA, NA),
      psa = c("normal(0.27, 0.001)", NA)),
      .Names = c("parameter", 
                 "value", "low", "high", "psa"),
      row.names = 1:2,
      class = "data.frame")
    
    expect_error(
      heemod:::create_parameters_from_tabular(pb_par)
    )
    
    pb_par <- structure(list(
      parameter = c("lngamma", "gamma"),
      value = c("0.3740968", 
                "exp(lngamma)"),
      low = c(1, NA),
      high = c(2, NA),
      psa = c(NA, NA)),
      .Names = c("parameter", 
                 "value", "low", "high", "psa"),
      row.names = 1:2,
      class = "data.frame")
    
    expect_error(
      heemod:::create_parameters_from_tabular(pb_par)
    )
    
    opt_pb <- structure(list(
      option = c("cost", "effect", "method", "method", 
                 "n"),
      value = c("cost", "qaly", "end", "50", "100")),
      .Names = c("option", 
                 "value"),
      row.names = c(1L, 2L, 4L, 5L, 6L),
      class = "data.frame")
    
    expect_error(
      heemod:::create_options_from_tabular(opt_pb)
    )
    
    opt_pb <- structure(list(
      option = c("cost", "effect", "method", "cycleszzz", 
                 "n"),
      value = c("cost", "qaly", "end", "50", "100")),
      .Names = c("option", 
                 "value"),
      row.names = c(1L, 2L, 4L, 5L, 6L),
      class = "data.frame")
    
    expect_error(
      heemod:::create_options_from_tabular(opt_pb)
    )
    
    test_par <- define_parameters(
      a = 2,
      b = 3
    )
    ndt <- data.frame(
      a = 2,
      c = 4
    )
    expect_error(
      heemod:::create_demographic_table(ndt, test_par)
    )
    ndt <- data.frame(
      a = 2,
      c = 4,
      .weights = 3
    )
    expect_error(
      heemod:::create_demographic_table(ndt, test_par)
    )
    
    expect_error(
      heemod:::read_file(
        system.file("tabular/test/wrong_ext.tab", package = "heemod")
      )
    )
    
    expect_error(
      create_model_from_tabular(states1, NULL, NULL, new.env())
    )
  }
)

test_that(
  "problems with output generate warnings", {
    
    expect_warning(
      run_model_tabular(
        system.file("tabular/test/test_no_overwrite", package = "heemod"),
        save = TRUE, overwrite = FALSE, run_psa = FALSE, run_demo = FALSE
      )
    )
    expect_warning(
      run_model_tabular(
        system.file("tabular/test/test_no_output_dir", package = "heemod"),
        save = TRUE, overwrite = TRUE, run_psa = FALSE, run_demo = FALSE
      )
    )
  }
)

test_that(
  "absolute path works", {
    
    ref_edit <- heemod:::read_file(
      system.file("tabular/thr/REFERENCE.csv", package = "heemod")
    )
    ref_edit$absolute_path <- c(rep(1, nrow(ref_edit) - 1), NA)
    for (i in seq_len(nrow(ref_edit) - 1))
      ref_edit$file[i] <-
      system.file(sprintf(
        "tabular/thr/%s", ref_edit$file[i]),
        package = "heemod")
    
    write.csv(
      ref_edit,
      paste(system.file("tabular/test", package = "heemod"),
            "edited_ref.csv", sep = "/"),
      row.names = FALSE
    )
    
    op <- options(heemod.verbose = TRUE)
    expect_message(
      capture.output(heemod:::gather_model_info(
        system.file("tabular/test", package = "heemod"),
        "edited_ref.csv"
      )),
      "Using absolute path for state, tm, parameters, demographics, data, output"
    )
    options(op)
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
      "A transition matrix, 5 states\\.

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
    result <- run_model_tabular(
      location = system.file("tabular/thr", package = "heemod"),
      run_psa = TRUE, run_demo = TRUE,
      save = TRUE, overwrite = TRUE
    )
    
    expect_identical(
      names(result),
      c("models", "model_runs", "dsa",      
        "psa","demographics")
    )
    
    expect_output(
      print(result$model_runs),
      "-223.3065   0.04426563 -5044.693",
      fixed = TRUE
    )
    
    expect_output(
      print(result$dsa),
      "A sensitivity analysis on 6 parameters.",
      fixed = TRUE
    )
    
    expect_output(
      print(result$demographics),
      "An analysis re-run on 8 parameter sets.",
      fixed = TRUE
    )
    
    plot(result$demographics, type = "counts")
    plot(result$demographics, type = "values", value = "cost")
    plot(result$demographics, type = "values", 
         value = c("cost", "qaly"), panels = "by_value", free_y = TRUE)
    
  }
)

test_that(
  "safe conversion works", {
    
    expect_error(
      heemod:::as_integer_safe(c(1, 1.5, 2))
    )
    expect_error(
      heemod:::as_numeric_safe(c(1, "a", 2))
    )
  }
)
