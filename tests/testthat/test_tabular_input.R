context("Test tabular input")

testdir <- file.path(tempdir(), "tabular", "test")

state_spec_file <- file.path(
  testdir,
  "THR_test_states.csv"
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
  "can read multinomial parameters from file", {
    from_input <- 
      define_psa(p_AA ~binomial(.7, 1000), 
                 p_AB + p_AC + p_AD ~ multinomial(202, 67, 10))
    from_file <-
      create_parameters_from_tabular(read_file(file.path(testdir, 
        "example_multinom_params.csv"
      )))
    
    expect_identical(
      names(from_file$params),
      c("p_AA", "p_AB", "p_AC", "p_AD")
    )
    expect_identical(
      round(sapply(from_file$params, "[[", "expr"), 4),
      c(p_AA = 721.0000, p_AB = 0.7240, p_AC = 0.2401, p_AD = 0.0358)
    )
    
    expect_error(
      create_parameters_from_tabular(read_file(file.path(testdir, 
        "example_multinom_params_dup_name.csv"
      ))),
      "Some variables appear as individual parameters and in a multinomial"
    )
    
    
    ## can't test identity of multinomial part
    ##   because of environments, so test results
    set.seed(5)
    from_input_draws <- eval_resample(from_input, 10)
    set.seed(5)
    from_file_draws <- eval_resample(from_file$psa_params, 10)
    expect_identical(from_input_draws, from_file_draws)
  }
)

test_that(
  "Bad spec file input is caught.", {
    expect_error(
      heemod:::gather_model_info(
        testdir,
        "bad_REFERENCE.csv"),
      "Duplicated values in reference file 'data' column: state."
    )
    expect_error(
      heemod:::gather_model_info(
        file.path(testdir, "test_diff_mod_name"),
        "REFERENCE.csv"),
      "newzzz"
    )
    expect_error(
      capture.output(
        heemod:::gather_model_info(
          testdir,
          "REFERENCE_1probmissing.csv")
      ),
      "Undefined probabilities"
    )
    expect_error(
      heemod:::gather_model_info(
        testdir,
        "REFERENCE_missingfunctions.csv"),
      "'source' directory missing: ",
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
      "'.state' should be a column name of the state file.",
      fixed = TRUE
    )
    
    na_state_spec <- data.frame(
      .model = rep("A", 5),
      .state = c("PrimaryTHR", "SuccessfulPrimary",
                 "RevisionTHR", "SuccessfulRevision",
                 "Death"),
      cost = c(0, 0, 5294, 0, NA),
      qaly = c(0.0, 0.85, 0.30, 0.75, 0.00),
      .discount.qaly = c(0.01, NA, NA, NA, NA),
      stringsAsFactors = FALSE
    )
    expect_error(create_states_from_tabular(na_state_spec),
                 "value cost for strategy 'A' has missing values",
                 fixed = TRUE
                 )
    na_state_spec$qaly[1] <- NA
    expect_error(create_states_from_tabular(na_state_spec),
                 "values cost, qaly for strategy 'A' have missing values",
                 fixed = TRUE
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
      heemod:::create_states_from_tabular(dup_state),
      "Duplicated state names: PrimaryTHR",
      fixed = TRUE
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
      heemod:::create_states_from_tabular(pb_disc_state),
      "Discounting rates defined for non-existing values: .discount.qalyz",
      fixed = TRUE
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
      heemod:::create_states_from_tabular(mult_disc_state),
      "Multiple discount values for '.discount.qaly'.",
      fixed = TRUE
    )
  }
)

test_that(
  "Bad transmission matrix input is caught.", {
    
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
      ),
      "Some states specified in the transition matrix differ from 'state_names'"
    )
  }
)

test_that(
  "Bad parameter file input is caught.", {
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
      heemod:::create_parameters_from_tabular(pb_par),
      "'low' and 'high' must be both non missing in DSA tabular definition."
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
      heemod:::create_parameters_from_tabular(pb_par),
      "Both 'low' and 'high' columns must be present"
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
      heemod:::create_parameters_from_tabular(pb_par),
      "No non-missing values in columns 'low' and 'high'"
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
      heemod:::create_parameters_from_tabular(pb_par),
      "No non-missing values in column 'psa'."
    )
  }
)

test_that(
  "other bad input is caught.", {
    
    opt_pb <- structure(list(
      option = c("cost", "effect", "method", "method", 
                 "n"),
      value = c("cost", "qaly", "end", "50", "100")),
      .Names = c("option", 
                 "value"),
      row.names = c(1L, 2L, 4L, 5L, 6L),
      class = "data.frame")
    
    expect_error(
      heemod:::create_options_from_tabular(opt_pb),
      "Some option names are duplicated: method",
      fixed = TRUE
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
      heemod:::create_options_from_tabular(opt_pb),
      "Unknown options: cycleszzz", 
      fixed = TRUE
    )
  
    opt_pb <- structure(list(
      option = c("cost", "effect", "method", "cycles", 
                 "n", "init"),
      value = c("cost", "qaly", "end", "50", "100", "c(1, 0, 0, 0)")),
      .Names = c("option", 
                 "value"),
      row.names = c(1L, 2L, 3L, 4L, 5L, 6L),
      class = "data.frame")
    
    expect_warning(
      heemod:::create_options_from_tabular(opt_pb),
      "initial values enclosed in c(); removing",
      fixed = TRUE
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
        file.path(testdir, "wrong_ext.tab")
      ),
      "file names must be for csv, xls, or xlsx"
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
        file.path(testdir, "test_no_overwrite"),
        save = TRUE, overwrite = FALSE, run_psa = FALSE, run_demo = FALSE
      )
    )
    expect_warning(
      run_model_tabular(
        file.path(testdir, "test_no_output_dir"),
        save = TRUE, overwrite = TRUE, run_psa = FALSE, run_demo = FALSE
      )
    )
  }
)

test_that(
  "absolute path works", {
    
    ref_edit <- heemod:::read_file(
      file.path(
        tempdir(), "tabular", "thr", "REFERENCE.csv")
    )
    ref_edit$absolute_path <- c(rep(1, nrow(ref_edit) - 1), NA)
    for (i in seq_len(nrow(ref_edit) - 1))
      ref_edit$file[i] <-
      file.path(tempdir(),
        "tabular", "thr",
        ref_edit$file[i])
    
    write.csv(
      ref_edit,
      file.path(testdir,
            "edited_ref.csv"),
      row.names = FALSE
    )
    op <- options(heemod.verbose = TRUE)
    expect_message(
      capture.output(heemod:::gather_model_info(
        testdir,
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
      heemod:::create_matrix_from_tabular(tm_spec, NULL),
      "length(state_names) > 0 is not TRUE",
      fixed = TRUE
    )
    expect_error(
      heemod:::create_matrix_from_tabular(NULL, state_names),
      "'trans_probs' must be a data frame."
    )
  }
)

THRmulti_prob_file_1 <- file.path(testdir, "THR_test_transition_probs.csv")
THRmulti_prob_file_2 <- file.path(testdir, "THR_test_transition_probs_2.csv")
THRmulti_prob_file_bad <- file.path(testdir, "THR_test_transition_probs_bad.csv")


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

tCSV <- heemod:::read_file(file.path(testdir, 
  "testing_CSV_file_with_comment_col.csv"
  ))
tXLS <- heemod:::read_file(file.path(testdir, 
  "testing_XLS_file_with_comment_col.xls"
  ))
tXLSX <- heemod:::read_file(file.path(testdir, 
"testing_XLSX_file_with_comment_col.xlsx"
  ))

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
      location = file.path(tempdir(), "tabular", "thr"),
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

