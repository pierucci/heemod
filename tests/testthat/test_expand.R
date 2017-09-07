context("Test state cycle expansion")


sn <- LETTERS[1:5]
mn <- c("I", "II", "III")

test_that(
  "complete_stl works", {
    
    expect_identical(
      heemod:::complete_stl(NULL, sn, mn, 10),
      structure(list(I = structure(
        c(11, 11, 11, 11, 11),
        .Names = c("A",  "B", "C", "D", "E")),
        II = structure(
          c(11, 11, 11, 11, 11),
          .Names = c("A", 
                     "B", "C", "D", "E")),
        III = structure(
          c(11, 11, 11, 11, 11),
          .Names = c("A",  "B", "C", "D", "E"))),
        .Names = c("I", "II", "III"))
    )
    
    expect_identical(
      heemod:::complete_stl(5, sn, mn, 10),
      structure(list(I = structure(
        c(5, 5, 5, 5, 5), .Names = c("A", "B", "C", "D", "E")),
        II = structure(
          c(5, 5, 5, 5, 5), .Names = c("A", 
                                       "B", "C", "D", "E")),
        III = structure(
          c(5, 5, 5, 5, 5), .Names = c("A", 
                                       "B", "C", "D", "E"))),
        .Names = c("I", "II", "III"))
    )
    
    expect_identical(
      heemod:::complete_stl(c(B = 5), sn, mn, 10),
      structure(list(
        I = structure(
          c(10, 5, 10, 10, 10),
          .Names = c("A",
                     "B", "C", "D", "E")),
        II = structure(
          c(10, 5, 10, 10, 10),
          .Names = c("A", 
                     "B", "C", "D", "E")),
        III = structure(
          c(10, 5, 10, 10, 10), 
          .Names = c("A", 
                     "B", "C", "D", "E"))),
        .Names = c("I", "II", "III"))
    )
    
    expect_identical(
      heemod:::complete_stl(c(A = 5, E = 8), sn, mn, 10),
      structure(list(I = structure(
        c(5, 10, 10, 10, 8), .Names = c("A", 
                                        "B", "C", "D", "E")),
        II = structure(
          c(5, 10, 10, 10, 8), .Names = c("A", 
                                          "B", "C", "D", "E")),
        III = structure(
          c(5, 10, 10, 10, 8), .Names = c("A", 
                                          "B", "C", "D", "E"))),
        .Names = c("I", "II", "III"))
    )
    
    expect_identical(
      heemod:::complete_stl(
        list(
          I = c(A = 5, E = 8),
          III = c(B = 2, C = 4)
        ),
        sn, mn, 10),
      structure(list(
        I = structure(c(5, 10, 10, 10, 8),
                      .Names = c("A", 
                                 "B", "C", "D", "E")),
        II = structure(c(10, 10, 10, 10, 10),
                       .Names = c("A", 
                                  "B", "C", "D", "E")),
        III = structure(c(10, 2, 4, 10, 10),
                        .Names = c("A", 
                                   "B", "C", "D", "E"))),
        .Names = c("I", "II", "III"))
    )
  }
)

test_that(
  "complete_stl throws errors", {
    
    expect_error(
      heemod:::complete_stl(-1, sn, mn, 10)
    )
    expect_error(
      heemod:::complete_stl(NA, sn, mn, 10)
    )
    expect_error(
      heemod:::complete_stl(12, sn, mn, 10)
    )
    expect_error(
      heemod:::complete_stl(5.5, sn, mn, 10)
    )
    
    expect_error(
      heemod:::complete_stl(c(A = 1, B = -1), sn, mn, 10)
    )
    expect_error(
      heemod:::complete_stl(c(A = 1, B = NA), sn, mn, 10)
    )
    expect_error(
      heemod:::complete_stl(c(A = 1, B = 12), sn, mn, 10)
    )
    expect_error(
      heemod:::complete_stl(c(A = 1, B = 5.5), sn, mn, 10)
    )
    
    expect_error(
      heemod:::complete_stl(c(A = 1, G = 5), sn, mn, 10)
    )
    
    expect_error(
      heemod:::complete_stl(
        list(
          I = c(A = 5, E = 8),
          III = c(B = 2, C = -1)
        ),
        sn, mn, 10)
    )
    expect_error(
      heemod:::complete_stl(
        list(
          I = c(A = 5, E = 8),
          III = c(B = 2, C = NA)
        ),
        sn, mn, 10)
    )
    expect_error(
      heemod:::complete_stl(
        list(
          I = c(A = 5, E = 8),
          III = c(B = 2, C = 12)
        ),
        sn, mn, 10)
    )
    expect_error(
      heemod:::complete_stl(
        list(
          I = c(A = 5, E = 8),
          III = c(B = 2, C = 5.5)
        ),
        sn, mn, 10)
    )

    expect_error(
      heemod:::complete_stl(
        list(
          I = c(A = 5, E = 8),
          III = c(B = 2, G = 5)
        ),
        sn, mn, 10)
    )
    expect_error(
      heemod:::complete_stl(
        list(
          I = c(A = 5, E = 8),
          VIII = c(B = 2, C = 5)
        ),
        sn, mn, 10)
    )
  }
)

test_that(
  "Expansion works.", {
    f <- function(x) abs(sin(x))
    
    tm <- define_transition(
      .5, .5,
      .3, .7
    )
    tm_exp <- define_transition(
      .4, .6,
      C, f(state_time)
    )
    
    sA <- define_state(
      c = 5,
      e = 3
    )
    sB <- define_state(
      c = 3,
      e = 9
    )
    sA_exp <- define_state(
      c = f(state_time),
      e = 7
    )
    
    expect_message(
      res <- run_model(
        define_strategy(
          transition = tm_exp,
          sA, sB
        ),
        cycles = 10,
        cost = c, effect = e
      ),
      "expanding state: B\\."
    )
    expect_equivalent(
      round(unlist(res$run_model[c(".cost", ".effect")]), 2),
      c(37697.04, 66908.88)
    )
    
    expect_message(
      res <- run_model(
          define_strategy(
            transition = tm,
            sA_exp, sB
          ),
          cycles = 10,
          cost = c, effect = e
        ),
      "expanding state: A\\."
    )
    expect_equivalent(
      round(unlist(res$run_model[c(".cost", ".effect")]), 2),
      c(20552.39, 81562.5)
    )
    
    expect_message(
      res <- run_model(
        define_strategy(
          transition = tm_exp,
          sA_exp, sB
        ),
        cycles = 10,
        cost = c, effect = e
      ),
      "expanding states: A, B\\."
    )
    expect_equivalent(
      round(unlist(res$run_model[c(".cost", ".effect")]), 2),
      c(21488.12, 82302.96)
    )
    
    # Changed expected result to reflect solution to issue #307
    
    res <- run_model(
      define_strategy(
        transition = tm_exp,
        sA_exp, sB
      ),
      cycles = 10,
      cost = c, effect = e,
      state_time_limit = c(A = 3, B = 8)
    )
    expect_equivalent(
      round(unlist(res$run_model[c(".cost", ".effect")]), 2),
      c(21364.69, 82307.89)
    )
    
    res1 <- run_model(
      define_strategy(
        transition = tm_exp,
        sA_exp, sB
      ),
      cycles = 10,
      cost = c, effect = e,
      state_time_limit = 5
    )
    res2 <- run_model(
      define_strategy(
        transition = tm_exp,
        sA_exp, sB
      ),
      cycles = 10,
      cost = c, effect = e,
      state_time_limit = c(A = 5, B = 5)
    )
    expect_equivalent(
      res1$run_model, res2$run_model
    )
    
    res1 <- run_model(
      define_strategy(
        transition = tm_exp,
        sA_exp, sB
      ),
      cycles = 10,
      cost = c, effect = e,
      state_time_limit = c(B = 7)
    )
    res2 <- run_model(
      define_strategy(
        transition = tm_exp,
        sA_exp, sB
      ),
      cycles = 10,
      cost = c, effect = e,
      state_time_limit = c(A = 10, B = 7)
    )
    expect_equivalent(
      res1$run_model, res2$run_model
    )
    
    
    f <- function(x) {
      abs(sin(x)) / 2
    }
    f(1:10)
    mat_mc <- define_transition(
      C, f(markov_cycle),
      0, 1
    )
    mat_sc <- define_transition(
      C, f(state_time),
      0, 1
    )
    sA <- define_state(c = 1, e = 1)
    sB <- define_state(c = 0, e = 0)
    
    strat_mc <- define_strategy(
      transition = mat_mc,
      sA, sB
    )
    strat_sc <- define_strategy(
      transition = mat_sc,
      sA, sB
    )
    res <- summary(run_model(
      strat_sc, strat_mc,
      cycles = 10,
      cost = c, effect = e
    ))
    
    expect_identical(
      res$res_comp$.icer[2], NaN
    )
  }
)