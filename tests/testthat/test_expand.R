context("Test state cycle expansion")


sn <- LETTERS[1:5]
mn <- c("I", "II", "III")

test_that(
  "complete_scl works", {
    
    expect_identical(
      heemod:::complete_scl(NULL, sn, mn, 10),
      structure(list(I = structure(
        c(10, 10, 10, 10, 10),
        .Names = c("A",  "B", "C", "D", "E")),
        II = structure(
          c(10, 10, 10, 10, 10),
          .Names = c("A", 
                     "B", "C", "D", "E")),
        III = structure(
          c(10, 10, 10, 10, 10),
          .Names = c("A",  "B", "C", "D", "E"))),
        .Names = c("I", "II", "III"))
    )
    
    expect_identical(
      heemod:::complete_scl(5, sn, mn, 10),
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
      heemod:::complete_scl(c(B = 5), sn, mn, 10),
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
      heemod:::complete_scl(c(A = 5, E = 8), sn, mn, 10),
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
      heemod:::complete_scl(
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
  "complete_scl throws errors", {
    
    expect_error(
      heemod:::complete_scl(-1, sn, mn, 10)
    )
    expect_error(
      heemod:::complete_scl(NA, sn, mn, 10)
    )
    expect_error(
      heemod:::complete_scl(11, sn, mn, 10)
    )
    expect_error(
      heemod:::complete_scl(5.5, sn, mn, 10)
    )
    
    expect_error(
      heemod:::complete_scl(c(A = 1, B = -1), sn, mn, 10)
    )
    expect_error(
      heemod:::complete_scl(c(A = 1, B = NA), sn, mn, 10)
    )
    expect_error(
      heemod:::complete_scl(c(A = 1, B = 11), sn, mn, 10)
    )
    expect_error(
      heemod:::complete_scl(c(A = 1, B = 5.5), sn, mn, 10)
    )
    
    expect_error(
      heemod:::complete_scl(c(A = 1, G = 5), sn, mn, 10)
    )
    
    expect_error(
      heemod:::complete_scl(
        list(
          I = c(A = 5, E = 8),
          III = c(B = 2, C = -1)
        ),
        sn, mn, 10)
    )
    expect_error(
      heemod:::complete_scl(
        list(
          I = c(A = 5, E = 8),
          III = c(B = 2, C = NA)
        ),
        sn, mn, 10)
    )
    expect_error(
      heemod:::complete_scl(
        list(
          I = c(A = 5, E = 8),
          III = c(B = 2, C = 11)
        ),
        sn, mn, 10)
    )
    expect_error(
      heemod:::complete_scl(
        list(
          I = c(A = 5, E = 8),
          III = c(B = 2, C = 5.5)
        ),
        sn, mn, 10)
    )

    expect_error(
      heemod:::complete_scl(
        list(
          I = c(A = 5, E = 8),
          III = c(B = 2, G = 5)
        ),
        sn, mn, 10)
    )
    expect_error(
      heemod:::complete_scl(
        list(
          I = c(A = 5, E = 8),
          VIII = c(B = 2, C = 5)
        ),
        sn, mn, 10)
    )
  }
)
