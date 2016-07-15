context("Transformation functions")

# from http://www.hsrd.research.va.gov/for_researchers/cyber_seminars/archives/819-notes.pdf
test_that(
  "Transformation functions produce correct results", {
    expect_equal(
      round(prob_to_prob(.6, from = 3, to = 1), 2),
      .26
    )
    expect_equal(
      round(prob_to_prob(.3, from = 5, to = 1), 4),
      .0689
    )
    expect_equal(
      round(rr_to_prob(2.37, .17), 3),
      .403
    )
    # https://en.wikipedia.org/wiki/Odds_ratio#Example
    
    expect_equal(
      round(or_to_prob(or = 36, .2), 2),
      .79
    )
  }
)

test_that(
  "Transformation functions fail when they should", {
    expect_error(rr_to_prob(2, .6))
  }
)
