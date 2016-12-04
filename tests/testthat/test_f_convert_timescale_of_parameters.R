context("Conversion of time-scale of the parameters")

test_that("Using different time units does not matter, as long as the ratio of scale_from / scale_to is constants",
          
          {# quarters vs. days
            expect_that(convert_timescale_of_parameters(0.03, 4, 1),
                       equals(convert_timescale_of_parameters(0.03, 365.25, 365.25/4))
                       )
          # years vs. days
            expect_that(convert_timescale_of_parameters(0.03, 1, 0.25),
                       equals(convert_timescale_of_parameters(0.03, 365.25, 365.25/4))
                       )
          # Wikipedia example: 
            expect_that(convert_timescale_of_parameters(0.01, 1, 12),
                        equals(0.126825030131969720661201)
            )
          }
          )