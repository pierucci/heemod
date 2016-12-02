#' Title  Re-scales the input in time, converting the value between two time-spans.
#'
#' @param input name or value of a parameter that is to be re-scaled  from the original time scale \code{scale_from} to the new scale \code{scale_to}.
#' @param scale_from length of the original time period
#' @param scale_to length of the original time period
#' @details Some input parameters may have different time scale than is the model time-step, 
#'   e.g. discount rates for costs and benefits are typically annual, while the model time-step 
#'   is likely to be shorter. To assure the discounting is correct, the correctly scaled input values
#'   must be provided to the calculation.
#'   Users may use this function to convert the inputs into the horizon of the model time-step.
#'   Continuous discounting is assumed, i.e. when converting a long-term discount rate into a short-term rate, 
#'   we assume that a partial gain from one short term is multiplicatively discounted in all following
#'   short terms. This means, e.g. that for a monthly rate of 1% we get the annual rate of 12.6825% (i.e. 
#'   (1+0.01)^12 - 1; example from https://en.wikipedia.org/wiki/Compound_interest).
#'   At the same time, we assume the short-term rate is time-invariant.
#'   Ad time units: we use only ratio of the two scales; thus, the time unit is arbitrary.
#'   
#'   
#' @return value scaled under the assumption of *compound* discounting (read here: https://en.wikipedia.org/wiki/Compound_interest).
#'
#' @export
#'
#' @examples
#'   ## 1% monthly interest rate to annual
#'   convert_timescale_of_parameters(0.01, 1, 12)
#'   ## 3% annual discount rate to (approximately) weekly 
#'   convert_timescale_of_parameters(0.03, 52, 1)
convert_timescale_of_parameters <- 
     function(input, 
              scale_from,
              scale_to){
       ((1 + input) ^ (scale_to/scale_from)) -1
     }
       