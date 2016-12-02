#' Calculate weighted costs associated with adverse events
#'
#' @param ae_df \code{data.frame} with information about adverse events
#' @param treatment the treatment for which the adverse event costs 
#'   should be calculated.
#' @param treatment_col column of \code{ae_df} holding treatment name
#' @param prob_col column of \code{ae_df} holding probability of an adverse
#'   event
#' @param cost_col column of \code{ae_df} holding costs
#' @param ... arguments to be passed through from \code{weighted_ae_costs_all}
#'   to \code{weighted_ae_costs}
#'
#' @return the weighted cost of adverse events for the given treatment
#' @export
#' @details 
#'   \code{weighted_ae_costs_all} calculates the costs for each treatment
#'     in \code{ae_df}.
#'
#' @examples
#' ae_df <- data.frame(treatment = rep(c("T1", "T2", "T3"), each = 3),
#' ae = rep(c("AE1", "AE2", "AE3"), 3),
#' prob = c(0.25, 0, 0.5, 0.2, 0.2, 0.2, 0, 0, 0),
#' cost = rep(c(1000, 500, 200), 3))
#' weighted_ae_costs(ae_df, treatment = "T2")

weighted_ae_costs <- function(ae_df, treatment = NULL, treatment_col = "treatment",
                          prob_col = "prob", cost_col = "cost"){
  if(!inherits(ae_df, "data.frame"))
    stop("ae_df must be a data.frame")
  if(nrow(ae_df) == 0) stop("ae_df should not be empty")
  ## if treatment is NULL, we don't need to care about treatment_col
  if(!is.null(treatment) & !(treatment_col %in% names(ae_df)))
    stop("treatment_col must name a column of ae_df")
  if(any(!c(prob_col, cost_col) %in% names(ae_df)))
    stop("prob_col and cost_col must name columns of ae_df")
  if(!is.null(treatment))
    ae_df <- ae_df[ae_df[, treatment_col] == treatment, ]
  ae_df[[prob_col]] %*% ae_df[[cost_col]]
  
}

#' @rdname weighted_ae_costs
#' @export
#' @examples
#' ae_df <- data.frame(treatment = rep(c("T1", "T2", "T3"), each = 3),
#'                     ae = rep(c("AE1", "AE2", "AE3"), 3),
#'                     prob = c(0.25, 0, 0.5, 0.2, 0.2, 0.2, 0, 0, 0),
#'                     cost = rep(c(1000, 500, 200), 3))
#' weighted_ae_costs_all(ae_df)

weighted_ae_costs_all <- function(ae_df, treatment_col = "treatment", ...){
  if(!inherits(ae_df, "data.frame")){
      stop("ae_df must be a data.frame")
  }
  res <- ae_df %>% 
          dplyr::group_by_(., treatment_col) %>%
          dplyr::do(., data.frame(weighted_ae_costs(., ...)))
  names(res)[2] <- "weighted_cost"
  res
  }
