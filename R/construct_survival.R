#' construct a survival object from tabular specification
#'
#' @param surv_def a data frame with the specification.  See details.
#' @param fit_tib the name of the tibble from which to take fits.
#'   Typically produced by [survival_fits_from_tabular()].
#' @param env an environment
#' @param state_names names of the model states
#' @details  This function is meant to be used only from within
#'   tabular_input.R.   It won't work well otherwise, in that
#'   the environment is unlikely to have what you need.
#' 
#' columns of surv_def:  .strategy, .type, .subset, dist, until
#'   where dist can be either the name of a distribution
#'   along with parameters, or a reference to a fit
#'   for example:  fit('exp') or exp(rate = 0.5)

#' @return a list with one element for each strategy.   Each element
#'   is in turn a list with two elements, pfs and os.   And those
#'   elements are survival objects of various kinds, with the
#'   commonality that they can be used in [compute_surv()].
#'
#' @examples

construct_survival <-
  function(surv_def, fit_tibble,
           state_names,
           env = new.env()) {
    
    if(!(".subset" %in% names(surv_def)))
      surv_def$.subset <- "all"
    
    surv_def_2 <- 
      surv_def %>% 
      dplyr::mutate(dist = gsub("fit\\((.*)\\)", "\\1", dist)) %>%
      dplyr::mutate(dist = gsub("'", "", dist)) %>%
      dplyr::mutate(dist = gsub('"', '', dist)) %>%
      dplyr::mutate(.type = toupper(.type))
    
    fit_tibble <-
      dplyr::mutate(fit_tibble, type = toupper(type)) 
    
    surv_def_3 <- 
      surv_def_2 %>% dplyr::left_join(., fit_tibble,
                                      by = c(".strategy" = "treatment",
                                             ".type" = "type",
                                             "dist" = "dist",
                                             ".subset" = "set_name")
                                        
      )
    
    
    
    surv_def_4 <- 
      surv_def_3 %>% 
      dplyr::group_by(.strategy, .type, .subset) %>%
      dplyr::do(fit = join_fits_across_time(.)) %>%
      dplyr::ungroup()
    surv_def_5 <- 
      surv_def_4 %>%
      dplyr::group_by(.strategy, .subset) %>%
      dplyr::rename(type = .type) %>%
      dplyr::do(part_surv = make_part_surv_from_small_tibble(.,
                      state_names = state_names))
    surv_def_5
  }

join_fits_across_time <- function(this_part){
  if ("until" %in% names(this_part)) {
    this_part <-
      dplyr::arrange(this_part, until)
    
    project_(dots = this_part$fit, 
             at= this_part$until[!is.na(this_part$until)])
    
  }  
  else{
    if (nrow(this_part) > 1) {
      print(this_part)
      stop(
        "can't have more than one distribution for a single ",
        "strategy and type unless 'until' is also specified"
      )
    }
    this_part$fit[[1]]
  }
  
}

