#' construct a survival object from tabular specification
#'
#' @param surv_def a data frame with the specification.  See details.
#' @param fit_matrix_name the name of the matrix from which to take fits.
#'   Typically produced by [survival_fits_from_tabular()].
#' @param env an environment
#' @details  This function is meant to be used only from within
#'   tabular_input.R.   It won't work well otherwise, in that
#'   the environment is unlikely to have what you need.
#' 
#' columns of surv_def:  .strategy, .type, dist, until
#'   dist can be either the name of a distribution
#'   along with parameters, or a reference to a fit
#'   for example:  fit('exp') or exp(rate = 0.5)

#' @return a list with one element for each strategy.   Each element
#'   is in turn a list with two elements, pfs and os.   And those
#'   elements are survival objects of various kinds, with the
#'   commonality that they can be used in [compute_surv()].
#'
#' @examples
construct_survival <-
  function(surv_def, fit_matrix_name = "fit_matrix",
           env = new.env()) {
    strategies <- unique(surv_def$.strategy)
    types <- unique(surv_def$.type)
    
    ## evaluate distribution text to get distributions
    
    ## if we have something of the form fit_matrix("exp") on a line
    ##   with .strategy A and .type os, replace it with
    ##   fit_matrix[["exp", "A"]]$os.
    
    surv_def$dist <-
      lapply(seq(along = surv_def$dist), function(i) {
        is_define_dist <-
          length(grep("define_survival", surv_def[i, "dist"])) > 0
        if (is_define_dist)
          res <- surv_def[i, "dist"]
        else{
          this_dist <- as.character(surv_def[i, "dist"])
          this_strategy <- as.character(surv_def[i, ".strategy"][[1]])
          this_type <- as.character(surv_def[i, ".type"][[1]])
          res <-
            gsub("fit\\((.*)\\)",
                 "fit_matrix[[\\1, zrw1]]\\$zrw2",
                 this_dist)
          res <-
            gsub("zrw1", paste("'", this_strategy, "'", sep = ""), res)
          res <- gsub("zrw2", this_type, res)
          res <- gsub("fit_matrix", fit_matrix_name, res)
          ## res <- parse(text = res)
        }
        res
      })
    
    # surv_def$dist <-
    #   lapply(surv_def$dist, function(this_dist) {
    #       if(is.character(this_dist))
    #         lazyeval::as.lazy(this_dist, env)
    #       else  
    #         lazyeval::as.lazy(eval(this_dist, env))
    #   })
    surv_def <- tibble::as_tibble(surv_def)
    
    res1 <-
      lapply(strategies,
             function(this_strategy) {
               surv_def_strategy <-
                 dplyr::filter(surv_def,
                               .strategy == this_strategy)
               res2 <- 
                 lapply(types,
                        function(this_type) {
                          this_part <-
                            dplyr::filter(surv_def_strategy,
                                          .type == this_type)
                          if ("until" %in% names(surv_def_strategy)) {
                            this_part <-
                              dplyr::arrange(this_part, until)
                            piece1 <- paste(this_part$dist, collapse = ", ")
                            piece1 <- paste("list(", piece1, ")")
                            piece2 <- paste(this_part$until[!is.na(this_part$until)], 
                                            collapse = ", ")
                            piece2 <- paste("c(", piece2, ")")
                            as.expression(
                              paste("project_(", 
                                    piece1,  
                                    ", ", 
                                    piece2,
                                    ")")
                            )
                            }
                          else{
                            if (nrow(this_part) > 1) {
                              print(this_part)
                              stop(
                                "can't have more than one distribution for a single ",
                                "strategy and type unless 'until' is also specified"
                              )
                            }
                            this_part$dist[[1]]
                          }
                        })
                res2 <-
                  lapply(res2, function(x) {
                      if(is.character(x))
                        lazyeval::as.lazy(x, env)
                      else
                        lazyeval::as.lazy(eval(x, env), env)
                  })

              names(res2) <- types
               res2
             })
    names(res1) <- strategies
    res1
  }