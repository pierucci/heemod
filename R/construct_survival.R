construct_survival <-
  function(surv_def) {
    ## columns:  .strategy, .type, dist, until
    ## dist can be either the name of a distribution
    ##   along with parameters, or a reference to a fit
    ##   for example:  fit_matrix[["exp"]]
    ##                 exp(rate = 0.5)
    
    surv_def <- tibble::as_tibble(surv_def)
    
    
    strategies <- unique(surv_def$.strategy)
    types <- unique(surv_def$.type)
    
    ## evaluate distribution text to get distributions
    
    
    
    surv_def$dist <-
      lapply(surv_def$dist, function(this_dist){
        lazyeval::lazy_(substitute(this_dist), env = parent.frame())
      })
      
        
    res1 <-
      lapply(strategies,
             function(this_strategy) {
               surv_def_strategy <-
                 dplyr::filter(surv_def,
                               .strategy == this_strategy)
               res2 <- lapply(types,
                              function(this_type) {
                                this_part <-
                                  dplyr::filter(surv_def_strategy,
                                                .type == this_type)
                                project_(this_part$dist,
                                         this_part$until[!is.na(this_part$until)])
                              })
               names(res2) <- types
               res2
             })
    names(res1) <- strategies
    res1
  }