construct_survival <-
  function(surv_def) {
    ## columns:  .strategy, .type, dist, until
    ## dist can be either the name of a distribution
    ##   along with parameters, or a reference to a fit
    ##   for example:  fit_matrix[["exp"]]
    ##                 exp(rate = 0.5)
    
    
    
    strategies <- unique(surv_def$.strategy)
    types <- unique(surv_def$.type)
    
    ## evaluate distribution text to get distributions
    
    ## if we have something of the form fit_matrix("exp") on a line
    ##   with .strategy A and .type os, replace it with
    ##   fit_matrix[["exp", "A"]]$os.
    
    surv_def$dist <- 
      lapply(seq(along = surv_def$dist), function(i){
        is_define_dist <- 
          length(grep("define_survival", surv_def[i, "dist"])) > 0
        if(is_define_dist)
          res <- surv_def[i, "dist"]
        else{
        this_dist <- as.character(surv_def[i, "dist"])
        this_strategy <- as.character(surv_def[i, ".strategy"][[1]])
        this_type <- as.character(surv_def[i, ".type"][[1]])
        res <- 
          gsub("()\\((.*)\\)", "\\1[[\\2, zrw1]]\\$zrw2", this_dist)
        res <- gsub("zrw1", paste("'", this_strategy, "'", sep = ""), res)
        res <- gsub("zrw2", this_type, res)
        }
      res
      })
  
    surv_def$dist <- 
      lapply(surv_def$dist, function(this_dist){
        lazyeval::lazy_(this_dist, 
                        env = parent.frame())
      })
    surv_def <- tibble::as_tibble(surv_def)
    
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