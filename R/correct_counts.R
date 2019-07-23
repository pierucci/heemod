correct_counts <- function(x, method = c("life-table",
                                         "beginning",
                                         "end")) {
  counts <- x$counts
  diff <- x$diff
  if (! is.function(method)) {
    method <- match.arg(method)
    
    n0 <- counts[- nrow(counts), ]
   
    n1 <- counts[-1, ]
    
    out <- list()
    
    switch(
      method,
      "beginning" = {
        out$counts <- n1
        if(!is.null(diff)) out$diff <- diff
      },
      "end" = {
        out$counts <- n0
        if(!is.null(diff)) out$diff <- diff
      },
      "life-table" = {
        out$counts <- (n0 + n1) / 2
        if(!is.null(diff)){
          out$diff <- lapply(seq_along(diff), function(i){
            half <- diff[[i]] / 2
            if (i > 1) half <- half + diff[[i-1]]/2
            return(half)
          })
        }
      })
  } else {
    out <- method(counts)
  }
  
  if (nrow(out$counts) != nrow(counts) - 1) {
    stop("State membership correction applied to an n-row table should return a table with n-1 rows.")
  }
  
  return(out)
}
