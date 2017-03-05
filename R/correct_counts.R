correct_counts <- function(x, method = c("life-table",
                                         "beginning",
                                         "end")) {
  
  if (! is.function(method)) {
    method <- match.arg(method)
    
    n0 <- x[- nrow(x), ]
    n1 <- x[-1, ]
    
    switch(
      method,
      "beginning" = {
        out <- n1
      },
      "end" = {
        out <- n0
      },
      "life-table" = {
        out <- (n0 + n1) / 2
      })
  } else {
    out <- method(x)
  }
  
  if (nrow(out) != nrow(x) - 1) {
    stop("State membership correction applied to an n-row table should return a table with n-1 rows.")
  }
  
  return(out)
}
