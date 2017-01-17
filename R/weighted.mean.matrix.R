weighted.mean.matrix <- function(x, w, ...){
  rowSums(x * w)/rowSums(w)

}

weighted.sum.matrix <- function(x, w, ...){
  rowSums(x * w)
  
}