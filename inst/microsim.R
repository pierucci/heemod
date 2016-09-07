
test <- function(x, max_depth = 0, depth = 0) {
  vec <- rep(0, length(x)^2)
  vec[seq(1, length(x)^2, length(x) + 1)] <- 1
  l_vec <- split(vec, rep(seq_along(x), each = length(x)))
  
  res <- lapply(l_vec, function(y) x * y + y)
  
  if (depth == max_depth) {
    return(res)
  } else {
    lapply(res, test, depth = depth + 1, max_depth = max_depth)
  }
}
