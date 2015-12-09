is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)
  abs(x - round(x)) < tol

resample <- function(x)
  structure(x(), class = "resample")

discount <- function(x, r)
  x / (1 + r) ^ (seq_along(x))

list_all_same <- function(x)
  length(x) == 0 |
  all(unlist(
    Map(function(y) identical(y, x[[1]]), x)
  ))
