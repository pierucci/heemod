N <- 2

par <- tibble::tibble(
  markov_cycle = 1:N,
  aa = sqrt(markov_cycle),
  bb = abs(sin(markov_cycle)),
  cc = 1 - bb
)
for (n in letters)
  par[[n]] <- runif(N)

mat <- define_matrix(
  C, 1/markov_cycle, 0, 0, 0,
  0, 1-bb, bb, 0, 0,
  0, 0, C, .5, 0,
  0, 0, 0, cc, C,
  .5, 0, 0, 0, .5
)

res <- heemod:::eval_matrix(mat, par)

library(microbenchmark)

x <- matrix(rep(1, 1e3), nrow = 200)

microbenchmark(
  heemod:::eval_matrix(mat, par)
)
