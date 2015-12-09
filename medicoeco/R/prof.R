
library(lineprof)

purl("~/Documents/package_medicoeco/demo.Rmd", output = "~/test.R")

write("
f <- function()
 simulate_cohort_iter(
    model = mod_pth_2,
    cycles = 60,
    init = init_pth,
    newdata = tab
  )
f()
      ",
      file = "~/test.R",
      append = TRUE)

source("~/test.R")

res <- lineprof(f(), interval = 1e-4)

shine(res)
