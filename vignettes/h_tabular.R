## ---- echo=FALSE, include=FALSE------------------------------------------
library(heemod)
library(dplyr)
format_na <- function(x, char = " ") {
  x[is.na(x)] <- char
  x
}

## ----echo = FALSE--------------------------------------------------------
heemod:::read_file(system.file("tabular/thr/REFERENCE.csv", package = "heemod")) %>% 
  format_na %>% 
  knitr::kable()

## ----echo = FALSE--------------------------------------------------------
heemod:::read_file(system.file("tabular/thr/THR_states.csv", package = "heemod")) %>% 
  format_na %>% 
  knitr::kable()

## ----echo = FALSE--------------------------------------------------------
heemod:::read_file(system.file("tabular/thr/THR_transition_probs.csv", package = "heemod")) %>% 
  format_na %>% 
  knitr::kable()

## ----echo = FALSE--------------------------------------------------------
heemod:::read_file(system.file("tabular/thr/THR_parameters.csv", package = "heemod")) %>% 
  format_na %>% 
  knitr::kable()

## ----echo = FALSE--------------------------------------------------------
heemod:::read_file(system.file("tabular/thr/THR_options.csv", package = "heemod")) %>% 
  format_na %>% 
  knitr::kable(row.names = FALSE)

## ------------------------------------------------------------------------
result <- run_model_tabular(
  location = system.file("tabular/thr", package = "heemod")
)

## ---- fig.width = 6, fig.align='center'----------------------------------
result$model_runs
plot(result$psa,
     type = "ce")
plot(result$dsa,
     result = "cost",
     strategy = "new")
result$demographics

