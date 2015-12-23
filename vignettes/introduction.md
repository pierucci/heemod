# An Introduction to heemod
`r Sys.Date()`  



This document is an introduction to `heemod`'s basic steps to define and run a model.

When building a Markov model for health economic evaluation, the following steps must be performed:

  * Specify transition probabilities between states.
  * Specify values attached to states (costs, utilities...).
  * Combine this information and run the model.
  
Other vignettes provide more details and examples on specific topics:

  * Simple Markov models: `vignette("homogenous", package = "heemod")`: How to specify, run and interpret a simple Markov model with no time-varying elements.
  * Time-varying Markov models: `vignette("non-homogenous", package = "heemod")`: Example analysis of a more complex Markov model with time-varying transition probabilities.

# Transition probabilities

The probability to change from one state to another during a time period is called a *transition probability*. The time period is called a *cycle*.

Transition probabilities between states can be specified through a 2-way table where the lines correspond to the states at the beginning of a cycle and the columns to the states at the end of a cycle. For example consider a model with 2 states `A` and `B`:

|       | A | B |
|-------|---|---|
| **A** | 1 | 2 |
| **B** | 3 | 4 |

When starting a cycle in state `A` (row `A`), the probability to still be in state `A` at the end of the cycle is found in colunm `A` (row `1`) and the probability to change to state `B` is found in column `B` (cell `2`).

Similarly, when starting a cycle from state `B` (row `B`), the probability to be in state `A` or `B` at the end of the cycle are found in cells `3` or `4` respectively.

In the context of Markov models, this 2-way table is called a *transition matrix*. A transition matrix can be defined easily in `heemod` with the `define_matrix` function. If we consider the previous example, where cell values have been replaced by actual probabilities:

|       |  A  |  B  |
|-------|-----|-----|
| **A** | 0.9 | 0.1 |
| **B** | 0.2 | 0.8 |

This transition matrix can be defined with the following command:


```r
mat_trans <- define_matrix(
  .9, .1,
  .2, .8
)
mat_trans
```

```
## An unevaluated matrix, 2 states.
## 
##   A   B  
## A 0.9 0.1
## B 0.2 0.8
```

# Attach values to states

In health economic evaluation, values are attached to states. Cost and utility are classical examples of such values. To continue with the previous example, the following values can be attachd to state `A` and `B`:

  * State `A` has a cost of 1234 per cycle and an utility of 0.85.
  * State `B` has a cost of 4321 per cycle and an utility of 0.50.
  
A state and its values can be defined with `define_state`:


```r
state_A <- define_state(
  cost = 1234,
  utility = 0.85
)
state_A
```

```
## An unevaluated state with 2 values.
## 
## cost = 1234
## utility = 0.85
```

```r
state_B <- define_state(
  cost = 4321,
  utility = 0.50
)
state_B
```

```
## An unevaluated state with 2 values.
## 
## cost = 4321
## utility = 0.5
```

In order to specify that both states belong to the same model they must be combined together with `define_state_list`:


```r
state_list <- define_state_list(
  state_A,
  state_B
)
```

```
## No named state -> generating names.
```

```r
state_list
```

```
## A list of 2 unevaluated states with 2 values each.
## 
## State names:
## 
## A
## B
## 
## State values:
## 
## cost
## utility
```

# Combine information in a model

Now that the transition matrix and the state values are defined, we can combine them to create a Markov model with `define_model`:


```r
mod_1 <- define_model(
  transition_matrix = mat_trans,
  states = state_list
)
mod_1
```

```
## An unevaluated Markov model:
## 
##     0 parameter,
##     2 states,
##     2 state values.
```

# Run a model

The model can then be run with `run_model` for a given number of cycles:


```r
res_mod_1 <- run_model(
  mod_1,
  cycles = 5
)
```

```
## No named model -> generating names.
```

```r
res_mod_1
```

```
## 1 Markov model, run for 5 cycles.
## 
## Model names:
## 
## A
```

The result can be explored with `summary`:


```r
summary(res_mod_1)
```

```
## 1 Markov model run for 5 cycles.
## 
## Initial states:
## 
##   N
## A 1
## B 0
##       cost  utility
## A 9317.536 3.893137
```

By default the model is run for one person starting in the first state (here state `A`).
