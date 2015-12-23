# Time-varying Markov models (non-homogenous)
`r Sys.Date()`  



In more complex Markov models in health economic evaluation transition probabilities between states can vary with time. These models are called *non-homogenous* or *time-inhomogenous* Markov models. A further distinction can be made depending on whether:

  1. transition probabilities depend on how long __the entire model__ has been running (model-time variation): this situation can be modelled with a non-homogenous Markov model;
  2. transition probabilities depend on how long __an individual__ has been in a state (individual-time variation): this situation usually needs to be modelled with a microsimulation.

However in some special cases non-homogenous Markov models can be used in situation 2, when the state with transition probabilities depending on individual-time is the starting state __and__ it is not possible to go back to that state after having left it. In this situation individual-time is equivalent to model-time. This is the case in the following example.

If you are not familiar with `heemod`, first consult the introduction vignette`vignette("introduction", package = "heemod")`.

# Model description

This example is an implementation of the assessment of a new total hip replacement (THR) technology described in chapter 3.5 of [Decision Modelling for Health Economic Evaluation](http://ukcatalogue.oup.com/product/9780198526629.do). A more detailed report is available [at this location](https://www.york.ac.uk/media/che/documents/papers/technicalpapers/CHE%20Technical%20Paper%2028.pdf), event though this reports goes a bit further in the analysis.

This model has 5 states:

  * Primary THR: starting state, individuals receive either the standard or the new THR (called NP1), outcomes are either success of primary THR or death (the operative mortality rate of primary THR will be called `omrPTHR`);
  * Success of primary THR: state after receiving primary THR if the surgery is successfull, individuals can stay in this state, need a THR revision, or die of other causes;
  * Revision of primary THR: for individuals whose primary THR needed revision, outcomes are either success of revision THR or death (the operative mortality rate of revision THR will be called `omrRTHR`);
  * Success of revision THR: state after receiving revision THR if the surgery is successfull, individuals can stay in this state, need a THR re-revision, or die of other causes;
  * Death (either caused by THR or another cause).
  
Two transition probabilities are time-varying in this model:

  * Probability of death by another cause increases with age;
  * Probability of primary THR revision increases with time.
  
Other-cause death probabilities (mortality rate `mr`) vary with age and gender with the following values:


```r
death_prob <- data.frame(
  age = rep(seq(35, 85, 10), each = 2),
  sex = rep(0:1, 6),
  mr = c(
    1.51e-3, .99e-3, 3.93e-3,
    2.6e-3, 10.9e-3, 6.7e-3,
    31.6e-3, 19.3e-3, 80.1e-3,
    53.5e-3, 187.9e-3, 154.8e-3
  )
)
death_prob
```

```
##    age sex      mr
## 1   35   0 0.00151
## 2   35   1 0.00099
## 3   45   0 0.00393
## 4   45   1 0.00260
## 5   55   0 0.01090
## 6   55   1 0.00670
## 7   65   0 0.03160
## 8   65   1 0.01930
## 9   75   0 0.08010
## 10  75   1 0.05350
## 11  85   0 0.18790
## 12  85   1 0.15480
```

Primary THR revision probability increases with time with the following formula (a Weibull distribution):

$$
P_{revision} = 1 - \exp(\lambda \times ((t-1)^\gamma-t^\gamma))
$$

Where $t$ is the time since revision, $\gamma = 1.45367786$ and:

$$
\lambda = exp(cons + ageC \times age + maleC \times sex)
$$

Where `age` and `sex` (female = 0, male = 1) are individual characteristics, `cons` = -5.49094, `ageC` = -0.0367 and `maleC` = 0.768536.

For the NP1 procedure the revision probability is modified by the relative risk `rrNP1` = 0.260677.

$$
P_{revision} = 1 - \exp(\lambda \times rrNP1 \times ((t-1)^\gamma-t^\gamma))
$$

Revision THR re-revision (`rrr`) probability is set to be constant at 0.04 per year.

# Parameters definition

The key element to specify time-varying elements in `heemod` is through the use of a package-defined variable, `markov_model`. This variable takes increasing values with each cycles, starting from 1. For example the age of individuals at any moment can be defined as `Initial age + markov_cycle`.

In order to build this more complex Markov model, parameters need to be defined through `define_parameters`. The equations decribed in the previous section can be written easily, here for a female population (`sex` = 0) starting at 60 years old (`age_init` = 60):


```r
# a function to return mr, given age and sex
mr_func <- function(age, sex) {
  age  <- floor(age/10-.5)*10+5
  age <- ifelse(age > 85, 85, age)
  merge(data.frame(age = age, sex = sex), death_prob)$mr
}
param_standard <- define_parameters(
    t = markov_cycle,
    
    age_init = 60,
    sex = 0,
    age = age_init + t,
    
    omrPTHR = .02,
    omrRTHR = .02,
    rrr = .04,
    
    cons = -5.49094,
    ageC = -.0367,
    maleC = .768536,
    lambda = exp(cons+ageC*age_init+maleC*sex),
    gamma = 1.45367786,
    
    rrNP1 = .260677,
    
    standardRR = 1 - exp(lambda*((t-1)^gamma-t^gamma)),
    
    mr = mr_func(age, sex)
)
param_standard
```

```
## 15 unevaluated parameters.
## 
## t = markov_cycle
## age_init = 60
## sex = 0
## age = age_init + t
## omrPTHR = 0.02
## omrRTHR = 0.02
## rrr = 0.04
## cons = -5.49094
## ageC = -0.0367
## maleC = 0.768536
## lambda = exp(cons + ageC * age_init + maleC * sex)
## gamma = 1.45367786
## rrNP1 = 0.260677
## standardRR = 1 - exp(lambda * ((t - 1)^gamma - t^gamma))
## mr = mr_func(age, sex)
```

The parameters for the NP1 group are almost the same as for the standard group, with the exception of the primary THR revision probability formula. Parameters can by modified through the function `modify`:


```r
param_np1 <- modify(
  param_standard,
  standardRR = 1 - exp(lambda*rrNP1*((t-1)^gamma-t^gamma))
)
param_np1
```

```
## 15 unevaluated parameters.
## 
## t = markov_cycle
## age_init = 60
## sex = 0
## age = age_init + t
## omrPTHR = 0.02
## omrRTHR = 0.02
## rrr = 0.04
## cons = -5.49094
## ageC = -0.0367
## maleC = 0.768536
## lambda = exp(cons + ageC * age_init + maleC * sex)
## gamma = 1.45367786
## rrNP1 = 0.260677
## standardRR = 1 - exp(lambda * rrNP1 * ((t - 1)^gamma - t^gamma))
## mr = mr_func(age, sex)
```

# Transition matrix definition

Now that parameters are defined, the probability transitions can be easily written:


```r
mat_trans <- define_matrix(
    state_names = c(
      "PrimaryTHR",
      "SuccessP",
      "RevisionTHR",
      "SuccessR",
      "Death"
    ),
    0, 1-omrPTHR,         0,          0,          omrPTHR,
    0, 1-(standardRR+mr), standardRR, 0,          mr,
    0, 0,                 0, 1-(omrRTHR+mr), omrRTHR+mr,
    0, 0,                 rrr,        1-(mr+rrr), mr,
    0, 0,                 0,          0,          1
)
mat_trans
```

```
## An unevaluated matrix, 5 states.
## 
##             PrimaryTHR SuccessP              RevisionTHR
## PrimaryTHR  0          1 - omrPTHR           0          
## SuccessP    0          1 - (standardRR + mr) standardRR 
## RevisionTHR 0          0                     0          
## SuccessR    0          0                     rrr        
## Death       0          0                     0          
##             SuccessR           Death       
## PrimaryTHR  0                  omrPTHR     
## SuccessP    0                  mr          
## RevisionTHR 1 - (omrRTHR + mr) omrRTHR + mr
## SuccessR    1 - (mr + rrr)     mr          
## Death       0                  1
```

# State definition

Utilities and costs are then associated to states. In this model costs are discounted at a rate of 6% and utilities at a rate of 1.5%.


```r
state_list <- define_state_list(
    PrimaryTHR = define_state(
      utility = 0,
      cost = 0
    ),
    SuccessP = define_state(
      utility = discount(.85, .015),
      cost = 0
    ),
    RevisionTHR = define_state(
      utility = discount(.30, .015),
      cost = discount(5294, .06)
    ),
    SuccessR = define_state(
      utility = discount(.75, .015),
      cost = 0
    ),
    Death = define_state(
      utility = 0,
      cost = 0
    )
  )
state_list
```

```
## A list of 5 unevaluated states with 2 values each.
## 
## State names:
## 
## PrimaryTHR
## SuccessP
## RevisionTHR
## SuccessR
## Death
## 
## State values:
## 
## utility
## cost
```

# Model definition

Now that parameters, transition matrix and states are defined we can define the models for the control group and the NP1 treatment:


```r
mod_standard <- define_model(
  parameters = param_standard,
  transition_matrix = mat_trans,
  states = state_list
)
mod_standard
```

```
## An unevaluated Markov model:
## 
##     15 parameters,
##     5 states,
##     2 state values.
```

```r
mod_np1 <- define_model(
  parameters = param_np1,
  transition_matrix = mat_trans,
  states = state_list
)
```

# Analyse models

Both models can now be run for 60 years, for 1 person starting in `PrimaryTHR`:


```r
res_mod <- run_models(
  standard = mod_standard,
  np1 = mod_np1,
  cycles = 60
)
res_mod
```

```
## 2 Markov models, run for 60 cycles.
## 
## Model names:
## 
## standard
## np1
```

A comparison of both models can be done with `summary`:


```r
summary(res_mod)
```

```
## 2 Markov models run for 60 cycles.
## 
## Initial states:
## 
##             N
## PrimaryTHR  1
## SuccessP    0
## RevisionTHR 0
## SuccessR    0
## Death       0
##           utility      cost
## standard 14.67977 221.95698
## np1      14.72271  58.89669
```

Since standard treatment costs 394 and NP1 costs 579, the ICER for a population of women aged 60 can be calculated with the following formula:

$$
\frac{(58.90 + 579) - (221.96 + 394)}{14.72 - 14.68} = 548.5
$$

The new treatment costs 548.5 GBP per QALY gained.
