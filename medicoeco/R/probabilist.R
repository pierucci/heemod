simulate_cohort_iter <- function(model, cycles,
                                 init, newdata) {
  
  eval_newdata <- function(new_params, model) {
    
    model$parameters <- modifyList(
      get_parameters(model),
      do.call(lazyeval::lazy_dots, new_params)
    )
    simulate_cohort(
      model = model,
      cycles = cycles,
      init = init
    )
  }
  tab %>%
    rowwise %>%
    do(res = eval_newdata(., model)) %>%
    bind_cols(newdata)
}
