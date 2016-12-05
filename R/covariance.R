compute_cov <- function(psa) {
  if (! requireNamespace("mgcv")) {
    stop("'mgcv' package required for covariance analysis.")
  }
  form_cost <- stats::as.formula(paste(
    ".cost ~", paste(sprintf("s(%s)", psa$resamp_par), collapse = "+")
  ))
  form_effect <- stats::as.formula(paste(
    ".effect ~", paste(sprintf("s(%s)", psa$resamp_par), collapse = "+")
  ))
  
  psa$psa %>% 
    dplyr::group_by_(".strategy_names") %>% 
    dplyr::do_(
      ~ compute_prop_var(mgcv::gam(formula = form_cost, data = .))
    ) %>% 
    dplyr::mutate(
      .result = "Cost"
    ) %>% 
    dplyr::bind_rows(
      psa$psa %>% 
        dplyr::group_by_(".strategy_names") %>% 
        dplyr::do_(
          ~ compute_prop_var(mgcv::gam(formula = form_effect, data = .))
        ) %>% 
        dplyr::mutate(
          .result = "Effect"
        )
    ) %>% 
    dplyr::ungroup() %>% 
    tidyr::gather_(
      key_col = ".par_names",
      value_col = ".prop",
      gather_cols = psa$resamp_par
    )
}

compute_prop_var <- function(mod) {
  n <- attr(mod$terms, "term.labels")
  if (identical(0, stats::var(mod$y))) {
    return(
      rep(0, length(n)) %>% 
        stats::setNames(n) %>% 
        as.list() %>% 
        as.data.frame()
    )
  }
  
  data_trans <- mgcv::predict.gam(mod, type = "terms") %>% 
    cbind(y = mod$y) %>% 
    scale() %>% 
    as.data.frame() %>% 
    stats::setNames(c(n, "y"))
  
  form <- stats::as.formula(paste(
    "y ~", paste(n, collapse = "+")
  ))
  res <- stats::lm(form, data = data_trans)
  
  val <- abs(stats::coef(res)[-1])
  tot <- sum(val)
  r2 <- summary(res)$r.squared
  if (r2 < .99) {
    warning(sprintf(
      "Only %.0f%% of variance explained, results may be inaccurate.",
      r2 * 100
    ))
  }
  as.data.frame(as.list(val / tot * r2))
}
