compute_cov <- function(psa, diff = FALSE, k, k_default = 10, threshold) {
  if (! requireNamespace("mgcv")) {
    stop("'mgcv' package required for covariance analysis.")
  }
  
  if (diff) {
    tab_psa <- psa$psa %>%
      dplyr::group_by(.data$.index) %>%
      dplyr::do(compute_icer(
        strategy_order = order(get_effect(get_model(psa))),
        threshold = threshold)) %>%
      dplyr::filter(!is.na(.dref)) %>% 
      dplyr::ungroup()
  } else {
    tab_psa <- psa$psa
  }
  
  max_k <- tab_psa %>% 
    dplyr::select(psa$resamp_par, .data$.strategy_names) %>% 
    dplyr::group_by(.data$.strategy_names) %>% 
    dplyr::summarise_all(dplyr::n_distinct) %>% 
    dplyr::summarise_all(min) %>% 
    dplyr::select(-.data$.strategy_names) %>% 
    unlist()
  
  default_k <- ifelse(
    max_k < k_default,
    max_k, k_default
  )
  
  if (! missing(k)) {
    stopifnot(all(names(k) %in% psa$resamp_par))
    if (any(pb <- default_k[names(k)] < k)) {
      warning(sprintf(
        "Number of distinct values < k: %s.",
        paste(names(k)[pb], collapse = ", ")
      ))
    }
    default_k[names(k)] <- k
  }
  
  if (sum(default_k) >= psa$N) {
    warning(sprintf(
      "Not enough PSA data (%i present, at least %i needed). Consider lowering 'k_default' of running more simulations.",
      psa$N, sum(default_k)
    ))
  }
  
  x_side <- paste(
    sprintf("s(%s, k = %i)", psa$resamp_par, default_k),
    collapse = "+")
  
  form_cost <- stats::as.formula(paste(
    ".cost ~", x_side
  ))
  form_effect <- stats::as.formula(paste(
    ".effect ~", x_side
  ))
  
  if (diff) {
    form_cost <- stats::as.formula(paste(
      ".dcost ~", x_side
    ))
    form_effect <- stats::as.formula(paste(
      ".deffect ~", x_side
    ))
    
    form_nmb <- stats::as.formula(paste(
      ".dnmb ~", x_side
    ))
  }
  
  res <- tab_psa %>% 
    dplyr::group_by(.data$.strategy_names) %>% 
    dplyr::do(
      compute_prop_var(mgcv::gam(formula = form_cost, data = .))
    ) %>% 
    dplyr::mutate(
      .result = "Cost"
    ) %>% 
    dplyr::bind_rows(
      tab_psa %>% 
        dplyr::group_by(.data$.strategy_names) %>% 
        dplyr::do(
          compute_prop_var(mgcv::gam(formula = form_effect, data = .))
        ) %>% 
        dplyr::mutate(
          .result = "Effect"
        )
    )
  
  if (diff) {
    res <- res %>% 
      dplyr::bind_rows(
        tab_psa %>% 
          dplyr::group_by(.data$.strategy_names) %>% 
          dplyr::do(
            compute_prop_var(mgcv::gam(formula = form_nmb, data = .))
          ) %>% 
          dplyr::mutate(
            .result = "NMB"
          )
      )
  }
  
  res %>% 
    dplyr::ungroup() %>% 
    reshape_long(
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
  
  if (any(is.na(val))) {
    warning(sprintf(
      "Parameter%s excluded because of perfect collinearity: %s.",
      plur(sum(is.na(val))),
      paste(names(val)[is.na(val)], collapse = ", ")
    ))
    val[is.na(val)] <- 0
  }
  
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
