context("Evaluation of survival functions")

fs1 = flexsurv::flexsurvreg(survival::Surv(rectime, censrec)~group, data=flexsurv::bc, dist="weibull")
fs2 = flexsurv::flexsurvreg(survival::Surv(rectime, censrec)~group, data=flexsurv::bc, dist="weibullPH")
fs3 = flexsurv::flexsurvspline(survival::Surv(rectime, censrec)~group, data=flexsurv::bc, scale="odds", k=2)
fs4 = flexsurv::flexsurvreg(survival::Surv(rectime, censrec)~group, data=flexsurv::bc, dist="exp")

cox = survival::coxph(survival::Surv(rectime,censrec)~group, data=flexsurv::bc)
cox_bl = survival::survfit(cox)

km = survival::survfit(survival::Surv(rectime,censrec)~group, data=flexsurv::bc)

test_that(
  "Applying treatment effects",
  {
  
  # Testing apply_hr, apply_af, apply_or against flexsurvreg output to see
  # that it is consitent.
  
  surv1_medium_surv = fs1 %>%
    set_covariates(group="Medium") %>%
    eval_surv(cycle=seq_len(10),cycle_length=200, type="surv")
  
  surv1_medium_aft_surv = fs1 %>%
    set_covariates(group="Good") %>%
    apply_af(fs1$coefficients[3], log.af=T) %>%
    eval_surv(cycle=seq_len(10),cycle_length=200, type="surv")
  
  surv1_poor_surv = fs1 %>%
    set_covariates(group="Poor") %>%
    eval_surv(cycle=seq_len(10),cycle_length=200, type="surv")
  
  surv1_poor_aft_surv = fs1 %>%
      set_covariates(group="Good") %>%
      apply_af(fs1$coefficients[4], log.af=T) %>%
      eval_surv(cycle=seq_len(10),cycle_length=200, type="surv")
  
  
  surv1_medium_prob = fs1 %>%
    set_covariates(group="Medium") %>%
    eval_surv(cycle=seq_len(10),cycle_length=200, type="prob")
  
  surv1_medium_aft_prob = fs1 %>%
    set_covariates(group="Good") %>%
    apply_af(fs1$coefficients[3], log.af=T) %>%
    eval_surv(cycle=seq_len(10),cycle_length=200, type="prob")
  
  surv1_poor_prob = fs1 %>%
    set_covariates(group="Poor") %>%
    eval_surv(cycle=seq_len(10),cycle_length=200, type="prob")
  
  surv1_poor_aft_prob = fs1 %>%
    set_covariates(group="Good") %>%
    apply_af(fs1$coefficients[4], log.af=T) %>%
    eval_surv(cycle=seq_len(10),cycle_length=200, type="prob")
  
  
  
  surv2_medium_surv = fs2 %>%
    set_covariates(group="Medium") %>%
    eval_surv(cycle=seq_len(10),cycle_length=200, type="surv")
  
  surv2_medium_hr_surv = fs2 %>%
    set_covariates(group="Good") %>%
    apply_hr(fs2$coefficients[3], log.hr=T) %>%
    eval_surv(cycle=seq_len(10),cycle_length=200, type="surv")
  
  surv2_poor_surv = fs2 %>%
    set_covariates(group="Poor") %>%
    eval_surv(cycle=seq_len(10),cycle_length=200, type="surv")
  
  surv2_poor_hr_surv = fs2 %>%
    set_covariates(group="Good") %>%
    apply_hr(fs2$coefficients[4], log.hr=T) %>%
    eval_surv(cycle=seq_len(10),cycle_length=200, type="surv")
  
  surv2_medium_prob = fs2 %>%
    set_covariates(group="Medium") %>%
    eval_surv(cycle=seq_len(10),cycle_length=200, type="prob")
  
  surv2_medium_hr_prob = fs2 %>%
    set_covariates(group="Good") %>%
    apply_hr(fs2$coefficients[3], log.hr=T) %>%
    eval_surv(cycle=seq_len(10),cycle_length=200, type="prob")
  
  surv2_poor_prob = fs2 %>%
    set_covariates(group="Poor") %>%
    eval_surv(cycle=seq_len(10),cycle_length=200, type="prob")
  
  surv2_poor_hr_prob = fs2 %>%
    set_covariates(group="Good") %>%
    apply_hr(fs2$coefficients[4], log.hr=T) %>%
    eval_surv(cycle=seq_len(10),cycle_length=200, type="prob")
  
  
  
  surv3_medium_surv = fs3 %>%
    set_covariates(group="Medium") %>%
    eval_surv(cycle=seq_len(10),cycle_length=200, type="surv")
  
  surv3_medium_or_surv = fs3 %>%
    set_covariates(group="Good") %>%
    apply_or(fs3$coefficients[5], log.or=T) %>%
    eval_surv(cycle=seq_len(10),cycle_length=200, type="surv")
  
  surv3_poor_surv = fs3 %>%
    set_covariates(group="Poor") %>%
    eval_surv(cycle=seq_len(10),cycle_length=200, type="surv")
  
  surv3_poor_or_surv = fs3 %>%
    set_covariates(group="Good") %>%
    apply_or(fs3$coefficients[6], log.or=T) %>%
    eval_surv(cycle=seq_len(10),cycle_length=200, type="surv")
  
  surv3_medium_prob = fs3 %>%
    set_covariates(group="Medium") %>%
    eval_surv(cycle=seq_len(10),cycle_length=200, type="prob")
  
  surv3_medium_or_prob = fs3 %>%
    set_covariates(group="Good") %>%
    apply_or(fs3$coefficients[5], log.or=T) %>%
    eval_surv(cycle=seq_len(10),cycle_length=200, type="prob")
  
  surv3_poor_prob = fs3 %>%
    set_covariates(group="Poor") %>%
    eval_surv(cycle=seq_len(10),cycle_length=200, type="prob")
  
  surv3_poor_or_prob = fs3 %>%
    set_covariates(group="Good") %>%
    apply_or(fs3$coefficients[6], log.or=T) %>%
    eval_surv(cycle=seq_len(10),cycle_length=200, type="prob")
  
  # Test acceleration
  expect_equal(surv1_medium_surv,surv1_medium_aft_surv)
  expect_equal(surv1_poor_surv, surv1_poor_aft_surv)
  expect_equal(surv1_medium_prob,surv1_medium_aft_prob)
  expect_equal(surv1_poor_prob, surv1_poor_aft_prob)
  
  # Test Proportional Hazards
  expect_equal(surv2_medium_surv,surv2_medium_hr_surv)
  expect_equal(surv2_poor_surv, surv2_poor_hr_surv)
  expect_equal(surv2_medium_prob,surv2_medium_hr_prob)
  expect_equal(surv2_poor_prob, surv2_poor_hr_prob)
  
  # Test Proportional Odds
  expect_equal(surv3_medium_surv,surv3_medium_or_surv)
  expect_equal(surv3_poor_surv, surv3_poor_or_surv)
  expect_equal(surv3_medium_prob,surv3_medium_or_prob)
  expect_equal(surv3_poor_prob, surv3_poor_or_prob)
}
)

test_that(
  "Defining Survial Distributions",
  {
    surv1 = define_survival(
      dist = "weibull",
      shape = 1.3797,
      scale = 4169.3446
    )
    
    surv2 = define_survival(
      dist = "weibullPH",
      shape = 1.38e+00,
      scale = 1.01e-05
    )
    
    surv3 = define_spline_survival(
      scale = "odds",
      gamma = c(-23.5136, 3.4483, 0.4873, -0.3147),
      knots = c(4.276666, 6.219263, 6.771924, 7.806289)
    )
    
    surv1_surv = surv1 %>%
      eval_surv(cycle=seq_len(10), cycle_length=200)
    fs1_surv = fs1 %>%
      set_covariates(group = "Good") %>%
      eval_surv(cycle=seq_len(10), cycle_length=200)
    
    surv1_prob = surv1 %>%
      eval_surv(cycle=seq_len(10), cycle_length=200)
    fs1_prob = fs1 %>%
      set_covariates(group = "Good") %>%
      eval_surv(cycle=seq_len(10), cycle_length=200)
    
    surv2_surv = surv2 %>%
      eval_surv(cycle=seq_len(10), cycle_length=200)
    fs2_surv = fs2 %>%
      set_covariates(group = "Good") %>%
      eval_surv(cycle=seq_len(10), cycle_length=200)
    
    surv2_prob = surv2 %>%
      eval_surv(cycle=seq_len(10), cycle_length=200)
    fs2_prob = fs2 %>%
      set_covariates(group = "Good") %>%
      eval_surv(cycle=seq_len(10), cycle_length=200)
    
    surv3_surv = surv3 %>%
      eval_surv(cycle=seq_len(10), cycle_length=200)
    fs3_surv = fs3 %>%
      set_covariates(group = "Good") %>%
      eval_surv(cycle=seq_len(10), cycle_length=200)
    
    surv3_prob = surv3 %>%
      eval_surv(cycle=seq_len(10), cycle_length=200)
    fs3_prob = fs3 %>%
      set_covariates(group = "Good") %>%
      eval_surv(cycle=seq_len(10), cycle_length=200)
    
    # Survival from flexsurv should equal equivalent from
    # define_survival
    expect_equal(surv1_surv, fs1_surv, tolerance=1E-4)
    expect_equal(surv1_prob, fs1_prob, tolerance=1E-4)
    
    expect_equal(surv2_surv, fs2_surv, tolerance=1E-4)
    expect_equal(surv2_prob, fs2_prob, tolerance=1E-4)
    
    expect_equal(surv3_surv, fs3_surv, tolerance=1E-4)
    expect_equal(surv3_prob, fs3_prob, tolerance=1E-4)
  }
)

test_that(
  "Combining Survival Distributions",
  {
    
      # Projecting a distribution w/ itself should not
      # change survival
      exp_surv = fs4 %>%
        set_covariates(group="Poor") %>%
        eval_surv(cycle=seq_len(10), cycle_length=200)
      exp_surv2 = fs4 %>%
        set_covariates(group="Poor") %>%
        project(fs4 %>% set_covariates(group="Poor"), at = 543.343) %>%
        eval_surv(cycle=seq_len(10), cycle_length=200)
      
      expect_equal(exp_surv, exp_surv2)
      
      # Pooling a distribution w/ itself should not
      # change survival
      exp_sur3 = fs4 %>%
        set_covariates(group="Poor") %>%
        eval_surv(cycle=seq_len(10), cycle_length=200)
      exp_surv4 = fs4 %>%
        set_covariates(group="Poor") %>%
        pool(fs4 %>% set_covariates(group="Poor"), weights = c(0.5, 0.5)) %>%
        eval_surv(cycle=seq_len(10), cycle_length=200)
      
      # Projecting + Pooling w/ self, applying null
      # treatment effects should not change survival
      exp_sur5 = fs4 %>%
        set_covariates(group="Poor") %>%
        eval_surv(cycle=seq_len(10), cycle_length=200)
      exp_surv6 = fs4 %>%
        set_covariates(group="Poor") %>%
        pool(fs4 %>% set_covariates(group="Poor"), weights = c(0.5, 0.5)) %>%
        apply_hr(1) %>%
        project(fs4 %>% set_covariates(group="Poor"), at = 89.1) %>%
        apply_af(1) %>%
        apply_or(1) %>%
        eval_surv(cycle=seq_len(10), cycle_length=200)
      
      expect_equal(exp_sur5, exp_surv6)
  }
)