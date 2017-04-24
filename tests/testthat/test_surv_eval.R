context("Evaluation of survival functions")

fs1 = flexsurv::flexsurvreg(
  survival::Surv(rectime, censrec)~group,
  data=flexsurv::bc,
  dist="weibull"
)
fs2 = flexsurv::flexsurvreg(
  survival::Surv(rectime, censrec)~group,
  data=flexsurv::bc,
  dist="weibullPH"
)
fs3 = flexsurv::flexsurvspline(
  survival::Surv(rectime, censrec)~group, 
  data=flexsurv::bc,
  scale="odds",
  k=2
)
fs4 = flexsurv::flexsurvreg(
  survival::Surv(rectime, censrec)~group,
  data=flexsurv::bc,
  dist="exp"
)
fs5 = flexsurv::flexsurvreg(
  survival::Surv(rectime, censrec)~1,
  data=flexsurv::bc,
  dist="genf"
)
fs6 = flexsurv::flexsurvreg(
  survival::Surv(time, status==1)~age+sex,
  data=survival::cancer,
  dist="gompertz"
)
cox = survival::coxph(
  survival::Surv(rectime,censrec)~group,
  data=flexsurv::bc
)
cox_bl = survival::survfit(cox)

km = survival::survfit(survival::Surv(rectime,censrec)~group, data=flexsurv::bc)
km_good = survival::survfit(
  survival::Surv(rectime,censrec)~group,
  data=flexsurv::bc %>% dplyr::filter(group=="Good")
)

test_that(
  "Flexsurvreg",
  {
    heemod_res = fs6 %>%
      set_covariates(age=50, sex=1) %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="surv")
    
    fs_res = summary(
      fs6,
      t=seq(from=200,to=2000,by=200),
      type="survival",
      ci=F,
      newdata=data.frame(age=50, sex=1)
    )[[1]]$est
    
    expect_equal(heemod_res, fs_res)
  }
)

test_that(
  "Applying treatment effects",
  {
    
    # Testing apply_hr, apply_af, apply_or and apply_shift
    # against flexsurvreg output to see that it is consistent.
    
    surv1_medium_surv = fs1 %>%
      set_covariates(group="Medium") %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="surv")
    
    surv1_medium_aft_surv = fs1 %>%
      set_covariates(group="Good") %>%
      apply_af(fs1$coefficients[3], log_af=T) %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="surv")
    
    surv1_poor_surv = fs1 %>%
      set_covariates(group="Poor") %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="surv")
    
    surv1_poor_aft_surv = fs1 %>%
      set_covariates(group="Good") %>%
      apply_af(fs1$coefficients[4], log_af=T) %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="surv")
    
    surv1_poor_shift_surv = fs1 %>%
      set_covariates(group = "Poor") %>%
      apply_shift(shift = 800) %>%
      compute_surv(time = seq_len(10), cycle_length = 200, type = "surv")
    
    surv1_medium_prob = fs1 %>%
      set_covariates(group="Medium") %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="prob")
    
    surv1_medium_aft_prob = fs1 %>%
      set_covariates(group="Good") %>%
      apply_af(fs1$coefficients[3], log_af=T) %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="prob")
    
    surv1_poor_prob = fs1 %>%
      set_covariates(group="Poor") %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="prob")
    
    surv1_poor_aft_prob = fs1 %>%
      set_covariates(group="Good") %>%
      apply_af(fs1$coefficients[4], log_af=T) %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="prob")
    
    
    
    surv2_medium_surv = fs2 %>%
      set_covariates(group="Medium") %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="surv")
    
    surv2_medium_hr_surv = fs2 %>%
      set_covariates(group="Good") %>%
      apply_hr(fs2$coefficients[3], log_hr=T) %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="surv")
    
    surv2_poor_surv = fs2 %>%
      set_covariates(group="Poor") %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="surv")
    
    surv2_poor_hr_surv = fs2 %>%
      set_covariates(group="Good") %>%
      apply_hr(fs2$coefficients[4], log_hr=T) %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="surv")
    
    surv2_medium_prob = fs2 %>%
      set_covariates(group="Medium") %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="prob")
    
    surv2_medium_hr_prob = fs2 %>%
      set_covariates(group="Good") %>%
      apply_hr(fs2$coefficients[3], log_hr=T) %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="prob")
    
    surv2_poor_prob = fs2 %>%
      set_covariates(group="Poor") %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="prob")
    
    surv2_poor_hr_prob = fs2 %>%
      set_covariates(group="Good") %>%
      apply_hr(fs2$coefficients[4], log_hr=T) %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="prob")
    
    
    
    surv3_medium_surv = fs3 %>%
      set_covariates(group="Medium") %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="surv")
    
    surv3_medium_or_surv = fs3 %>%
      set_covariates(group="Good") %>%
      apply_or(fs3$coefficients[5], log_or=T) %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="surv")
    
    surv3_poor_surv = fs3 %>%
      set_covariates(group="Poor") %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="surv")
    
    surv3_poor_or_surv = fs3 %>%
      set_covariates(group="Good") %>%
      apply_or(fs3$coefficients[6], log_or=T) %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="surv")
    
    surv3_medium_prob = fs3 %>%
      set_covariates(group="Medium") %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="prob")
    
    surv3_medium_or_prob = fs3 %>%
      set_covariates(group="Good") %>%
      apply_or(fs3$coefficients[5], log_or=T) %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="prob")
    
    surv3_poor_prob = fs3 %>%
      set_covariates(group="Poor") %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="prob")
    
    surv3_poor_or_prob = fs3 %>%
      set_covariates(group="Good") %>%
      apply_or(fs3$coefficients[6], log_or=T) %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="prob")
    
    # Test acceleration
    expect_equal(surv1_medium_surv,surv1_medium_aft_surv)
    expect_equal(surv1_poor_surv, surv1_poor_aft_surv)
    expect_equal(surv1_medium_prob,surv1_medium_aft_prob)
    expect_equal(surv1_poor_prob, surv1_poor_aft_prob)
    
    expect_identical(surv1_poor_surv, 
                     apply_af(apply_af(surv1_poor_surv, 2),
                              1/2)
    )
    expect_identical(surv1_poor_surv, 
                     apply_af(apply_af(surv1_poor_surv, 1/2),
                              2)
    )
    expect_identical(apply_af(surv1_poor_surv, 2),
                     apply_af(apply_af(surv1_poor_surv, 4),
                              1/2)
    )
    
    # Test Proportional Hazards
    expect_equal(surv2_medium_surv,surv2_medium_hr_surv)
    expect_equal(surv2_poor_surv, surv2_poor_hr_surv)
    expect_equal(surv2_medium_prob,surv2_medium_hr_prob)
    expect_equal(surv2_poor_prob, surv2_poor_hr_prob)
    
    expect_identical(fs1, 
                     apply_hr(apply_hr(fs1, 2),
                              1/2)
    )
    expect_identical(fs1, 
                     apply_hr(apply_hr(fs1, 1/2),
                              2)
    )
    expect_identical(apply_hr(fs1, 2),
                     apply_hr(apply_hr(fs1, 4),
                              1/2)
    )
    
    # Test Proportional Odds
    expect_equal(surv3_medium_surv,surv3_medium_or_surv)
    expect_equal(surv3_poor_surv, surv3_poor_or_surv)
    expect_equal(surv3_medium_prob,surv3_medium_or_prob)
    expect_equal(surv3_poor_prob, surv3_poor_or_prob)
    
    expect_identical(fs1, 
                     apply_or(apply_or(fs1, 2),
                                 1/2)
    )
    expect_identical(fs1, 
                     apply_or(apply_or(fs1, 1/2),
                                 2)
    )
    expect_identical(apply_or(fs1, 2),
                     apply_or(apply_or(fs1, 4),
                                 1/2)
    )
    
    
    # Test shifts
    expect_equal(surv1_poor_surv[1:6], surv1_poor_shift_surv[5:10])
    expect_equal(length(surv1_poor_surv), length(surv1_poor_shift_surv))
    expect_identical(surv1_poor_surv, 
                     apply_shift(apply_shift(surv1_poor_surv, 5),
                                 -5)
    )
    expect_identical(fs1, 
                     apply_shift(apply_shift(fs1, -3),
                                 3)
    )
    expect_identical(apply_shift(fs1, 2),
                     apply_shift(apply_shift(fs1, 5),
                                 -3)
    )
    expect_identical(surv1_poor_shift_surv[1:3], 
                     rep(1, 3))
    
    ## Test combinations
    fsm = fs5 %>% set_covariates(group = "Medium")
    fsm_changes = fsm %>%
      apply_shift(5) %>% apply_hr(0.5) %>% apply_shift(-5) %>% apply_hr(2) 
    
    fsm_survs <- fsm %>%
      compute_surv(time=seq_len(10),cycle_length=200, type="surv")
    fsm_changes_survs = fsm_changes %>% 
      compute_surv(time=seq_len(10),cycle_length=200, type="surv")
    expect_equal(fsm_survs, fsm_changes_survs)
   
    fsm_changes = fsm %>% 
      apply_shift(5) %>% apply_hr(0.5) %>% apply_af(0.5) %>% 
        apply_af(2) %>% apply_shift(-5) %>% apply_hr(2)    
    fsm_changes_survs = fsm_changes %>% 
      compute_surv(time=seq_len(10),cycle_length=200, type="surv")
    expect_equal(fsm_survs, fsm_changes_survs)
    
    fsm_changes = fsm %>%
      apply_shift(5) %>% apply_or(0.5) %>% apply_shift(-5) %>% apply_or(2) 
    fsm_changes_survs = fsm_changes %>% 
      compute_surv(time=seq_len(10),cycle_length=200, type="surv")
    expect_equal(fsm_survs, fsm_changes_survs)
    
    ## misaligned shifts
    surv1_poor_shift_surv_misaligned1 = fs1 %>%
      set_covariates(group = "Poor") %>%
      apply_shift(shift = 100) %>%
      compute_surv(time = seq_len(10), 
                   cycle_length = 300, type = "surv")
    
    surv1_poor_shift_surv_misaligned2 = fs1 %>%
      set_covariates(group = "Poor") %>%
      compute_surv(time = seq_len(30), 
                   cycle_length = 100, type = "surv") %>%
      .[seq_len(10)*3 - 1]
    
    expect_equal(surv1_poor_shift_surv_misaligned1, 
                 surv1_poor_shift_surv_misaligned2)
    
    
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
    
    surv5 = define_survival(
      dist = "genf",
      mu = 6.96387,
      sigma = 1.17338,
      Q = -1.08049,
      P = 0.33090
    )
    
    surv1_surv = surv1 %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    fs1_surv = fs1 %>%
      set_covariates(group = "Good") %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    
    surv1_prob = surv1 %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    fs1_prob = fs1 %>%
      set_covariates(group = "Good") %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    
    surv2_surv = surv2 %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    fs2_surv = fs2 %>%
      set_covariates(group = "Good") %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    
    surv2_prob = surv2 %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    fs2_prob = fs2 %>%
      set_covariates(group = "Good") %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    
    surv3_surv = surv3 %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    fs3_surv = fs3 %>%
      set_covariates(group = "Good") %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    
    surv3_prob = surv3 %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    fs3_prob = fs3 %>%
      set_covariates(group = "Good") %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    
    surv5_surv = surv5 %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    fs5_surv = fs5 %>%
      set_covariates(group = "Good") %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    
    surv5_prob = surv5 %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    fs5_prob = fs5 %>%
      set_covariates(group = "Good") %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    
    # Survival from flexsurv should equal equivalent from
    # define_survival
    expect_equal(surv1_surv, fs1_surv, tolerance=1E-4)
    expect_equal(surv1_prob, fs1_prob, tolerance=1E-4)
    
    expect_equal(surv2_surv, fs2_surv, tolerance=1E-4)
    expect_equal(surv2_prob, fs2_prob, tolerance=1E-4)
    
    expect_equal(surv3_surv, fs3_surv, tolerance=1E-4)
    expect_equal(surv3_prob, fs3_prob, tolerance=1E-4)
    
    expect_equal(surv5_surv, fs5_surv, tolerance=1E-4)
    expect_equal(surv5_prob, fs5_prob, tolerance=1E-4)
  }
)

test_that(
  "Survfit",
  {
    
    # Survival for a KM fit only to one group should be the same
    # as survival for all w/ covariates set to same group.
    
    km_prob = km %>%
      set_covariates(data.frame(group = "Good")) %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    
    km_good_prob = km_good %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    
    km_surv = km %>%
      set_covariates(data.frame(group = "Good")) %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    
    km_good_surv = km_good %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    
    expect_equal(km_prob, km_good_prob)
    expect_equal(km_surv, km_good_surv)
    
  }
)

test_that(
  "Combining Survival Distributions",
  {
    
    # Projecting a distribution w/ itself should not
    # change survival
    exp_surv = fs4 %>%
      set_covariates(group="Poor") %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    exp_surv2 = fs4 %>%
      set_covariates(group="Poor") %>%
      project(fs4 %>% set_covariates(group="Poor"), at = 543.343) %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    
    expect_equal(exp_surv, exp_surv2)
    
    # Pooling a distribution w/ itself should not
    # change survival
    exp_sur3 = fs4 %>%
      set_covariates(group="Poor") %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    exp_surv4 = fs4 %>%
      set_covariates(group="Poor") %>%
      pool(fs4 %>% set_covariates(group="Poor"), weights = c(0.5, 0.5)) %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    
    # Projecting + Pooling w/ self, applying null
    # treatment effects should not change survival
    exp_sur5 = fs4 %>%
      set_covariates(group="Poor") %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    exp_surv6 = fs4 %>%
      set_covariates(group="Poor") %>%
      pool(fs4 %>% set_covariates(group="Poor"), weights = c(0.5, 0.5)) %>%
      apply_hr(1) %>%
      project(fs4 %>% set_covariates(group="Poor"), at = 89.1) %>%
      apply_af(1) %>%
      apply_or(1) %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    
    expect_equal(exp_sur5, exp_surv6)
    
    # Should also work if cycle doesn't start at 1
    exp_sur7 = fs4 %>%
      set_covariates(group="Poor") %>%
      compute_surv(time=seq(from=10,to=20,by=1), cycle_length=100)
    exp_surv8 = fs4 %>%
      set_covariates(group="Poor") %>%
      pool(fs4 %>% set_covariates(group="Poor"), weights = c(0.5, 0.5)) %>%
      apply_hr(1) %>%
      project(fs4 %>% set_covariates(group="Poor"), at = 89.1) %>%
      apply_af(1) %>%
      apply_or(1) %>%
      compute_surv(time=seq(from=10,to=20,by=1), cycle_length=100)
    
    expect_equal(exp_sur7, exp_surv8)
    
    # Should also work for length 1 input
    exp_sur9 = fs4 %>%
      set_covariates(group="Poor") %>%
      compute_surv(time=25, cycle_length=365.25/7)
    exp_surv10 = fs4 %>%
      set_covariates(group="Poor") %>%
      pool(fs4 %>% set_covariates(group="Poor"), weights = c(0.5, 0.5)) %>%
      apply_hr(1) %>%
      project(fs4 %>% set_covariates(group="Poor"), at = 89.1) %>%
      apply_af(1) %>%
      apply_or(1) %>%
      compute_surv(time=25, cycle_length=365.25/7)
    
    expect_equal(exp_sur9, exp_surv10)
    
    # Adding exponential hazards to itself same as applying a HR of 2
    exp_double1_prob = fs4 %>%
      set_covariates(group="Medium") %>%
      add_hazards(.,.) %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    
    exp_double2_prob = fs4 %>%
      set_covariates(group="Medium") %>%
      apply_hr(hr=2) %>%
      compute_surv(time=seq_len(10), cycle_length=200)
    
    exp_double1_surv = fs4 %>%
      set_covariates(group="Medium") %>%
      add_hazards(.,.) %>%
      compute_surv(time=seq_len(10), cycle_length=200, type="surv")
    
    exp_double2_surv = fs4 %>%
      set_covariates(group="Medium") %>%
      apply_hr(hr=2) %>%
      compute_surv(time=seq_len(10), cycle_length=200, type="surv")
    
    expect_equal(exp_double1_prob, exp_double2_prob)
    expect_equal(exp_double1_surv, exp_double2_surv)
    
    # Running a flexsurvreg w/o specifying covariates should
    # be the same as a wieghted average of the covariate levels
    # based on distribution in original data
    fs1_weighted1_surv = fs3 %>%
      compute_surv(time=seq_len(10), cycle_length=200, type="surv")
    
    fs1_weighted2_surv = pool(
      fs3 %>% set_covariates(group="Good"),
      fs3 %>% set_covariates(group="Medium"),
      fs3 %>% set_covariates(group="Poor"),
      weights = c(229, 229, 228)
    ) %>%
      compute_surv(time=seq_len(10), cycle_length=200, type="surv")
    
    fs2_weighted1_prob = fs3 %>%
      compute_surv(time=seq_len(10), cycle_length=200, type="prob")
    
    fs2_weighted2_prob = pool(
      fs3 %>% set_covariates(group="Good"),
      fs3 %>% set_covariates(group="Medium"),
      fs3 %>% set_covariates(group="Poor"),
      weights = c(229, 229, 228)
    ) %>%
      compute_surv(time=seq_len(10), cycle_length=200, type="prob")
    
    expect_equal(fs1_weighted1_surv, fs1_weighted2_surv)
    expect_equal(fs2_weighted1_prob, fs2_weighted2_prob)
    
    # Should also give a warning
    expect_message(
      fs3 %>% compute_surv(time=seq_len(10), cycle_length=100, type="prob"),
      "No covariates provided, returning aggregate survial across all subjects."
    )
  }
)