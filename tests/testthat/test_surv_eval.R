context("Evaluation of survival functions")


fs1 = flexsurv::flexsurvreg(survival::Surv(rectime, censrec)~group, data=flexsurv::bc, dist="weibull")
fs2 = flexsurv::flexsurvreg(survival::Surv(rectime, censrec)~group, data=flexsurv::bc, dist="weibullPH")

# Testing apply_hr, apply_af, apply_or against flexsurvreg output to see
# that it is consitent.

surv1_medium_surv = fs1 %>%
  set_covariates(group="Medium") %>%
  eval_surv_(cycle=seq_len(10),cycle_length=200, type="surv")

surv1_medium_aft_surv = fs1 %>%
  set_covariates(group="Good") %>%
  apply_af(fs1$coefficients[3], log.af=T) %>%
  eval_surv_(cycle=seq_len(10),cycle_length=200, type="surv")

surv1_poor_surv = fs1 %>%
  set_covariates(group="Poor") %>%
  eval_surv_(cycle=seq_len(10),cycle_length=200, type="surv")

surv1_poor_aft_surv = fs1 %>%
    set_covariates(group="Good") %>%
    apply_af(fs1$coefficients[4], log.af=T) %>%
    eval_surv_(cycle=seq_len(10),cycle_length=200, type="surv")


surv1_medium_prob = fs1 %>%
  set_covariates(group="Medium") %>%
  eval_surv_(cycle=seq_len(10),cycle_length=200, type="prob")

surv1_medium_aft_prob = fs1 %>%
  set_covariates(group="Good") %>%
  apply_af(fs1$coefficients[3], log.af=T) %>%
  eval_surv_(cycle=seq_len(10),cycle_length=200, type="prob")

surv1_poor_prob = fs1 %>%
  set_covariates(group="Poor") %>%
  eval_surv_(cycle=seq_len(10),cycle_length=200, type="prob")

surv1_poor_aft_prob = fs1 %>%
  set_covariates(group="Good") %>%
  apply_af(fs1$coefficients[4], log.af=T) %>%
  eval_surv_(cycle=seq_len(10),cycle_length=200, type="prob")



surv2_medium_surv = fs2 %>%
  set_covariates(group="Medium") %>%
  eval_surv_(cycle=seq_len(10),cycle_length=200, type="surv")

surv2_medium_hr_surv = fs2 %>%
  set_covariates(group="Good") %>%
  apply_hr(fs2$coefficients[3], log.hr=T) %>%
  eval_surv_(cycle=seq_len(10),cycle_length=200, type="surv")

surv2_poor_surv = fs2 %>%
  set_covariates(group="Poor") %>%
  eval_surv_(cycle=seq_len(10),cycle_length=200, type="surv")

surv2_poor_hr_surv = fs2 %>%
  set_covariates(group="Good") %>%
  apply_hr(fs2$coefficients[4], log.hr=T) %>%
  eval_surv_(cycle=seq_len(10),cycle_length=200, type="surv")

surv2_medium_prob = fs2 %>%
  set_covariates(group="Medium") %>%
  eval_surv_(cycle=seq_len(10),cycle_length=200, type="prob")

surv2_medium_hr_prob = fs2 %>%
  set_covariates(group="Good") %>%
  apply_hr(fs2$coefficients[3], log.hr=T) %>%
  eval_surv_(cycle=seq_len(10),cycle_length=200, type="prob")

surv2_poor_prob = fs2 %>%
  set_covariates(group="Poor") %>%
  eval_surv_(cycle=seq_len(10),cycle_length=200, type="prob")

surv2_poor_hr_prob = fs2 %>%
  set_covariates(group="Good") %>%
  apply_hr(fs2$coefficients[4], log.hr=T) %>%
  eval_surv_(cycle=seq_len(10),cycle_length=200, type="prob")

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