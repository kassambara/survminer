context("multi-state (survfitms) fits give a clear error, not a cryptic crash")

# Regression test for #373: a factor (or >2-level) status passed to Surv() makes
# survival::survfit() return a multi-state 'survfitms' object, whose n.risk /
# n.event / prev are matrix-shaped. surv_summary() then crashed with a cryptic
# "arguments imply differing number of rows". survminer draws single-event
# Kaplan-Meier curves only (multi-state / competing risks is ggcompetingrisks()),
# so surv_summary()/ggsurvplot() now fail early with an actionable message. The
# guard fires ONLY on survfitms; ordinary survfit inputs are unchanged.
library(survival)

test_that("a factor status yields a survfitms fit (setup sanity) (#373)", {
  d <- lung; d$s <- factor(ifelse(d$status == 2, "dead", "alive"))
  fms <- survfit(Surv(time, s) ~ 1, data = d)
  expect_true(inherits(fms, "survfitms"))
})

test_that("surv_summary() errors clearly on a survfitms fit (#373)", {
  d <- lung; d$s <- factor(ifelse(d$status == 2, "dead", "alive"))
  fms <- survfit(Surv(time, s) ~ 1, data = d)
  expect_error(surv_summary(fms), "multi-state")
  expect_error(surv_summary(fms), "ggcompetingrisks", fixed = TRUE)
})

test_that("ggsurvplot() errors clearly on a survfitms fit instead of crashing (#373)", {
  d <- lung; d$s <- factor(ifelse(d$status == 2, "dead", "alive"))
  fms <- survfit(Surv(time, s) ~ 1, data = d)
  expect_error(ggsurvplot(fms, data = d), "multi-state")
})

test_that("no-regression: numeric 0/1 and logical status still plot (#373)", {
  # numeric status -> ordinary survfit -> unchanged
  f_num <- survfit(Surv(time, status) ~ sex, data = lung)
  expect_false(inherits(f_num, "survfitms"))
  ss_num <- surv_summary(f_num, data = lung)
  expect_s3_class(ss_num, "data.frame")
  expect_true(all(c("time", "surv", "strata") %in% names(ss_num)))
  # logical status -> also ordinary survfit (not survfitms)
  d <- lung; d$dead <- lung$status == 2
  f_log <- survfit(Surv(time, dead) ~ sex, data = d)
  expect_false(inherits(f_log, "survfitms"))
  p <- suppressWarnings(ggsurvplot(f_log, data = d))
  expect_s3_class(p, "ggsurvplot")
})

test_that("no-regression: coxph-newdata (matrix surv) surv_summary is unchanged (#373)", {
  fit <- coxph(Surv(time, status) ~ age + sex, data = lung)
  sf <- survfit(fit, newdata = data.frame(age = c(50, 70), sex = c(1, 2)))
  ss <- surv_summary(sf, data = lung)
  expect_s3_class(ss, "data.frame")
  expect_true("strata" %in% names(ss))          # matrix path preserved
})
