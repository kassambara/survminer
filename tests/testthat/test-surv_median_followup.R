# Median follow-up (reverse Kaplan-Meier): surv_median_followup().

library(survival)

test_that("surv_median_followup matches prodlim's reverse-KM median (numeric parity)", {
  skip_if_not_installed("prodlim")
  # Overall (ungrouped)
  fit0 <- surv_fit(Surv(time, status) ~ 1, data = lung)
  got0 <- surv_median_followup(fit0)
  d <- within(lung, status <- Surv(time, status)[, 2])   # 0/1 status for prodlim
  q0 <- quantile(prodlim::prodlim(prodlim::Hist(time, status) ~ 1, data = d,
                                  reverse = TRUE), 0.5)
  expect_equal(got0$median[1], q0$quantile, tolerance = 1e-6)
  expect_equal(got0$lower[1],  q0$lower,    tolerance = 1e-6)

  # Per group (sex): each group's median vs prodlim on that subset
  fit <- surv_fit(Surv(time, status) ~ sex, data = lung)
  got <- surv_median_followup(fit)
  for (sx in c(1, 2)) {
    ds <- within(lung[lung$sex == sx, ], status <- Surv(time, status)[, 2])
    qs <- quantile(prodlim::prodlim(prodlim::Hist(time, status) ~ 1, data = ds,
                                    reverse = TRUE), 0.5)
    row <- got[got$strata == paste0("sex=", sx), ]
    expect_equal(row$median, qs$quantile, tolerance = 1e-6)
    expect_equal(row$lower,  qs$lower,    tolerance = 1e-6)
  }
})

test_that("surv_median_followup equals a hand-traced reverse-KM median", {
  # Tiny data traced by hand so the expected value does not reuse the function's
  # own `1 - status` reversal. status: 1 = event, 0 = censored.
  # Reversed (censored become events): events at t = 4, 8, 12; censored at 2, 6, 10.
  # Reverse KM:  S(<4)=1 -> S=0.8 at 4 (4/5) -> S=0.533 at 8 (2/3) -> S=0 at 12.
  # First time S <= 0.5 is t = 12  => median follow-up = 12.
  d <- data.frame(time   = c(2, 4, 6, 8, 10, 12),
                  status = c(1, 0, 1, 0,  1,  0))
  fit <- surv_fit(Surv(time, status) ~ 1, data = d)
  expect_equal(surv_median_followup(fit)$median[1], 12)
})

test_that("median follow-up differs from median survival (the whole point)", {
  fit <- surv_fit(Surv(time, status) ~ 1, data = lung)
  fu  <- surv_median_followup(fit)$median[1]
  os  <- surv_median(fit)$median[1]
  # For lung, follow-up median (588) is well above survival median (310).
  expect_gt(fu, os)
})

test_that("surv_median_followup structure and columns", {
  fit <- surv_fit(Surv(time, status) ~ sex, data = lung)
  tab <- surv_median_followup(fit)
  expect_s3_class(tab, "data.frame")
  expect_identical(names(tab), c("strata", "median", "lower", "upper"))
  expect_equal(nrow(tab), 2L)
  expect_setequal(tab$strata, c("sex=1", "sex=2"))
})

test_that("an ungrouped fit gives a single overall 'All' row", {
  fit <- surv_fit(Surv(time, status) ~ 1, data = lung)
  tab <- surv_median_followup(fit)
  expect_equal(nrow(tab), 1L)
  expect_identical(tab$strata, "All")
  expect_true(is.finite(tab$median))
})

test_that("conf.int overrides the fit's confidence level", {
  fit90 <- surv_fit(Surv(time, status) ~ 1, data = lung, conf.int = 0.90)
  # honours the fit's level by default
  d90 <- surv_median_followup(fit90)
  # explicit override to 0.99 widens (or holds) the interval vs 0.90
  d99 <- surv_median_followup(fit90, conf.int = 0.99)
  expect_equal(d90$median, d99$median)                 # point estimate unchanged
  # a wider confidence level cannot give a tighter lower limit
  expect_lte(d99$lower[1], d90$lower[1])
})

test_that("a fit built with conf.type = 'none' yields NA limits, median intact", {
  fit <- surv_fit(Surv(time, status) ~ 1, data = lung, conf.type = "none")
  tab <- surv_median_followup(fit)
  expect_true(is.finite(tab$median[1]))
  expect_true(is.na(tab$lower[1]))
  expect_true(is.na(tab$upper[1]))
})

test_that("temporary reverse-KM columns do not clash with user columns", {
  # a data set that already contains `.rkm_time` / `.rkm_rev` must still work
  d <- lung
  d$.rkm_time <- -1
  d$.rkm_rev <- -1
  fit <- surv_fit(Surv(time, status) ~ sex, data = d)
  tab <- surv_median_followup(fit)
  expect_setequal(tab$strata, c("sex=1", "sex=2"))
  expect_equal(tab$median[tab$strata == "sex=1"], 840)
})

test_that("competing-risks / multi-state and left-censored data are refused", {
  d <- data.frame(time = c(2, 4, 6, 8, 3, 5, 7, 9),
                  ev = factor(c(0, 1, 2, 1, 0, 2, 1, 2)),
                  g = rep(c("a", "b"), each = 4))
  fms <- survfit(Surv(time, ev) ~ g, data = d)
  expect_error(surv_median_followup(fms, data = d), "right-censored")
  fl <- survfit(Surv(time, rep(1, 8), type = "left") ~ g, data = d)
  expect_error(surv_median_followup(fl, data = d), "right-censored")
})

test_that("non-survfit input is rejected", {
  expect_error(surv_median_followup(lm(time ~ status, data = lung)),
               "must be a survfit")
})
