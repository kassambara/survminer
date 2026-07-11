context("surv_median() on a fit without / non-default confidence limits (#818)")

# surv_median() hard-coded the "0.95LCL"/"0.95UCL" columns of summary()$table, so
# it errored on a survfit stored without CI (conf.type = "none") AND on a
# non-default confidence level (conf.int = 0.9 names them "0.9LCL"/"0.9UCL"). The
# CI columns are now detected by suffix, with NA fallback when absent.
library(survival)

test_that("surv_median() returns NA limits for a no-CI fit instead of erroring (#818)", {
  fit <- survfit(Surv(time, status) ~ rx, data = colon, conf.type = "none")
  expect_error(m <- surv_median(fit), NA)
  expect_true(all(is.na(m$lower)))
  expect_true(all(is.na(m$upper)))
  expect_false(all(is.na(m$median)))                 # medians still computed
  expect_equal(nrow(m), length(fit$strata))
})

test_that("surv_median() uses the real limits for a non-default confidence level (#818)", {
  # a naive NA-fill would silently drop the true 0.90 CI; it must be used.
  f90 <- survfit(Surv(time, status) ~ rx, data = colon, conf.int = 0.90)
  m90 <- surv_median(f90)
  tab <- as.data.frame(summary(f90)$table)
  expect_equal(m90$lower, unname(tab[["0.9LCL"]]))
  expect_equal(m90$upper, unname(tab[["0.9UCL"]]))
  # 90% interval is narrower than the 95% interval for the same fit
  m95 <- surv_median(survfit(Surv(time, status) ~ rx, data = colon))
  ok <- !is.na(m90$lower) & !is.na(m95$lower)
  expect_true(all((m90$upper - m90$lower)[ok] <= (m95$upper - m95$lower)[ok]))
})

test_that("no-regression: default 0.95 surv_median is unchanged (#818)", {
  m <- surv_median(survfit(Surv(time, status) ~ rx, data = colon))
  expect_identical(colnames(m), c("strata", "median", "lower", "upper"))
  expect_false(any(is.na(m$lower[m$strata != "rx=Lev+5FU"])))   # real limits present
})

test_that("combine = TRUE and single-group ~1 handle a no-CI fit (#818)", {
  expect_error(
    surv_median(list(a = survfit(Surv(time, status) ~ sex, data = colon,
                                 conf.type = "none")), combine = TRUE), NA)
  m1 <- surv_median(survfit(Surv(time, status) ~ 1, data = colon, conf.type = "none"))
  expect_equal(m1$strata, "All")
  expect_true(is.na(m1$lower))
})
