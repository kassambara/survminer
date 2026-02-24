context("surv_pvalue")

library(survival)
data(lung)

# Basic 2-group fit
fit_sex <- survfit(Surv(time, status) ~ sex, data = lung)

# 3+ group fit (ph.ecog with 3 levels after filtering)
lung2 <- lung[complete.cases(lung$ph.ecog), ]
lung2 <- lung2[lung2$ph.ecog != 3, ]
fit_ecog <- survfit(Surv(time, status) ~ ph.ecog, data = lung2)

test_that("surv_pvalue works with default survdiff method", {
  res <- surv_pvalue(fit_sex, lung, method = "survdiff")
  expect_true(is.data.frame(res))
  expect_true("pval" %in% names(res))
  expect_true("method" %in% names(res))
  expect_true("pval.txt" %in% names(res))
  expect_equal(res$method, "Log-rank")
  expect_true(res$pval < 0.01 && res$pval > 0.0001)
})

test_that("surv_pvalue works with log-rank weight (method='1')", {
  res <- surv_pvalue(fit_sex, lung, method = "1")
  expect_equal(res$method, "Log-rank")
  expect_true(res$pval < 0.01)
})

test_that("surv_pvalue works with Gehan-Breslow (method='n')", {
  res <- surv_pvalue(fit_sex, lung, method = "n")
  expect_equal(res$method, "Gehan-Breslow")
  expect_true(res$pval < 0.01)
})

test_that("surv_pvalue works with Tarone-Ware (method='sqrtN')", {
  res <- surv_pvalue(fit_sex, lung, method = "sqrtN")
  expect_equal(res$method, "Tarone-Ware")
  expect_true(res$pval < 0.01)
})

test_that("surv_pvalue works with Peto-Peto (method='S1')", {
  res <- surv_pvalue(fit_sex, lung, method = "S1")
  expect_equal(res$method, "Peto-Peto")
  expect_true(res$pval < 0.01)
})

test_that("surv_pvalue works with modified Peto-Peto (method='S2')", {
  res <- surv_pvalue(fit_sex, lung, method = "S2")
  expect_equal(res$method, "modified Peto-Peto")
  expect_true(res$pval < 0.01)
})

test_that("surv_pvalue works with Fleming-Harrington (method='FH_p=1_q=1')", {
  res <- surv_pvalue(fit_sex, lung, method = "FH_p=1_q=1")
  expect_equal(res$method, "Fleming-Harrington (p=1, q=1)")
  expect_true(res$pval < 0.01)
})

test_that("surv_pvalue works with 3+ groups", {
  res <- surv_pvalue(fit_ecog, lung2, method = "n")
  expect_equal(res$method, "Gehan-Breslow")
  expect_true(res$pval < 0.001)
})

test_that("surv_pvalue works with test.for.trend", {
  res <- surv_pvalue(fit_ecog, lung2, method = "1", test.for.trend = TRUE)
  expect_equal(res$method, "Log-rank, tft")
  expect_true(res$pval < 0.001)
})

test_that("surv_pvalue pval.txt is formatted correctly", {
  res <- surv_pvalue(fit_sex, lung, method = "S1")
  expect_true(grepl("^p [=<]", res$pval.txt))
})

test_that("surv_pvalue methods can be specified by full name", {
  res_gb <- surv_pvalue(fit_sex, lung, method = "Gehan-Breslow")
  expect_equal(res_gb$method, "Gehan-Breslow")

  res_pp <- surv_pvalue(fit_sex, lung, method = "Peto-Peto")
  expect_equal(res_pp$method, "Peto-Peto")
})
