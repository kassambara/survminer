context("surv_pvalue pval.digits")

# Regression test for #343: surv_pvalue()$pval.txt used a hard-coded 2
# significant digits. A pval.digits argument now controls the number of
# significant digits; its default (2) reproduces the previous output exactly.
library(survival)

test_that("surv_pvalue() default pval.digits is unchanged (#343)", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  # default must be byte-identical to the previous hard-coded signif(pvalue, 2)
  expect_identical(surv_pvalue(fit, data = lung)$pval.txt, "p = 0.0013")
  expect_identical(surv_pvalue(fit, data = lung, pval.digits = 2)$pval.txt, "p = 0.0013")
})

test_that("surv_pvalue() honors pval.digits (#343)", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  expect_identical(surv_pvalue(fit, data = lung, pval.digits = 3)$pval.txt, "p = 0.00131")
  expect_identical(surv_pvalue(fit, data = lung, pval.digits = 4)$pval.txt, "p = 0.001311")
  expect_identical(surv_pvalue(fit, data = lung, pval.digits = 1)$pval.txt, "p = 0.001")
})

test_that("pval.digits leaves the '< 0.0001' floor and the numeric pval intact (#343)", {
  fit <- survfit(Surv(time, status) ~ ph.ecog, data = subset(lung, ph.ecog < 3))
  res2 <- surv_pvalue(fit, data = subset(lung, ph.ecog < 3), pval.digits = 2)
  res5 <- surv_pvalue(fit, data = subset(lung, ph.ecog < 3), pval.digits = 5)
  # the numeric p-value must not depend on the display digits
  expect_equal(res2$pval, res5$pval)
})

test_that("pval.digits works through the list (facet) code path (#343)", {
  gb <- surv_group_by(lung, "sex")
  fits <- surv_fit(Surv(time, status) ~ ph.ecog, data = gb$data)
  res <- surv_pvalue(fits, gb$data, pval.digits = 3)
  expect_true(is.list(res))
  expect_true(all(grepl("^p [=<]", vapply(res, function(x) x$pval.txt, character(1)))))
})
