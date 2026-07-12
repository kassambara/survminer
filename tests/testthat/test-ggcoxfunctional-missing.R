context("ggcoxfunctional missing data")

# Regression test for #248: ggcoxfunctional() failed with "'x' and 'y' lengths
# differ" when the Cox formula contained a covariate with missing values.
# model.matrix() drops rows with missing terms, but the null-model martingale
# residuals were computed from the full data, so the two had different lengths.
# The data is now restricted to the model-matrix (complete-case) rows.
library(survival)

test_that("ggcoxfunctional() works with a covariate that has missing values (#248)", {
  # lung$wt.loss has 14 NAs
  fit <- coxph(Surv(time, status) ~ age + wt.loss, data = lung)
  expect_error(p <- ggcoxfunctional(fit, data = lung), NA)   # no error
  expect_identical(names(p), c("age", "wt.loss"))

  # each plot uses the complete-case rows only (228 - 14 = 214)
  n_complete <- sum(stats::complete.cases(lung[, c("time", "status", "age", "wt.loss")]))
  b <- ggplot2::ggplot_build(p[["wt.loss"]])
  expect_equal(nrow(b$data[[1]]), n_complete)
})

test_that("no-regression: complete-data ggcoxfunctional() is unaffected (#248)", {
  cc <- na.omit(lung[, c("time", "status", "age", "wt.loss")])
  fit <- coxph(Surv(time, status) ~ age + wt.loss, data = cc)
  expect_silent(p <- ggcoxfunctional(fit, data = cc))
  b <- ggplot2::ggplot_build(p[["age"]])
  expect_equal(nrow(b$data[[1]]), nrow(cc))   # all rows used (no drop)
})

test_that("missing data + a factor term are both handled (#248 + #357)", {
  lf <- lung
  lf$sex <- factor(lf$sex, labels = c("male", "female"))
  fit <- coxph(Surv(time, status) ~ age + sex + wt.loss, data = lf)
  # factor sex dropped (#357) with a warning; NA rows handled (#248)
  expect_warning(p <- ggcoxfunctional(fit, data = lf), "non-continuous")
  expect_identical(names(p), c("age", "wt.loss"))
})
