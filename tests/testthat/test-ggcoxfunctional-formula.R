# ggcoxfunctional() accepts either a fitted coxph or a formula + data, without a
# deprecation warning (the diagnostic uses only the model formula, so the two are
# equivalent).

skip_if_not_installed("survival")

test_that("the formula form works and no longer warns", {
  library(survival)
  f <- Surv(time, status) ~ age + wt.loss
  # formula form: no deprecation (or any) warning
  expect_silent(p_formula <- ggcoxfunctional(f, data = lung))
  expect_s3_class(p_formula, "ggcoxfunctional")

  # fit form: identical result
  fit <- coxph(f, data = lung)
  p_fit <- ggcoxfunctional(fit, data = lung)
  expect_equal(length(p_formula), length(p_fit))
  expect_equal(names(p_formula), names(p_fit))
  # the plotted martingale-residual data matches between the two entry points,
  # for every term (the diagnostic uses only the formula, not the fitted coefs)
  for (i in seq_along(p_fit))
    expect_equal(p_formula[[i]]$data, p_fit[[i]]$data)
})
