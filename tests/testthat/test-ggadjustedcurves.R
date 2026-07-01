context("ggadjustedcurves")

# Regression test for #501 / #628: ggadjustedcurves() / surv_adjustedcurves()
# errored with "cannot xtfrm data frame" for a tibble input, because the curve
# helpers index columns with data[, variable], which returns a one-column
# tibble (not a vector) that then fails in sort()/unique(). The data is now
# coerced to a plain data.frame, so tibble and data.frame give identical curves.
test_that("ggadjustedcurves() accepts a tibble input (#501, #628)", {
  library(survival)
  df <- lung
  df$sex <- factor(df$sex)
  fit <- coxph(Surv(time, status) ~ sex + age, data = df)
  tb <- tibble::as_tibble(df)

  # default method ("conditional"): tibble must not error and must match df
  expect_error(ggadjustedcurves(fit, data = tb, variable = "sex"), NA)

  # tibble and data.frame produce identical adjusted curves for each method
  for (m in c("single", "average", "conditional")) {
    v <- if (m == "single") NULL else "sex"
    r_df <- surv_adjustedcurves(fit, data = df, variable = v, method = m)
    r_tb <- surv_adjustedcurves(fit, data = tb, variable = v, method = m)
    expect_equal(r_tb, r_df)
  }
})

# The "marginal" method also indexes the `reference` argument, so a tibble
# passed there (not just as `data`) must be coerced too (#628 review).
test_that("marginal adjusted curves accept tibble data and reference (#628)", {
  library(survival)
  fit2 <- coxph(Surv(stop, event) ~ size + strata(rx), data = bladder)
  df <- bladder
  tb <- tibble::as_tibble(df)

  base <- surv_adjustedcurves(fit2, data = df, variable = "rx",
                              method = "marginal", reference = df)
  # tibble as data, as reference, and as both -> identical to the data.frame run
  expect_equal(surv_adjustedcurves(fit2, data = tb, variable = "rx",
                                   method = "marginal", reference = df), base)
  expect_equal(surv_adjustedcurves(fit2, data = df, variable = "rx",
                                   method = "marginal", reference = tb), base)
  expect_equal(surv_adjustedcurves(fit2, data = tb, variable = "rx",
                                   method = "marginal", reference = tb), base)
})
