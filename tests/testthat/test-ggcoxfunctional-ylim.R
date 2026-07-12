context("ggcoxfunctional default y-limits")

# Regression test for #465: ggcoxfunctional() previously defaulted the y-axis
# limits to the range of the lowess smoother (lowess_y), which is much narrower
# than the martingale residuals it plots as points, so most points were clipped
# out of view. The default y-axis now auto-scales to include ALL points, while a
# user-supplied ylim is still honoured.
library(survival)

test_that("default y-axis includes all martingale-residual points (#465)", {
  fit <- coxph(Surv(time, status) ~ age, data = lung)
  p   <- ggcoxfunctional(fit, data = lung)
  b   <- ggplot2::ggplot_build(p[["age"]])
  pts <- b$data[[1]]$y                      # geom_point layer = residuals
  yr  <- b$layout$panel_params[[1]]$y.range
  # every plotted point must fall inside the panel y-range
  expect_true(all(pts >= yr[1] & pts <= yr[2], na.rm = TRUE))
  # and the panel must be wider than the (narrow) lowess range that used to clip
  lw <- range(b$data[[2]]$y, na.rm = TRUE)  # geom_line layer = lowess smoother
  expect_true(yr[1] <= min(pts, na.rm = TRUE) && yr[2] >= max(pts, na.rm = TRUE))
  expect_true((yr[2] - yr[1]) > (lw[2] - lw[1]))
})

test_that("no-regression: user-supplied ylim is honoured (#465)", {
  fit <- coxph(Surv(time, status) ~ age, data = lung)
  p   <- ggcoxfunctional(fit, data = lung, ylim = c(-0.5, 1))
  b   <- ggplot2::ggplot_build(p[["age"]])
  yr  <- b$layout$panel_params[[1]]$y.range
  # The panel is clamped to the user's window (with a small axis expansion),
  # NOT auto-scaled to the full residual range (which reaches ~ -2.9). Bounds
  # are loose so the test is robust to ggplot2's expansion-factor defaults.
  expect_true(yr[1] <= -0.5 && yr[1] > -1)    # near user lower bound, not the -2.9 default
  expect_true(yr[2] >=  1.0 && yr[2] <  1.5)  # near user upper bound
})

test_that("no-regression: default x-axis is unchanged (#465)", {
  fit <- coxph(Surv(time, status) ~ age, data = lung)
  p   <- ggcoxfunctional(fit, data = lung)
  b   <- ggplot2::ggplot_build(p[["age"]])
  xr  <- b$layout$panel_params[[1]]$x.range
  ar  <- range(lung$age)
  # x-range still spans the covariate values (with expansion), as before
  expect_true(xr[1] <= ar[1] && xr[2] >= ar[2])
})
