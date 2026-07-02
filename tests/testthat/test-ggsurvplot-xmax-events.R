context("ggsurvplot x-axis covers all events")

# Regression test for #655: the default upper x-limit was the largest "nice"
# axis break (scales::extended_breaks), which can be below max(time), silently
# clipping events beyond it. The upper x-limit now extends to cover max(time),
# while the axis tick breaks (and risk-table columns) are unchanged.
library(survival)

x_range <- function(p) ggplot2::ggplot_build(p$plot)$layout$panel_params[[1]]$x.range

test_that("default x-axis extends to show an event beyond the last break (#655)", {
  df <- lung
  df$status[6] <- 2
  df$time[6]   <- 1100          # event beyond the default last break (1000)
  fit <- survfit(Surv(time, status) ~ sex, data = df)
  p   <- ggsurvplot(fit, data = df)
  expect_gte(x_range(p)[2], 1100)
})

test_that("no-regression: data within the breaks is unchanged (#655)", {
  # max(time) <= last nice break -> xmax stays at the break, x-range unchanged
  d2  <- subset(lung, time <= 1000)
  fit <- survfit(Surv(time, status) ~ sex, data = d2)
  p   <- ggsurvplot(fit, data = d2)
  last_break <- max(scales::extended_breaks()(d2$time))
  # upper panel limit is driven by the break (with the usual expansion), not by
  # a value beyond max(time); assert it does not exceed the break by much
  expect_lte(x_range(p)[2], last_break * 1.06)
  expect_gte(x_range(p)[2], max(d2$time))
})

test_that("no-regression: user-supplied xlim is honoured (#655)", {
  df <- lung
  df$status[6] <- 2
  df$time[6]   <- 1100
  fit <- survfit(Surv(time, status) ~ sex, data = df)
  p   <- ggsurvplot(fit, data = df, xlim = c(0, 500))
  xr  <- x_range(p)
  expect_lt(xr[2], 600)          # clipped to the user window, not extended to 1100
})

test_that("no-regression: axis tick breaks (and risk-table columns) are unchanged (#655)", {
  df <- lung
  df$status[6] <- 2
  df$time[6]   <- 1100
  fit <- survfit(Surv(time, status) ~ sex, data = df)
  p   <- ggsurvplot(fit, data = df, risk.table = TRUE, break.time.by = 250)
  tb  <- ggplot2::ggplot_build(p$table)$layout$panel_params[[1]]$x$breaks
  tb  <- tb[!is.na(tb)]
  # no stray 1100 tick was introduced; breaks stay on the round grid
  expect_false(any(abs(tb - 1100) < 1e-6))
  expect_true(all(tb %% 250 == 0))
})
