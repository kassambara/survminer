context("risk-table t=0 alignment with the curve origin (axes.offset = FALSE)")

# Regression tests for #645 / #302 / #448: with axes.offset = FALSE the table
# panel starts exactly at x = 0 (like the curve), but the t = 0 numbers-at-risk
# were historically nudged right to max(xlim)/30, so they didn't line up with the
# curve's t = 0. They are now kept at x = 0 and only the t = 0 column is
# left-aligned (a centred number at x = 0 would be clipped). The default
# axes.offset = TRUE path, the log-axis path, and the inset (risk.table.pos="in")
# path are unchanged.
library(survival)
library(ggplot2)

fit <- survfit(Surv(time, status) ~ sex, data = lung)

t0_hjust <- function(tb) {
  lyr <- suppressWarnings(ggplot_build(tb)$data[[1]])
  sort(unique(lyr$hjust[lyr$x == 0]))
}

test_that("no-regression: axes.offset = TRUE keeps t=0 centred at time 0 (#645)", {
  p <- suppressWarnings(ggsurvplot(fit, data = lung, risk.table = TRUE,
                                   axes.offset = TRUE, xlim = c(0, 1050),
                                   break.time.by = 250))
  expect_equal(min(p$table$data$time), 0)          # t=0 stays at 0
  lyr <- suppressWarnings(ggplot_build(p$table)$data[[1]])
  expect_equal(sort(unique(lyr$hjust)), 0.5)       # scalar centred, unchanged
})

test_that("axes.offset = FALSE aligns t=0 to x=0 and left-aligns it (#645)", {
  p <- suppressWarnings(ggsurvplot(fit, data = lung, risk.table = TRUE,
                                   axes.offset = FALSE, xlim = c(0, 1050),
                                   break.time.by = 250))
  # t=0 numbers now sit at x=0 (previously max(xlim)/30 = 35)
  expect_equal(min(p$table$data$time), 0)
  # only the t=0 column is left-aligned; interior columns stay centred
  expect_equal(t0_hjust(p$table), 0)
  lyr <- suppressWarnings(ggplot_build(p$table)$data[[1]])
  expect_setequal(unique(lyr$hjust), c(0, 0.5))
  # still draws
  tmp <- tempfile(fileext = ".pdf"); grDevices::pdf(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)
  expect_error(print(p), NA)
})

test_that("no-regression: inset risk.table.pos='in' keeps historical geometry (#645)", {
  p <- suppressWarnings(ggsurvplot(fit, data = lung, risk.table = TRUE,
                                   risk.table.pos = "in", axes.offset = FALSE,
                                   xlim = c(0, 1050), break.time.by = 250))
  # origin.align = FALSE for the inset -> t=0 still relocated (not 0)
  expect_gt(min(p$table$data$time), 0)
})

test_that("no-regression: standalone log-axis table keeps the relocation (no log10(0)) (#645)", {
  tb <- suppressWarnings(ggrisktable(fit, data = lung, xlog = TRUE,
                                     axes.offset = FALSE, break.time.by = 250))
  if (inherits(tb, "list")) tb <- tb$risk.table
  expect_false(any(tb$data$time == 0))             # relocation kept for log axis
  lyr <- suppressWarnings(ggplot_build(tb)$data[[1]])
  expect_true(all(is.finite(lyr$x)))               # no -Inf from log10(0)
  expect_equal(sort(unique(lyr$hjust)), 0.5)       # scalar path
})

test_that("cumevents/cumcensor tables also align t=0 with axes.offset = FALSE (#645)", {
  p <- suppressWarnings(ggsurvplot(fit, data = lung, risk.table = FALSE,
                                   cumevents = TRUE, cumcensor = TRUE,
                                   axes.offset = FALSE, xlim = c(0, 1050),
                                   break.time.by = 250))
  expect_equal(min(p$cumevents$data$time), 0)
  expect_equal(min(p$ncensor.plot$data$time), 0)   # cumcensor table
})

test_that("no-regression: standalone linear ggrisktable gets the fix (#645)", {
  tb <- suppressWarnings(ggrisktable(fit, data = lung, axes.offset = FALSE,
                                     break.time.by = 250))
  if (inherits(tb, "list")) tb <- tb$risk.table
  expect_equal(min(tb$data$time), 0)
  expect_equal(t0_hjust(tb), 0)
})
