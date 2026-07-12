context("break.y.by outside [0, 1]")

# Regression test for #378 (and #442): break.y.by computed the y breaks as
# seq(0, 1, by = break.y.by), so for transformed curves (fun = "cloglog" /
# "event" / "cumhaz") or a custom ylim outside [0, 1] the breaks were wrong
# (only 0 and 1, missing the negative / >1 range). The breaks are now derived
# from the displayed y-range.
library(survival)

y_breaks <- function(p) {
  bk <- ggplot2::ggplot_build(p$plot)$layout$panel_params[[1]]$y$breaks
  bk[!is.na(bk)]
}

test_that("cloglog break.y.by covers the negative range (#378)", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  p   <- ggsurvplot(fit, data = lung, fun = "cloglog", break.y.by = 1)
  bk  <- y_breaks(p)
  expect_true(any(bk < 0))          # negative breaks now present
  expect_true(all(bk == round(bk))) # on a clean integer grid
})

test_that("fun='event' with ylim > 1 breaks reach the top (#442)", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  p   <- ggsurvplot(fit, data = lung, fun = "event", ylim = c(0, 2),
                    break.y.by = 0.5)
  expect_equal(y_breaks(p), c(0, 0.5, 1, 1.5, 2))
})

test_that("no-regression: default survival break.y.by is unchanged (#378)", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  p   <- ggsurvplot(fit, data = lung, break.y.by = 0.25)
  expect_equal(y_breaks(p), c(0, 0.25, 0.5, 0.75, 1))
})
