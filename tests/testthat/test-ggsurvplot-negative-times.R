context("ggsurvplot negative survival times")

# Regression test for #523: ggsurvplot() silently plotted negative survival
# times, producing a curve that appears to increase (not meaningful for a
# Kaplan-Meier estimate). It now emits a warning. The plot itself is unchanged.
library(survival)

test_that("ggsurvplot() warns when survival times are negative (#523)", {
  xx  <- data.frame(stim = c(-1, 1:4), sts = rep(1, 5))
  fit <- survfit(Surv(stim, sts) ~ 1, data = xx)
  expect_warning(ggsurvplot(fit, data = xx, xlim = c(-2, 5)),
                 "[Nn]egative survival times")
})

test_that("ggsurvplot_combine() also warns on negative times (#523)", {
  # The check lives in ggsurvplot_df(), the common draw point, so the combine
  # path (which bypasses ggsurvplot_core) is covered too.
  xx  <- data.frame(stim = c(-1, 1:4), sts = rep(1, 5))
  fit <- survfit(Surv(stim, sts) ~ 1, data = xx)
  expect_warning(ggsurvplot_combine(list(a = fit), data = xx),
                 "[Nn]egative survival times")
})

test_that("no-regression: ordinary (non-negative) data does NOT warn (#523)", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  # no negative-time warning should be raised for standard data
  warns <- character(0)
  withCallingHandlers(
    ggsurvplot(fit, data = lung),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("[Nn]egative survival times", warns)))
})

test_that("no-regression: the plot is still produced despite the warning (#523)", {
  xx  <- data.frame(stim = c(-1, 1:4), sts = rep(1, 5))
  fit <- survfit(Surv(stim, sts) ~ 1, data = xx)
  p   <- suppressWarnings(ggsurvplot(fit, data = xx, xlim = c(-2, 5)))
  expect_s3_class(p, "ggsurvplot")
  # the negative time is still present in the plotted curve (behaviour unchanged)
  b <- ggplot2::ggplot_build(p$plot)
  expect_true(min(b$data[[1]]$x, na.rm = TRUE) < 0)
})
