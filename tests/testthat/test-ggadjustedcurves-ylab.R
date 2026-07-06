context("ggadjustedcurves y-axis label follows fun (#555)")

# #555: ggadjustedcurves() kept the default y-axis label "Survival rate" even
# when `fun` transformed the curve (e.g. fun = "cumhaz" drew the cumulative
# hazard but still labelled the axis "Survival rate"). It now relabels the axis
# to match `fun`, like ggsurvplot() does -- but only when the user kept the
# default ylab, so fun = NULL and any user-supplied ylab are unchanged.
library(survival)

fit <- coxph(Surv(time, status) ~ sex + age, data = lung)
ylab_of <- function(p) p$labels$y

test_that("no-regression: default fun keeps the 'Survival rate' label (#555)", {
  p <- ggadjustedcurves(fit, data = lung, variable = "sex")
  expect_identical(ylab_of(p), "Survival rate")
})

test_that("fun relabels the y-axis to match the transformation (#555)", {
  expect_identical(ylab_of(ggadjustedcurves(fit, data = lung, variable = "sex",
                                            fun = "cumhaz")), "Cumulative hazard")
  expect_identical(ylab_of(ggadjustedcurves(fit, data = lung, variable = "sex",
                                            fun = "event")), "Cumulative event")
  expect_identical(ylab_of(ggadjustedcurves(fit, data = lung, variable = "sex",
                                            fun = "pct")), "Survival rate (%)")
  expect_identical(ylab_of(ggadjustedcurves(fit, data = lung, variable = "sex",
                                            fun = "log")), "log(Survival rate)")
})

test_that("no-regression: a user-supplied ylab is never overridden (#555)", {
  p <- ggadjustedcurves(fit, data = lung, variable = "sex",
                        fun = "cumhaz", ylab = "My axis")
  expect_identical(ylab_of(p), "My axis")
})

test_that("a function `fun` keeps the default label (can't be named) (#555)", {
  p <- ggadjustedcurves(fit, data = lung, variable = "sex",
                        fun = function(y) -log(y))
  expect_identical(ylab_of(p), "Survival rate")
})
