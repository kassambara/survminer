context("surv_adjustedcurves fun argument")

# Feature test for #630: surv_adjustedcurves() gains a `fun` argument (matching
# ggadjustedcurves()) that transforms the returned survival column. The default
# (fun = NULL) leaves the output unchanged.
library(survival)

test_that("surv_adjustedcurves(fun=) transforms the survival column (#630)", {
  fit <- coxph(Surv(time, status) ~ sex + age, data = lung)
  base <- surv_adjustedcurves(fit, variable = "sex", data = lung, method = "average")

  ev <- surv_adjustedcurves(fit, variable = "sex", data = lung, method = "average",
                            fun = "event")
  expect_equal(ev$surv, 1 - base$surv)

  pct <- surv_adjustedcurves(fit, variable = "sex", data = lung, method = "average",
                             fun = "pct")
  expect_equal(pct$surv, 100 * base$surv)

  ch <- surv_adjustedcurves(fit, variable = "sex", data = lung, method = "average",
                            fun = "cumhaz")
  expect_equal(ch$surv, -log(base$surv))

  # only the survival column changes; time/variable columns are untouched
  expect_identical(ev[setdiff(names(ev), "surv")],
                   base[setdiff(names(base), "surv")])
})

test_that("no-regression: surv_adjustedcurves() default (fun = NULL) is unchanged (#630)", {
  fit <- coxph(Surv(time, status) ~ sex + age, data = lung)
  d1 <- surv_adjustedcurves(fit, variable = "sex", data = lung, method = "average")
  d2 <- surv_adjustedcurves(fit, variable = "sex", data = lung, method = "average",
                            fun = NULL)
  expect_identical(d1, d2)
  expect_true(all(d1$surv >= 0 & d1$surv <= 1))
})

test_that("no-regression: ggadjustedcurves(fun=) is not double-transformed (#630)", {
  # ggadjustedcurves() applies `fun` itself and does NOT forward it to
  # surv_adjustedcurves(), so the transform must be applied exactly once.
  fit <- coxph(Surv(time, status) ~ sex + age, data = lung)
  raw <- surv_adjustedcurves(fit, variable = "sex", data = lung, method = "average")
  p <- ggadjustedcurves(fit, variable = "sex", data = lung, method = "average",
                        fun = "event")
  y <- ggplot2::ggplot_build(p)$data[[1]]$y
  expect_equal(range(y), range(1 - raw$surv))  # once, not 1-(1-y)
})
