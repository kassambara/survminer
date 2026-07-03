context("ggadjustedcurves fun transformation")

# Regression test for #287 (and duplicates #498, #630, #660): ggadjustedcurves()
# accepted a `fun` argument but used it only to set the y-axis limits; the
# plotted curve was always the raw survival probability. `fun` now transforms
# the curve (e.g. "event" -> 1 - surv, "cumhaz" -> -log(surv)).
library(survival)

curve_y <- function(p) ggplot2::ggplot_build(p)$data[[1]]$y

test_that("fun = 'event' plots 1 - survival (#287)", {
  fit <- coxph(Surv(stop, event) ~ rx + number + size, data = bladder)
  p_def <- ggadjustedcurves(fit, data = bladder, method = "average", variable = "rx")
  p_evt <- ggadjustedcurves(fit, data = bladder, method = "average", variable = "rx",
                            fun = "event")
  expect_equal(sort(curve_y(p_evt)), sort(1 - curve_y(p_def)))
})

test_that("fun = 'cumhaz' transforms the curve (#498)", {
  fit <- coxph(Surv(stop, event) ~ rx + number + size, data = bladder)
  p_def <- ggadjustedcurves(fit, data = bladder, method = "average", variable = "rx")
  p_cum <- ggadjustedcurves(fit, data = bladder, method = "average", variable = "rx",
                            fun = "cumhaz")
  expect_false(isTRUE(all.equal(curve_y(p_cum), curve_y(p_def))))
  expect_equal(sort(curve_y(p_cum)), sort(-log(curve_y(p_def))))
})

test_that("no-regression: default (fun = NULL) is the raw survival curve (#287)", {
  fit <- coxph(Surv(stop, event) ~ rx + number + size, data = bladder)
  p <- ggadjustedcurves(fit, data = bladder, method = "average", variable = "rx")
  y <- curve_y(p)
  expect_true(all(y >= 0 & y <= 1))
})
