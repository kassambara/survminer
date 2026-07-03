context("ggcoxzph transform = log")

# Regression test for #454 (and duplicate #588): with cox.zph(transform = "log"),
# ggcoxzph() drew the fitted line at log(pred.x) while the residual points and
# the SE bands were on the original (exp-ed) time scale, so the fit line was
# squeezed to the far left, mismatched with the points. The fit line now uses
# the same scale as the points and the axis is log-scaled (like
# survival::plot.cox.zph(log = "x")).
library(survival)

layer_xranges <- function(p) {
  b <- ggplot2::ggplot_build(p)
  lapply(b$data, function(d) if ("x" %in% names(d)) range(d$x, na.rm = TRUE))
}

test_that("fit line and residual points share the x-scale for transform='log' (#454)", {
  fit <- coxph(Surv(time, status) ~ age + sex, data = lung)
  zph <- cox.zph(fit, transform = "log")
  p   <- ggcoxzph(zph)[[1]]
  xr  <- layer_xranges(p)
  # layer 1 = fit line (geom_line), layer 2 = points (geom_point)
  expect_equal(xr[[1]], xr[[2]], tolerance = 1e-6)
  # x-axis is log-scaled
  b <- ggplot2::ggplot_build(p)
  expect_match(b$layout$panel_scales_x[[1]]$trans$name, "log")
  expect_error(ggplot2::ggplotGrob(p), NA)
})

test_that("no-regression: identity and km transforms still render (#454)", {
  fit <- coxph(Surv(time, status) ~ age + sex, data = lung)
  for (tr in c("identity", "km")) {
    zph <- cox.zph(fit, transform = tr)
    expect_error(ggplot2::ggplotGrob(ggcoxzph(zph)[[1]]), NA)
  }
})
