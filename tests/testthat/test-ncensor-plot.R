context("ncensor.plot")

# Regression test for #298: ggsurvplot(..., ncensor.plot = TRUE) errored at
# DRAW time with "Unknown colour name: strata" for a single-group fit (~ 1),
# because the default color = "strata" was passed to geom_bar as a literal
# colour when the data has no 'strata' column. The bars must render, and
# grouped fits / explicit colours must be unaffected.
test_that("ncensor.plot renders for a single-group fit (#298)", {
  lung <- survival::lung

  # single group (~ 1): previously crashed at draw time
  f1 <- survival::survfit(survival::Surv(time, status) ~ 1, data = lung)
  p1 <- ggsurvplot(f1, data = lung, ncensor.plot = TRUE)
  expect_error(ggplot2::ggplot_build(p1$ncensor.plot), NA)
  # the single bar matches the survival curve colour (not a stray literal)
  curve_col <- unique(stats::na.omit(
    ggplot2::ggplot_build(p1$plot)$data[[1]]$colour))
  bar_col <- unique(ggplot2::ggplot_build(p1$ncensor.plot)$data[[1]]$fill)
  expect_identical(bar_col, curve_col)

  # no-regression: grouped fit still renders, bars coloured by strata (> 1 fill)
  f2 <- survival::survfit(survival::Surv(time, status) ~ sex, data = lung)
  p2 <- ggsurvplot(f2, data = lung, ncensor.plot = TRUE)
  b2 <- ggplot2::ggplot_build(p2$ncensor.plot)
  expect_gt(length(unique(b2$data[[1]]$fill)), 1)

  # no-regression: an explicit colour is honoured (not overridden), single group.
  # (color = <literal> triggers an unrelated survminer "use palette=" advisory.)
  p3 <- suppressWarnings(
    ggsurvplot(f1, data = lung, ncensor.plot = TRUE, color = "dodgerblue")
  )
  b3 <- ggplot2::ggplot_build(p3$ncensor.plot)
  expect_true("dodgerblue" %in% b3$data[[1]]$fill)
})
