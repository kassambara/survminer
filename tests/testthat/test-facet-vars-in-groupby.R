context("ggsurvplot facet.by with all variables in facet.by")

# Regression test for #304: ggsurvplot(fit, facet.by = "X") errored with
# "subscript out of bounds" when every variable of the survival formula is also
# a facet.by variable -- a null model (~ 1) faceted, or ~ X faceted by X. In that
# case there is no within-panel grouping to build into a .strata. column, so
# .create_strata() was called with zero variables and crashed.
library(survival)

x <- data.frame(
  Time = c(10, 20, 20, 20),
  Dead = c(TRUE, FALSE, FALSE, FALSE),
  X = c(1, 1, 2, 2),
  Q = c(1, 1, 1, 1)
)

test_that("facet.by works for a null model ~ 1 (#304)", {
  p <- ggsurvplot(survfit(Surv(Time, Dead) ~ 1, x), data = x, facet.by = "X")
  expect_error(ggplot2::ggplotGrob(p), NA)
  # The plot data must carry the faceting variable and show the correct
  # per-panel subgroup curves: X = 1 drops to 0.5 (one event at t = 10),
  # X = 2 stays at 1.0 (no events).
  pd <- p$data
  expect_true("X" %in% colnames(pd))
  expect_equal(min(pd$surv[pd$X == 1]), 0.5)
  expect_equal(min(pd$surv[pd$X == 2]), 1.0)
})

test_that("facet.by works when the grouping variable equals facet.by (#304)", {
  p <- ggsurvplot(survfit(Surv(Time, Dead) ~ X, x), data = x, facet.by = "X")
  expect_error(ggplot2::ggplotGrob(p), NA)
})

test_that("no-regression: facet.by with an extra grouping variable still works (#304)", {
  # length(vars.notin.groupby) == 1 branch
  fit1 <- survfit(Surv(time, status) ~ sex, data = colon)
  p1 <- ggsurvplot(fit1, data = colon, facet.by = "rx")
  expect_error(ggplot2::ggplotGrob(p1), NA)
  # length(vars.notin.groupby) >= 2 branch (.strata. combination)
  fit2 <- survfit(Surv(time, status) ~ sex + adhere, data = colon)
  p2 <- ggsurvplot(fit2, data = colon, facet.by = "rx")
  expect_error(ggplot2::ggplotGrob(p2), NA)
})
