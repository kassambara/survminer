context("build_ggsurvplot() exported grob builder (#569)")

# #569: the internal helper that assembles a ggsurvplot into a single grob
# (survival curve + risk table + ...) had no public accessor. build_ggsurvplot()
# is a thin exported wrapper over the internal .build_ggsurvplot(), returning the
# assembled gtable grob.
library(survival)

fit <- survfit(Surv(time, status) ~ sex, data = lung)

test_that("build_ggsurvplot() returns the assembled grob (#569)", {
  p <- ggsurvplot(fit, data = lung, risk.table = TRUE)
  g <- build_ggsurvplot(p)
  expect_s3_class(g, "gtable")
  expect_true(inherits(g, "grob"))
  # same as the internal builder it wraps
  expect_identical(class(g), class(survminer:::.build_ggsurvplot(p)))
  # drawable without error
  expect_error({ grid::grid.newpage(); grid::grid.draw(g) }, NA)
})

test_that("build_ggsurvplot() works without a risk table (#569)", {
  p <- ggsurvplot(fit, data = lung)
  expect_true(inherits(build_ggsurvplot(p), "grob"))
})

test_that("build_ggsurvplot() requires a ggsurvplot object (#569)", {
  expect_error(build_ggsurvplot(1), "ggsurvplot")
  expect_error(build_ggsurvplot(ggplot2::ggplot()), "ggsurvplot")
})
