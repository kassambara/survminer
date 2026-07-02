context("ggsurvplot_add_all legend")

# Regression test for #566: `legend = "right"` partial-matched BOTH `legend.title`
# and `legend.labs`, raising "argument matches multiple formal arguments" before
# reaching ggsurvplot_core(). `legend` is now an explicit argument and forwarded,
# so any legend position works with add.all.
test_that("ggsurvplot_add_all() accepts a legend position (#566)", {
  library(survival)
  fit <- survfit(Surv(time, status) ~ sex, data = colon)

  for (pos in c("right", "bottom", "left", "none", "top")) {
    p <- ggsurvplot_add_all(fit, colon, legend = pos)
    expect_identical(
      ggplot2::ggplot_build(p$plot)$plot$theme$legend.position, pos
    )
  }

  # also via the ggsurvplot(add.all = TRUE) entry point (the reported call)
  expect_error(ggsurvplot(fit, data = colon, add.all = TRUE, legend = "right"), NA)
})
