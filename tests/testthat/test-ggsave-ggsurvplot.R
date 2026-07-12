context("ggsave ggsurvplot")

# Regression test for #152: a ggsurvplot object is a list of ggplots, so
# ggsave() (which calls grid::grid.draw() on the plot) had no applicable
# method and errored ("no applicable method for 'grid.draw' ... class list").
# A grid.draw.ggsurvplot S3 method now delegates to print(x, newpage = FALSE),
# so ggsave(filename, plot = p) works directly, and print() is unchanged.
test_that("ggsave() works directly on a ggsurvplot via grid.draw() (#152)", {
  library(survival)
  fit <- survfit(Surv(time, status) ~ sex, data = lung)

  # risk.table = TRUE (curve + table): previously errored inside ggsave()
  p <- ggsurvplot(fit, risk.table = TRUE)
  f1 <- tempfile(fileext = ".pdf")
  on.exit(unlink(f1), add = TRUE)
  expect_error(ggplot2::ggsave(f1, plot = p, width = 7, height = 6), NA)
  expect_gt(file.info(f1)$size, 0)

  # risk.table = FALSE (single plot) also works
  p2 <- ggsurvplot(fit, risk.table = FALSE)
  f2 <- tempfile(fileext = ".pdf")
  on.exit(unlink(f2), add = TRUE)
  expect_error(ggplot2::ggsave(f2, plot = p2, width = 7, height = 5), NA)
  expect_gt(file.info(f2)$size, 0)

  # no-regression: print() still draws the assembled plot without error
  dev <- tempfile(fileext = ".pdf")
  on.exit(unlink(dev), add = TRUE)
  grDevices::pdf(dev)
  expect_error(print(p), NA)
  grDevices::dev.off()
})
