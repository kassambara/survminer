context("risk-table large-dash size is customizable")

# Regression test for #642: with tables.y.text = FALSE the risk-table y-axis
# shows a large dash per stratum (size 50). Users could not change its size
# (`p$table <- p$table + theme(axis.text.y = element_markdown(size = ...))` was
# overwritten back to 50 when .set_large_dash_as_ytext() is re-applied at print).
# The dash is now built with size = 50 but the print-time re-application keeps a
# user-set size. The default (no customization) is unchanged.
library(survival)

dash_size <- function(g) g$theme$axis.text.y$size

test_that(".set_large_dash_as_ytext keeps a user-set size, defaults to 50 (#642)", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  p <- suppressWarnings(ggsurvplot(fit, data = lung, risk.table = TRUE,
                                   tables.y.text = FALSE))
  # default table is built with the large dash (size 50)
  expect_equal(dash_size(p$table), 50)
  # re-applying (as print does) keeps 50 -> byte-identical default
  expect_equal(dash_size(survminer:::.set_large_dash_as_ytext(p$table)), 50)
  # forcing a size (as the build does) sets it
  expect_equal(dash_size(survminer:::.set_large_dash_as_ytext(p$table, size = 50)), 50)
})

test_that("a user-set dash size survives the print-time re-application (#642)", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  p <- suppressWarnings(ggsurvplot(fit, data = lung, risk.table = TRUE,
                                   tables.y.text = FALSE))
  p$table <- p$table + theme(axis.text.y = ggtext::element_markdown(size = 10))
  # the print path calls .set_large_dash_as_ytext(x$table) with no size -> keep 10
  expect_equal(dash_size(survminer:::.set_large_dash_as_ytext(p$table)), 10)
  # and the whole thing still draws
  tmp <- tempfile(fileext = ".pdf"); grDevices::pdf(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)
  expect_error(suppressWarnings(print(p)), NA)
})

test_that("no-regression: cumevents/cumcensor default dash size is 50 (#642)", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  p <- suppressWarnings(ggsurvplot(fit, data = lung, risk.table = TRUE,
                                   cumcensor = TRUE, tables.y.text = FALSE))
  expect_equal(dash_size(p$ncensor.plot), 50)
})
