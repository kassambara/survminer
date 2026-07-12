context("break.time.by accepts a vector of custom breaks (#695, #435)")

# #435 / #695: break.time.by only accepted a single step (seq(0, max, by=)),
# so a vector of custom time points errored ("'by' must be of length 1"), and a
# custom scale_x_continuous(breaks=) on $plot never reached the risk table
# (misalignment). break.time.by now accepts a vector, used as-is for both the
# curve and the tables. A single value is unchanged.
library(survival)

fit <- survfit(Surv(time, status) ~ sex, data = lung)

built_x_breaks <- function(p) {
  b <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$get_breaks()
  sort(b[!is.na(b)])
}

test_that("a vector break.time.by sets the exact plot breaks (#435)", {
  brks <- c(0, 100, 300, 600, 1000)
  p <- ggsurvplot(fit, data = lung, break.time.by = brks)
  expect_equal(built_x_breaks(p$plot), brks)
})

test_that("the risk table uses the same custom breaks -> aligned (#695)", {
  brks <- c(0, 100, 300, 600, 1000)
  p <- ggsurvplot(fit, data = lung, risk.table = TRUE, break.time.by = brks)
  expect_equal(built_x_breaks(p$plot), built_x_breaks(p$table))
  expect_error(ggplot2::ggplotGrob(p$table), NA)
})

test_that("break.x.by (alias) also accepts a vector (#695)", {
  brks <- c(0, 250, 750)
  p <- ggsurvplot(fit, data = lung, break.x.by = brks)
  expect_equal(built_x_breaks(p$plot), brks)
})

test_that("no-regression: a single break.time.by is unchanged (#435)", {
  # helper: length-1 reproduces seq(0, max, by=)
  expect_equal(.time_breaks(250, 1000), seq(0, 1000, by = 250))
  p <- ggsurvplot(fit, data = lung, break.time.by = 250)
  expect_true(all(c(0, 250, 500, 750, 1000) %in% built_x_breaks(p$plot)))
})
