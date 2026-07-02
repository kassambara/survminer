context("ggsurvplot_combine surv.median.line")

# Regression test for #316: surv.median.line was documented to pass from
# ggsurvplot_combine() to ggsurvplot(), but it was forwarded to ggsurvplot_df()
# (which does not draw median lines) and silently ignored. Median lines are now
# computed from the list of fits and drawn on the combined plot.
library(survival)

n_segments <- function(p) {
  sum(vapply(p$plot$layers, function(l) inherits(l$geom, "GeomSegment"), logical(1)))
}

test_that("ggsurvplot_combine() draws median lines when requested (#316)", {
  fits <- list(all = survfit(Surv(time, status) ~ 1, data = colon),
               rx  = survfit(Surv(time, status) ~ rx, data = colon))

  # "hv" -> a horizontal + a vertical geom_segment layer
  expect_equal(n_segments(ggsurvplot_combine(fits, data = colon, surv.median.line = "hv")), 2L)
  # "v" and "h" -> a single geom_segment layer each
  expect_equal(n_segments(ggsurvplot_combine(fits, data = colon, surv.median.line = "v")), 1L)
  expect_equal(n_segments(ggsurvplot_combine(fits, data = colon, surv.median.line = "h")), 1L)
})

test_that("no-regression: ggsurvplot_combine() without surv.median.line draws no segments (#316)", {
  fits <- list(all = survfit(Surv(time, status) ~ 1, data = colon),
               rx  = survfit(Surv(time, status) ~ rx, data = colon))
  expect_equal(n_segments(ggsurvplot_combine(fits, data = colon)), 0L)
  expect_equal(n_segments(ggsurvplot_combine(fits, data = colon, surv.median.line = "none")), 0L)
})

test_that(".add_median_lines() drops NA medians and warns if none reached (#316)", {
  p <- ggsurvplot_combine(
    list(a = survfit(Surv(time, status) ~ 1, data = colon)),
    data = colon, surv.median.line = "hv"
  )
  expect_s3_class(p$plot, "ggplot")
  # a fit whose median is never reached (1 event, 9 censored) -> warning, no segment
  d <- data.frame(time = 1:10, status = c(1, rep(0, 9)))
  expect_warning(
    ggsurvplot_combine(list(x = survfit(Surv(time, status) ~ 1, data = d)),
                       data = d, surv.median.line = "hv"),
    "Median survival not reached"
  )
})
