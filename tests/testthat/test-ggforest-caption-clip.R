context("ggforest global-stats caption not clipped")

# Regression test for #696: the global-statistics caption (# events, global
# p-value, AIC, concordance) drawn at the bottom of ggforest() was cut off in
# short plots. Space is now reserved below the last row (only when the caption is
# drawn), so it renders. Hard to assert "not clipped" from grobs, so we lock in
# that (a) short/tall plots render without error, (b) the caption is present when
# global.stats = TRUE and absent when FALSE.
library(survival)

.ggforest_labels <- function(p) {
  g <- grid::grid.force(ggplot2::ggplotGrob(p))
  out <- character(0)
  walk <- function(gr) {
    if (!is.null(gr$children)) for (ch in gr$children) walk(ch)
    if (!is.null(gr$label)) out[[length(out) + 1]] <<- paste(gr$label, collapse = "|")
  }
  walk(g)
  unlist(out)
}

test_that("short ggforest renders and shows the global-stats caption (#696)", {
  m <- coxph(Surv(time, status) ~ sex, data = lung)     # 1 variable -> short plot
  p <- ggforest(m, data = lung)
  expect_error(ggplot2::ggplotGrob(p), NA)
  expect_true(any(grepl("# Events", .ggforest_labels(p))))
})

test_that("tall ggforest still renders with the caption (#696)", {
  m <- coxph(Surv(time, status) ~ sex + ph.ecog + age + wt.loss, data = lung)
  p <- ggforest(m, data = lung)
  expect_error(ggplot2::ggplotGrob(p), NA)
  expect_true(any(grepl("# Events", .ggforest_labels(p))))
})

test_that("no-regression: global.stats = FALSE draws no caption (#696)", {
  m <- coxph(Surv(time, status) ~ sex, data = lung)
  p <- ggforest(m, data = lung, global.stats = FALSE)
  expect_error(ggplot2::ggplotGrob(p), NA)
  expect_false(any(grepl("# Events", .ggforest_labels(p))))
})
