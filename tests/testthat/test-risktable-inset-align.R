context("risk.table.pos='in' numbers stay aligned to the survival plot x-axis (#302)")

# Regression tests for #302: with risk.table.pos = "in" the numbers-at-risk used
# to be embedded as a single risk-table grob stretched across the panel with one
# annotation_custom() call (xmin = -max(time)/20 .. Inf). The grob's internal x
# layout was pinned to the full data range, so as soon as the visible range
# changed -- xlim, axes.offset = FALSE -- the numbers drifted out of alignment
# with the axis ticks. Each number is now drawn as its own text annotation placed
# at its real (time, y), so ggplot's coordinate system keeps it on the tick, and
# because annotation_custom() draws in data coordinates without going through the
# y-scale the survival curve's coordinate range is left untouched.
library(survival)
library(ggplot2)

fit <- survfit(Surv(time, status) ~ sex, data = lung)

# helper: the per-number text annotations added by the inset transform.
# Number annotations have a finite xmax (drawn at a single time); the title
# annotation is left-aligned across the panel (xmax = Inf).
inset_annotations <- function(p, axes.offset = TRUE) {
  pin <- survminer:::.put_risktable_in_survplot(p, axes.offset = axes.offset)
  is_ann <- vapply(pin$plot$layers,
                   function(l) inherits(l$geom, "GeomCustomAnn"), logical(1))
  anns <- pin$plot$layers[is_ann]
  info <- lapply(anns, function(l) {
    gp <- l$geom_params
    list(xmin = gp$xmin, xmax = gp$xmax,
         label = tryCatch(gp$grob$label, error = function(e) NA_character_),
         col   = tryCatch(gp$grob$gp$col, error = function(e) NA_character_))
  })
  numbers <- Filter(function(z) is.finite(z$xmax), info)
  titles  <- Filter(function(z) !is.finite(z$xmax), info)
  list(plot = pin$plot, all = info, numbers = numbers, titles = titles)
}

test_that("inset numbers sit exactly on the x-axis break times (#302)", {
  p <- suppressWarnings(ggsurvplot(fit, data = lung, break.time.by = 250,
                                   risk.table = TRUE, risk.table.pos = "in",
                                   xlim = c(0, 500)))
  A <- inset_annotations(p, axes.offset = TRUE)
  # numbers are drawn as one data-coordinate annotation each (the old code drew a
  # single stretched grob -> only one annotation with a large negative xmin, so
  # this per-tick set fails on the old implementation)
  expect_gt(length(A$numbers), 1L)
  xs <- vapply(A$numbers, function(z) z$xmin, numeric(1))
  # each number is placed at its true event time; coord then clips those beyond
  # xlim at draw time. The set of x positions equals the actual break times.
  expect_setequal(round(sort(unique(xs))), c(0, 250, 500, 750, 1000))
})

test_that("no-regression: inset leaves the survival plot coordinate range unchanged (#302)", {
  yr <- function(p) suppressWarnings(
    ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y.range)
  xr <- function(p) suppressWarnings(
    ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x.range)

  # check across scale types: the label annotations must not retrain or expand
  # the y-scale, so "in" and "out" share an identical coordinate range.
  for (extra in list(list(), list(fun = "event"), list(fun = "cumhaz"),
                     list(ylim = c(0, 0.5)))) {
    pin  <- do.call(ggsurvplot, c(list(fit, data = lung, break.time.by = 250,
              risk.table = TRUE, risk.table.pos = "in"), extra))
    pout <- do.call(ggsurvplot, c(list(fit, data = lung, break.time.by = 250,
              risk.table = TRUE, risk.table.pos = "out"), extra))
    survplot_in <- survminer:::.put_risktable_in_survplot(pin, axes.offset = TRUE)$plot
    expect_equal(yr(survplot_in), yr(pout$plot))
    expect_equal(xr(survplot_in), xr(pout$plot))
  }
})

test_that("inset keeps the risk-table title (#302)", {
  # default title
  p <- suppressWarnings(ggsurvplot(fit, data = lung, break.time.by = 250,
                                   risk.table = TRUE, risk.table.pos = "in"))
  A <- inset_annotations(p, axes.offset = TRUE)
  expect_length(A$titles, 1L)
  expect_identical(A$titles[[1]]$label, "Number at risk")
  # custom title is honoured too
  p2 <- suppressWarnings(ggsurvplot(fit, data = lung, break.time.by = 250,
                                    risk.table = TRUE, risk.table.pos = "in",
                                    risk.table.title = "MY RISK TITLE"))
  A2 <- inset_annotations(p2, axes.offset = TRUE)
  expect_identical(A2$titles[[1]]$label, "MY RISK TITLE")
})

test_that("inset numbers follow the strata palette and cover every stratum (#302)", {
  p <- suppressWarnings(ggsurvplot(fit, data = lung, break.time.by = 250,
                                   risk.table = TRUE, risk.table.pos = "in"))
  A <- inset_annotations(p, axes.offset = TRUE)
  cols <- unique(vapply(A$numbers, function(z) z$col, character(1)))
  # two strata -> two distinct colours matching the curves
  expect_length(cols, 2L)
  expect_setequal(cols, c("#F8766D", "#00BFC4"))
})

test_that("inset still renders for a single-stratum fit (#302)", {
  fit1 <- survfit(Surv(time, status) ~ 1, data = lung)
  p <- suppressWarnings(ggsurvplot(fit1, data = lung, break.time.by = 250,
                                   risk.table = TRUE, risk.table.pos = "in"))
  tmp <- tempfile(fileext = ".pdf"); grDevices::pdf(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)
  expect_error(suppressWarnings(print(p)), NA)
})
