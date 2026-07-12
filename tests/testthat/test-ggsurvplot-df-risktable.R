context("risk table from a surv_summary data frame (#409)")

# Regression test for #409: ggsurvplot(surv_summary(fit), risk.table = TRUE)
# produced only the curve, no risk table -- the data-frame input path ignored
# risk.table/cumevents/cumcensor. It now builds those tables from the data frame
# (which carries n.risk / n.event / n.censor) and returns the same compound
# "ggsurvplot" object as the survfit path. ggsurvplot_df() itself is unchanged
# (still returns a bare ggplot), so the survfit and combine paths are untouched.
library(survival)

draw_ok <- function(p) {
  tmp <- tempfile(fileext = ".pdf"); grDevices::pdf(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)
  print(p); invisible(TRUE)
}

fit <- survfit(Surv(time, status) ~ sex, data = lung)
ss  <- surv_summary(fit, lung)

test_that(".get_timepoints_from_df matches the survfit time-point table (#409)", {
  cols <- c("strata", "time", "n.risk", "pct.risk", "n.event",
            "cum.n.event", "n.censor", "cum.n.censor", "strata_size")
  times <- c(0, 200, 400, 600, 800, 1000)
  tgt <- survminer:::.get_timepoints_survsummary(fit, lung, times)[, cols]
  got <- survminer:::.get_timepoints_from_df(ss, times)[, cols]
  tgt$strata <- as.character(tgt$strata); got$strata <- as.character(got$strata)
  expect_equal(got, tgt, check.attributes = FALSE)
})

test_that("ggsurvplot(surv_summary(fit), risk.table=TRUE) builds a risk table (#409)", {
  p <- suppressWarnings(ggsurvplot(ss, risk.table = TRUE))
  expect_s3_class(p, "ggsurvplot")
  expect_false(is.null(p$table))
  expect_error(suppressWarnings(draw_ok(p)), NA)
  # the risk-table layer is identical to the survfit path's
  tbl_df  <- suppressWarnings(ggplot2::ggplot_build(p$table)$data)
  tbl_fit <- suppressWarnings(ggplot2::ggplot_build(
    ggsurvplot(fit, data = lung, risk.table = TRUE)$table)$data)
  expect_equal(tbl_df, tbl_fit)
})

test_that("cumevents and cumcensor also build from a data frame (#409)", {
  p <- suppressWarnings(ggsurvplot(ss, cumevents = TRUE, cumcensor = TRUE))
  expect_false(is.null(p$cumevents))
  expect_false(is.null(p$ncensor.plot))
  expect_error(suppressWarnings(draw_ok(p)), NA)
})

test_that("single-stratum data frame gets a risk table (#409)", {
  fit1 <- survfit(Surv(time, status) ~ 1, data = lung)
  p <- suppressWarnings(ggsurvplot(surv_summary(fit1, lung), risk.table = TRUE))
  expect_false(is.null(p$table))
  expect_error(suppressWarnings(draw_ok(p)), NA)
})

test_that("no-regression: a data frame without a table request is a bare ggplot (#409)", {
  p <- suppressWarnings(ggsurvplot(ss))
  expect_s3_class(p, "ggplot")
  expect_false(inherits(p, "ggsurvplot"))
})

test_that("axes.offset is honored on the data-frame path (#409)", {
  xr <- function(p){ pl <- if(inherits(p,"ggsurvplot")) p$plot else p
    suppressWarnings(ggplot2::ggplot_build(pl))$layout$panel_params[[1]]$x.range }
  # axes.offset = FALSE removes the x expansion (range starts at 0), like the
  # survfit path; with a table too.
  expect_equal(xr(suppressWarnings(ggsurvplot(ss, axes.offset = FALSE)))[1], 0)
  expect_equal(xr(suppressWarnings(ggsurvplot(ss, axes.offset = FALSE, risk.table = TRUE)))[1], 0)
})

test_that("cumevents/cumcensor display type is applied per table (#409)", {
  lbl <- function(tb) suppressWarnings(ggplot2::ggplot_build(tb)$data[[1]]$label)
  # cumevents = "percentage" shows percentages, matching the survfit path
  ce_df  <- suppressWarnings(ggsurvplot(ss, cumevents = "percentage"))$cumevents
  ce_fit <- suppressWarnings(ggsurvplot(fit, data = lung, cumevents = "percentage"))$cumevents
  expect_equal(lbl(ce_df), lbl(ce_fit))
})

test_that("risk.table.height is honored on the data-frame path (#409)", {
  h <- attr(suppressWarnings(ggsurvplot(ss, risk.table = TRUE, risk.table.height = 0.4)),
            "heights")$table
  expect_equal(h, 0.4)
})

test_that("a data frame lacking n.risk gives a clear error with risk.table (#409)", {
  bad <- data.frame(time = 1:5, surv = seq(1, 0.5, length.out = 5))
  expect_error(suppressWarnings(ggsurvplot(bad, risk.table = TRUE)), "Can't build")
})
