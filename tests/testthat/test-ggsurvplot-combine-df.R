context("ggsurvplot_combine accepts surv_summary data frames (#323)")

# Regression test for #323: ggsurvplot_combine() only accepted a list of survfit
# objects; passing a surv_summary() data frame (as ggsurvplot_df() accepts)
# errored in surv_summary() -> summary(x)$table ("$ operator is invalid for
# atomic vectors"). A data-frame element is now drawn directly. The survfit path
# is unchanged; the number-at-risk/cumulative tables and median lines are not
# available for a summary data frame and are refused with a clear message.
library(survival)

draw_ok <- function(p) {
  tmp <- tempfile(fileext = ".pdf"); grDevices::pdf(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)
  print(p); invisible(TRUE)
}

pfs <- survfit(Surv(time, status) ~ 1, data = colon)
os  <- survfit(Surv(time, status) ~ rx, data = colon)
dpfs <- surv_summary(pfs, colon)
dos  <- surv_summary(os, colon)

test_that("a surv_summary data frame can be combined (with or without data) (#323)", {
  p1 <- suppressWarnings(ggsurvplot_combine(list(a = dpfs)))          # no data
  expect_s3_class(p1, "ggsurvplot")
  expect_error(suppressWarnings(draw_ok(p1)), NA)
  p2 <- suppressWarnings(ggsurvplot_combine(list(a = dpfs), data = colon))
  expect_error(suppressWarnings(draw_ok(p2)), NA)
})

test_that("several data frames combine with name::strata labels (#323)", {
  p <- suppressWarnings(ggsurvplot_combine(list(PFS = dpfs, OS = dos)))
  b <- suppressWarnings(ggplot2::ggplot_build(p$plot))
  strata <- levels(p$data.survplot$strata %||% factor(character()))
  # one PFS curve + three OS curves
  lv <- levels(droplevels(as.factor(b$data[[1]]$group)))
  expect_gte(length(lv), 4L)
  expect_error(suppressWarnings(draw_ok(p)), NA)
})

test_that("a data frame and a survfit can be mixed (#323)", {
  p <- suppressWarnings(ggsurvplot_combine(list(PFS = dpfs, OS = os), data = colon))
  expect_error(suppressWarnings(draw_ok(p)), NA)
})

test_that("tables/median lines are refused for a summary data frame (#323)", {
  expect_error(suppressWarnings(ggsurvplot_combine(list(a = dpfs), risk.table = TRUE)),
               "require the survfit")
  expect_error(suppressWarnings(ggsurvplot_combine(list(a = dpfs), cumevents = TRUE)),
               "require the survfit")
  expect_error(suppressWarnings(ggsurvplot_combine(list(a = dpfs), surv.median.line = "hv")),
               "require the survfit")
})

test_that("a tibble surv_summary element is handled (coerced, not xtfrm error) (#323)", {
  skip_if_not_installed("tibble")
  tb <- tibble::as_tibble(dpfs)
  p <- suppressWarnings(ggsurvplot_combine(list(a = tb)))
  expect_error(suppressWarnings(draw_ok(p)), NA)
})

test_that("a data frame lacking surv_summary columns errors clearly (#323)", {
  bad <- data.frame(x = 1:5, y = 6:10)
  expect_error(suppressWarnings(ggsurvplot_combine(list(a = bad))),
               "missing the surv_summary")
})

test_that("no-regression: a survfit list still combines (with tables) (#323)", {
  p <- suppressWarnings(ggsurvplot_combine(list(PFS = pfs, OS = os), data = colon))
  expect_s3_class(p, "ggsurvplot")
  expect_error(suppressWarnings(draw_ok(p)), NA)
  pr <- suppressWarnings(ggsurvplot_combine(list(PFS = pfs, OS = os), data = colon,
                                            risk.table = TRUE))
  expect_false(is.null(pr$table))
})
