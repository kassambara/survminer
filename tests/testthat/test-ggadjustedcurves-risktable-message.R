context("ggadjustedcurves risk.table message (#286)")

# ggadjustedcurves() returns a plain ggplot and has no risk table of its own.
# A risk.table = TRUE passed here used to be silently swallowed by `...` and do
# nothing. It now emits a message pointing to the documented recipe, without
# changing the returned plot.
library(survival)

cox <- coxph(Surv(time, status) ~ sex + age + ph.ecog, data = lung)

test_that("risk.table = TRUE emits a recipe-pointing message (#286)", {
  expect_message(
    ggadjustedcurves(cox, variable = "sex", data = lung, risk.table = TRUE),
    "does not draw a risk table")
})

test_that("no message for ordinary calls or risk.table = FALSE (#286)", {
  expect_message(ggadjustedcurves(cox, variable = "sex", data = lung), NA)
  expect_message(
    ggadjustedcurves(cox, variable = "sex", data = lung, risk.table = FALSE), NA)
})

test_that("the message does not change the returned plot (#286)", {
  a <- ggplot2::ggplot_build(
    suppressMessages(ggadjustedcurves(cox, variable = "sex", data = lung,
                                      risk.table = TRUE)))$data
  b <- ggplot2::ggplot_build(
    ggadjustedcurves(cox, variable = "sex", data = lung))$data
  expect_identical(a, b)
})
