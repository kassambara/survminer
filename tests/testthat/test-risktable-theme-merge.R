context("risk-table y-text theme is customizable (ggplot2 4.x)")

# Regression test for #557: under ggplot2 4.x, theme elements are S7 objects and
# ggtext::element_markdown() (used to colour the risk-table y labels per strata)
# can no longer be merged with a user-supplied element_text(), so customizing the
# risk table failed with "Can't merge the `axis.text.y` theme element". On
# ggplot2 >= 4.0 the y labels are now coloured with element_text(colour = vector)
# (native, mergeable); on < 4.0 element_markdown() is kept (it merges there).
library(survival)

drew_ok <- function(p) {
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)
  tryCatch({ suppressWarnings(grid::grid.draw(p)); TRUE }, error = function(e) FALSE)
}

test_that("a user element_text() override on the risk table merges and draws (#557)", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  p <- suppressWarnings(ggsurvplot(fit, data = lung, risk.table = TRUE))
  # This is the operation that errored under ggplot2 4.x with
  # "Can't merge the `axis.text.y` theme element".
  p$table <- p$table +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 14, face = "bold"))
  expect_true(drew_ok(p))
})

test_that("risk-table y labels are still coloured per strata after the fix (#557)", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  p <- suppressWarnings(ggsurvplot(fit, data = lung, risk.table = TRUE))
  el <- p$table$theme$axis.text.y
  # a per-label colour vector is carried (one colour per stratum), whichever
  # element class is used for the running ggplot2 version
  expect_length(el$colour, length(levels(surv_summary(fit, data = lung)$strata)))
  expect_true(drew_ok(p))
})

test_that("no-regression: risk.table.y.text = FALSE (large dash) still draws (#557)", {
  # The large-dash labels are an element_markdown(); the per-strata colouring must
  # also stay element_markdown() so the two merge (element_text() on top would
  # error "Can't merge the axis.text.y theme element" on ggplot2 4.x).
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  expect_true(drew_ok(suppressWarnings(
    ggsurvplot(fit, data = lung, risk.table = TRUE, risk.table.y.text = FALSE))))
  expect_true(drew_ok(suppressWarnings(
    ggsurvplot(fit, data = lung, risk.table = TRUE, tables.y.text = FALSE,
               cumcensor = TRUE))))
})

test_that("no-regression: risk.table.y.text.col = FALSE stays plain and customizable", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  p <- suppressWarnings(ggsurvplot(fit, data = lung, risk.table = TRUE,
                                   risk.table.y.text.col = FALSE))
  p$table <- p$table +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12))
  expect_true(drew_ok(p))
})
