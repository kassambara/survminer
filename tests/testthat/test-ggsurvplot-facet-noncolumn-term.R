context("ggsurvplot_facet clear error for non-column grouping terms (#380)")

# Regression test for #380: ggsurvplot_facet() auto-derives its colour from the
# survival-formula grouping variable. When the fit was built from a transformed
# or mis-specified term (e.g. `~ I(sex)`, `~ strata(sex)`, `~ cut(age, 3)`, or a
# formula assembled with eval(as.name(...)) inside a loop), that term is not a
# data column and surv_summary() does not split it into its own column, so the
# panels cannot be coloured by it. It used to be forwarded as `color`, treated as
# a colour palette by ggsurvplot_df(), and failed at draw time with a cryptic
# "Unknown colour name" / grDevices::col2rgb() error. The facet path now fails
# early with an actionable message pointing at reformulate() / a plain variable.
library(survival)

draw_ok <- function(p) {
  tmp <- tempfile(fileext = ".pdf"); grDevices::pdf(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)
  print(p)
  invisible(TRUE)
}

test_that("ggsurvplot_facet errors clearly when the grouping term is not a data column (#380)", {
  d <- lung; d$ph.ecog <- factor(d$ph.ecog)
  fit <- surv_fit(Surv(time, status) ~ I(sex), data = d)
  # actionable message (old code errored cryptically at draw time with col2rgb)
  expect_error(suppressWarnings(ggsurvplot_facet(fit, data = d, facet.by = "ph.ecog")),
               "cannot colour the panels by the grouping term")
  expect_error(suppressWarnings(ggsurvplot_facet(fit, data = d, facet.by = "ph.ecog")),
               "reformulate")
})

test_that("the clear error also covers strata() and cut() grouping terms (#380)", {
  d <- lung; d$ph.ecog <- factor(d$ph.ecog)
  for (frm in c("Surv(time, status) ~ strata(sex)",
                "Surv(time, status) ~ cut(age, 3)")) {
    fit <- surv_fit(as.formula(frm), data = d)
    expect_error(suppressWarnings(ggsurvplot_facet(fit, data = d, facet.by = "ph.ecog")),
                 "cannot colour the panels by the grouping term")
  }
})

test_that("no-regression: a plain grouping variable still renders and colours by its groups (#380)", {
  # For a fit whose grouping variable IS a data column the guard is a no-op
  # (color <- vars.notin.groupby, exactly as before), so the plot is unchanged:
  # it renders and shows one colour per group level.
  d <- lung; d$ph.ecog <- factor(d$ph.ecog); d$sex <- factor(d$sex)
  fit <- surv_fit(Surv(time, status) ~ sex, data = d)
  p <- suppressWarnings(ggsurvplot_facet(fit, data = d, facet.by = "ph.ecog"))
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  n_cols <- length(unique(b$data[[1]]$colour))
  expect_equal(n_cols, nlevels(d$sex))   # two sex groups -> two colours
  expect_error(suppressWarnings(draw_ok(p)), NA)
})
