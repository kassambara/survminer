context("ggsurvplot_facet pval argument")

# Regression test for #636: the docs (inherited from ggsurvplot()) claimed that
# a numeric or character `pval` could be substituted, but ggsurvplot_facet()
# computes a p-value per panel. A character `pval` crashed at `if(pval)` with
# "argument is not interpretable as logical"; a numeric `pval` was silently
# ignored. Now a non-FALSE pval draws the per-panel p-values and a
# numeric/character value warns (rather than crashing); the docs are corrected.
library(survival)

test_that("character pval no longer errors, warns instead (#636)", {
  fit <- survfit(Surv(time, status) ~ rx, data = colon)
  expect_warning(
    ggsurvplot_facet(fit, colon, facet.by = "sex", pval = "p = 0.03"),
    "not substituted"
  )
})

test_that("numeric pval warns that it is not substituted (#636)", {
  fit <- survfit(Surv(time, status) ~ rx, data = colon)
  expect_warning(
    ggsurvplot_facet(fit, colon, facet.by = "sex", pval = 0.345),
    "not substituted"
  )
})

test_that("no-regression: pval = TRUE draws per-panel p-values without warning (#636)", {
  fit <- survfit(Surv(time, status) ~ rx, data = colon)
  w <- character(0)
  p <- withCallingHandlers(
    ggsurvplot_facet(fit, colon, facet.by = "sex", pval = TRUE),
    warning = function(cnd) { w <<- c(w, conditionMessage(cnd)); invokeRestart("muffleWarning") }
  )
  expect_false(any(grepl("not substituted", w)))
  # the p-value text layer is present
  b <- ggplot2::ggplot_build(p)
  labs <- unlist(lapply(b$data, function(d) if ("label" %in% names(d)) d$label))
  expect_true(any(grepl("^p", labs)))
})

test_that("no-regression: pval = FALSE draws no p-value (#636)", {
  fit <- survfit(Surv(time, status) ~ rx, data = colon)
  expect_error(ggsurvplot_facet(fit, colon, facet.by = "sex", pval = FALSE), NA)
})
