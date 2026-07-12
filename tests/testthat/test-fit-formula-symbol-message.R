context("actionable message when a survfit formula can't be recovered (#533)")

# #533: building survfit(form, data) with `form` a variable in a non-global
# scope (function / lapply / nest_by / {targets}) leaves fit$call$formula as a
# symbol that is out of scope at plot time. Extraction previously failed with
# the cryptic "object of type 'symbol' is not subsettable". .get_fit_formula()
# now raises an actionable message pointing to surv_fit(). Fits whose formula is
# recoverable (inline formula, or built with surv_fit()) are unchanged.
library(survival)

test_that("gone-scope symbol formula gives an actionable message, not cryptic (#533)", {
  mk  <- function() { f <- Surv(time, status) ~ sex; survfit(f, data = lung) }
  fit <- mk()   # `f` is gone
  expect_error(ggsurvplot(fit, data = lung),
               "could not be extracted")
  expect_error(ggsurvplot(fit, data = lung),
               "surv_fit")
  # the actionable guidance is what matters; the appended "Original error:"
  # text is survival-version/locale dependent, so it is not asserted on.
})

test_that("no-regression: inline-formula fits recover the formula unchanged (#533)", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  f <- .get_fit_formula(fit)
  expect_s3_class(f, "formula")
  expect_identical(attr(stats::terms(f), "term.labels"), "sex")
  # renders fine
  expect_error(ggplot2::ggplot_build(ggsurvplot(fit, data = lung)$plot), NA)
})

test_that("no-regression: surv_fit() many-models pattern works (#533)", {
  f  <- Surv(time, status) ~ sex
  sf <- surv_fit(f, data = lung)                 # surv_fit retains the formula
  expect_error(ggplot2::ggplot_build(ggsurvplot(sf, data = lung)$plot), NA)
  expect_s3_class(.get_fit_formula(sf), "formula")
})
