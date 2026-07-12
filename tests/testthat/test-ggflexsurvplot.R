context("ggflexsurvplot")

# Regression test for #408: a factor grouping variable was silently collapsed to
# a single "All" stratum because is_factor_or_character() called ggplot2's
# is.facet() (a Facet-object test, always FALSE for a data column) instead of
# is.factor(). The helper must recognise factors AND characters, but not numerics.
test_that("is_factor_or_character recognises factors and characters (#408)", {
  expect_true(is_factor_or_character(factor(c("a", "b", "a"))))
  expect_true(is_factor_or_character(c("a", "b", "a")))
  # no-regression: a numeric vector is not a grouping variable -> FALSE (unchanged)
  expect_false(is_factor_or_character(1:5))
  expect_false(is_factor_or_character(c(1.5, 2.5)))
})

# Regression test for #436: ggflexsurvplot() called .extract.survfit(fit) without
# forwarding the data it had already resolved, so .extract.survfit() re-derived the
# data from fit$call$data and errored with "object '<name>' not found" when the
# original data object was out of scope at plot time -- even when the user passed
# data = explicitly. The resolved data must be forwarded.
test_that("ggflexsurvplot() forwards user data to .extract.survfit (#436)", {
  skip_if_not_installed("flexsurv")
  # Build the fit inside a local scope so fit$call$data is NOT visible at plot time.
  make_fit <- function() {
    dd <- survival::lung
    dd$sex <- factor(dd$sex, labels = c("m", "f"))
    flexsurv::flexsurvreg(survival::Surv(time, status) ~ sex, data = dd,
                          dist = "weibull")
  }
  fit <- make_fit()
  dat <- survival::lung
  dat$sex <- factor(dat$sex, labels = c("m", "f"))

  # With an explicit data = argument this must no longer error, and must group.
  p <- suppressWarnings(ggflexsurvplot(fit, data = dat))
  expect_length(unique(as.character(p$plot$data$strata)), 2)
})
