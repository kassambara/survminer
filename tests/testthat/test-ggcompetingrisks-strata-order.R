context("ggcompetingrisks keeps model strata order in facets")

# Regression test for #470: ggcompetingrisks() on a multi-state (survfitms)
# competing-risks fit labelled its facets from a character `strata` column, so
# facet_wrap() sorted the panels alphabetically instead of following the model's
# strata order. The strata column is now a factor with levels = names(fit$strata),
# so the panels keep the model order. When the model order already is
# alphabetical the facet order is unchanged.
library(survival)

facet_order <- function(p) levels(factor(ggplot2::ggplot_build(p)$plot$data$strata))

make_fit <- function(levels_order) {
  set.seed(1)
  n <- 150
  ev  <- factor(sample(c("censor", "relapse", "death"), n, replace = TRUE),
                levels = c("censor", "relapse", "death"))
  grp <- factor(sample(levels_order, n, replace = TRUE), levels = levels_order)
  survfit(Surv(rexp(n, 0.1), ev) ~ grp)
}

test_that("facets follow the model's (non-alphabetical) strata order (#470)", {
  fit <- make_fit(c("Zhigh", "Mmid", "Alow"))
  p <- ggcompetingrisks(fit)
  expect_identical(facet_order(p), names(fit$strata))     # model order, not sorted
  expect_false(identical(facet_order(p), sort(names(fit$strata))))
})

test_that("no-regression: alphabetical strata order is unchanged (#470)", {
  fit <- make_fit(c("a", "b", "c"))
  p <- ggcompetingrisks(fit)
  expect_identical(facet_order(p), names(fit$strata))     # == sorted here
})

test_that("no-regression: a single-group (no strata) fit still draws (#470)", {
  set.seed(1); n <- 100
  ev <- factor(sample(c("censor", "relapse", "death"), n, replace = TRUE),
               levels = c("censor", "relapse", "death"))
  fit <- survfit(Surv(rexp(n, 0.1), ev) ~ 1)
  expect_error(ggplot2::ggplot_build(ggcompetingrisks(fit)), NA)
})
