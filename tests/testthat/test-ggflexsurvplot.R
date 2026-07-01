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
