context("pairwise_survdiff")

# Regression test for #648: pairwise_survdiff() errored with "undefined columns
# selected" when the formula contained a strata() term (e.g. ~ rx + strata(sex)),
# because strata(sex) is not a data column. strata() terms are now separated
# from the grouping variable and kept in the survdiff formula (a stratified
# pairwise test).
library(survival)

test_that("pairwise_survdiff() works with a strata() term (#648)", {
  expect_error(
    res <- pairwise_survdiff(Surv(time, status) ~ rx + strata(sex), data = colon),
    NA
  )
  expect_s3_class(res, "pairwise.htest")
  # the comparison is over the grouping variable rx (3 levels -> 2x2 matrix)
  expect_equal(rownames(res$p.value), c("Lev", "Lev+5FU"))
  expect_equal(colnames(res$p.value), c("Obs", "Lev"))
})

test_that("no-regression: single grouping variable is unchanged (#648)", {
  res <- pairwise_survdiff(Surv(time, status) ~ rx, data = colon)
  expect_s3_class(res, "pairwise.htest")
  # Lev vs Obs is not significant; Lev+5FU vs Obs is highly significant
  expect_gt(res$p.value["Lev", "Obs"], 0.5)
  expect_lt(res$p.value["Lev+5FU", "Obs"], 1e-6)
})

test_that("no-regression: two grouping variables still work (#648)", {
  expect_error(
    pairwise_survdiff(Surv(time, status) ~ rx + adhere, data = colon), NA)
})

test_that("a formula with only strata() terms errors clearly (#648)", {
  expect_error(
    pairwise_survdiff(Surv(time, status) ~ strata(sex), data = colon),
    "at least one grouping variable"
  )
})
