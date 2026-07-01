context("surv_group_by")

# Regression test for #548 / #670: surv_group_by() errored with
# "cannot xtfrm data frame" when the input was a tibble, because
# data[, grouping.vars] returns a one-column tibble (not a vector) which
# .levels() then tried to coerce with as.factor(). data[[grouping.vars]]
# returns a vector for both data.frame and tibble input.
test_that("surv_group_by() handles a tibble input (#548, #670)", {
  df <- survival::colon
  tb <- tibble::as_tibble(df)

  # tibble path must not error
  expect_error(surv_group_by(tb, "rx"), NA)

  # no-regression: data.frame path still works and tibble yields the SAME
  # group names / order (the fix must not change the data.frame result)
  g_df <- surv_group_by(df, "rx")
  g_tb <- surv_group_by(tb, "rx")
  expect_identical(names(g_tb$data), names(g_df$data))

  # two grouping variables also work for both inputs
  expect_error(surv_group_by(df, c("rx", "adhere")), NA)
  expect_error(surv_group_by(tb, c("rx", "adhere")), NA)
})
