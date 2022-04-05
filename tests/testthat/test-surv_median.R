test_that("surv_median calculates medians", {
  df_medians <- surv_median(survfit(Surv(time, status) ~ sex,
                                    data=lung))
  expect_equal(df_medians[['median']], c(270,426))
  expect_lt(df_medians[['lower']][2], 426)
})
