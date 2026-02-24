test_that("basic KM plot runs without {lifecycle} warnings", {
  rlang::local_options(lifecycle_verbosity = "warning")

  expect_no_warning(
    {
      ggsurvplot(
        fit = survfit(Surv(time = time, event = status) ~ x, data = aml),
        surv.median.line = "hv"
      )
    },
    class = "lifecycle_warning_deprecated"
  )
})

test_that("basic KM plot runs without {lifecycle} errors", {
  expect_no_error(
    {
      ggsurvplot(
        fit = survfit(Surv(time = time, event = status) ~ x, data = aml),
        surv.median.line = "hv"
      )
    },
    class = "lifecycle_error_deprecated"
  )
})
