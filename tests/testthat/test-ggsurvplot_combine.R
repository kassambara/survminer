start_time <- 250
fit1 <- survfit(Surv(time, status) ~ sex, data = lung)
fit2 <- survfit(Surv(time, status) ~ sex, data = lung, start.time = start_time)

test_that("survplot_combine plots successfully into 4 lines; second 2 fits have only (0,1) before start_time", {
  p <- ggsurvplot_combine(list(
      original=fit1, conditional=fit2
    ), data = lung)
  .build <- ggplot_build(p$plot)
  .build_data <- .build$data[[1]]
  expect_equal(length(unique(.build_data[['group']])), 4)
  expect_lt(nrow(.build_data[(.build_data[['group']] >= 3) &
                               (.build_data[['x']] < start_time), ]), 3)
})

test_that("survplot_combine includes dataframes when keep.data==TRUE", {
  p <- ggsurvplot_combine(list(
    original=fit1, conditional=fit2
  ), data = lung, keep.data = TRUE)
  expect_equal(length(names(p)), 3)
})
