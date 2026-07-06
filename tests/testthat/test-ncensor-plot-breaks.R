context("ncensor.plot y-axis breaks don't overlap with many counts (#542)")

# #542: the "Number of censoring" panel set one y-axis break per distinct
# n.censor value. With many distinct counts the short panel crowds the integer
# labels and they overlap. .ncensor_y_breaks() keeps the original behaviour for
# few distinct counts (byte-identical) and falls back to ~5 evenly spaced
# integer breaks when there are many.
library(survival)

test_that("no-regression: <= 5 distinct counts keep one break per count (#542)", {
  expect_equal(.ncensor_y_breaks(c(0, 1, 2)), c(0, 1, 2))
  expect_equal(.ncensor_y_breaks(c(0, 0, 1, 2, 2)), c(0, 1, 2))     # unique-ed
  expect_equal(.ncensor_y_breaks(0:4), 0:4)                          # exactly 5
  expect_equal(.ncensor_y_breaks(c(0, 3, 9)), c(0, 3, 9))            # sparse, few
})

test_that("many distinct counts collapse to few spaced integer breaks (#542)", {
  b6 <- .ncensor_y_breaks(0:6)     # 7 distinct -> new path
  expect_true(length(b6) < 7)
  expect_true(all(b6 == round(b6)))            # integer breaks
  expect_true(min(b6) == 0 && max(b6) <= 6)    # within range
  expect_true(all(diff(b6) > 0))               # strictly increasing, no dupes

  b9 <- .ncensor_y_breaks(0:9)
  expect_true(length(b9) <= 6)
  expect_true(all(b9 == round(b9)))

  b40 <- .ncensor_y_breaks(seq(0, 40, by = 2))
  expect_equal(b40, c(0, 10, 20, 30, 40))
})

test_that("integration: ggsurvplot(ncensor.plot=TRUE) builds with de-crowded breaks (#542)", {
  fit <- survfit(Surv(time, status) ~ rx, data = colon)
  p <- ggsurvplot(fit, data = colon, ncensor.plot = TRUE)
  counts <- sort(unique(p$ncensor.plot$data$n.censor))
  skip_if_not(length(counts) > 5)   # colon must exercise the new path
  built <- ggplot2::ggplot_build(p$ncensor.plot)
  br <- built$layout$panel_params[[1]]$y$get_breaks()
  br <- br[!is.na(br)]
  expect_true(length(br) < length(counts))     # fewer labels than distinct counts
  expect_error(print(p), NA)                    # full compound object prints
})
