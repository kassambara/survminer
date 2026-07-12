context("linetype vector with hex dash patterns (#344)")

# #344: ggsurvplot(..., linetype = c("solid", "F1")) crashed with
# "the condition has length > 1". .get_lty() only mapped a per-strata vector to
# scale_linetype_manual when every value was a base line-type name (or all
# numeric); a vector containing a hex dash pattern fell through and was returned
# as a length-2 value that later hit `if (linetype == "strata")`. Any length > 1
# linetype is now treated as a per-strata manual vector.
library(survival)

fit <- survfit(Surv(time, status) ~ sex, data = lung)

test_that("a linetype vector containing a hex pattern renders (#344)", {
  expect_error(
    ggplot2::ggplotGrob(ggsurvplot(fit, data = lung, linetype = c("solid", "F1"))$plot),
    NA)
  expect_error(
    ggplot2::ggplotGrob(ggsurvplot(fit, data = lung, linetype = c("F1", "4C"))$plot),
    NA)
})

test_that("the hex dash patterns are applied per strata (#344)", {
  ld <- ggplot2::layer_data(
    ggsurvplot(fit, data = lung, linetype = c("solid", "F1"))$plot, 1)
  # two strata drawn with two distinct linetypes (not a single recycled value)
  expect_equal(length(unique(ld$linetype)), 2L)
})

test_that("no-regression: numeric and base-name vectors still work (#344)", {
  expect_equal(.get_lty(c(1, 2))$lty, "strata")
  expect_equal(.get_lty(c(1, 2))$lty.manual, c(1, 2))
  expect_equal(.get_lty(c("solid", "dashed"))$lty, "strata")
  expect_equal(.get_lty(c("solid", "dashed"))$lty.manual, c("solid", "dashed"))
  expect_error(ggplot2::ggplotGrob(ggsurvplot(fit, data = lung, linetype = c(1, 2))$plot), NA)
})

test_that("no-regression: a single linetype value is passed through unchanged (#344)", {
  expect_equal(.get_lty("strata")$lty, "strata")
  expect_null(.get_lty("strata")$lty.manual)
  expect_equal(.get_lty("dashed")$lty, "dashed")
  expect_null(.get_lty("dashed")$lty.manual)
  expect_equal(.get_lty("F1")$lty, "F1")
  expect_null(.get_lty("F1")$lty.manual)
})
