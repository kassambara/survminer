context("weighted cumulative event/censor counts")

# Regression test for #560 (and its duplicate #554): with a weighted survfit,
# the cumulative number-of-events / number-censored columns were the raw
# cumulative sums of the (fractional) weighted counts, so the risk/cumevents
# tables showed decimals (e.g. 30.89) while the per-interval n.event/n.censor
# were rounded. The cumulative columns are now rounded to the same precision.
library(survival)

test_that("weighted cumulative counts are rounded like their siblings (#560, #554)", {
  set.seed(1)
  d <- lung
  d$w <- runif(nrow(d), 0.5, 1.5)
  fw <- survfit(Surv(time, status) ~ sex, data = d, weights = w)
  ss <- survminer:::.get_timepoints_survsummary(fw, d, times = c(0, 200, 400, 600))
  expect_equal(ss$cum.n.event,  round(ss$cum.n.event))
  expect_equal(ss$cum.n.censor, round(ss$cum.n.censor))
})

test_that("no-regression: unweighted cumulative counts unchanged (#560)", {
  fu <- survfit(Surv(time, status) ~ sex, data = lung)
  ss <- survminer:::.get_timepoints_survsummary(fu, lung, times = c(0, 200, 400, 600))
  # integers before and after (round of an integer is identity)
  expect_equal(ss$cum.n.event,  c(0, 54, 89, 103, 0, 18, 37, 45))
  expect_equal(ss$cum.n.censor, c(0, 6, 18, 22, 0, 6, 27, 34))
})
