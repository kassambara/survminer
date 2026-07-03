context("weighted pct.risk denominator")

# Regression test for #561: the percentage at risk was computed as
# n.risk * 100 / fit$n, but fit$n is the UNWEIGHTED subject count while n.risk is
# weighted for a weighted survfit(), so pct.risk could exceed 100%. The
# denominator now falls back to the weighted number at risk at the origin ONLY
# when the weighting makes n.risk exceed fit$n; unweighted fits keep fit$n and
# are byte-identical to before.
library(survival)

test_that("weighted pct.risk stays <= 100% and starts at 100 (#561)", {
  d <- lung
  d$w <- rep(2, nrow(d))  # constant weights -> n.risk = 2 * count, clearly > fit$n
  fw <- survfit(Surv(time, status) ~ sex, data = d, weights = w)
  tp <- survminer:::.get_timepoints_survsummary(fw, d, times = c(0, 200, 400))
  expect_true(all(tp$pct.risk <= 100))
  expect_true(all(tp$pct.risk[tp$time == 0] == 100))
})

test_that("weighted risk.table='percentage' render stays <= 100% (#561)", {
  d <- lung
  d$w <- rep(2, nrow(d))
  fw <- survfit(Surv(time, status) ~ sex, data = d, weights = w)
  p <- ggsurvplot(fw, data = d, risk.table = "percentage", break.time.by = 200)
  labs <- suppressWarnings(as.numeric(p$table$data$llabels))
  expect_true(all(labs[!is.na(labs)] <= 100))
  expect_true(all(labs[p$table$data$time == 0] == 100))
})

test_that("no-regression: unweighted pct.risk is unchanged (#561)", {
  fu <- survfit(Surv(time, status) ~ sex, data = lung)
  tu <- survminer:::.get_timepoints_survsummary(fu, lung, times = c(0, 200, 400))
  s  <- summary(fu, times = c(0, 200, 400), extend = TRUE)
  expected <- round(s$n.risk * 100 / rep(fu$n, each = 3))
  expect_identical(tu$pct.risk, expected)
})

test_that("no-regression: unweighted left-truncated pct.risk uses fit$n (#561)", {
  # For counting-process (left-truncated) data the number at risk at time 0 can
  # be below fit$n; the guard must NOT fire here, so the old fit$n denominator is
  # preserved (byte-identical to previous behaviour).
  skip_if_not(exists("heart"))
  ft <- survfit(Surv(start, stop, event) ~ surgery, data = heart)
  th <- survminer:::.get_timepoints_survsummary(ft, heart, times = c(0, 50, 100))
  s  <- summary(ft, times = c(0, 50, 100), extend = TRUE)
  expected <- round(s$n.risk * 100 / rep(ft$n, each = 3))
  expect_identical(th$pct.risk, expected)
})

test_that("no-regression: weighted null model pct.risk <= 100 (#561)", {
  d <- lung
  d$w <- rep(2, nrow(d))
  fn <- survfit(Surv(time, status) ~ 1, data = d, weights = w)
  tn <- survminer:::.get_timepoints_survsummary(fn, d, times = c(0, 200, 400))
  expect_true(all(tn$pct.risk <= 100))
  expect_true(all(tn$pct.risk[tn$time == 0] == 100))
})
