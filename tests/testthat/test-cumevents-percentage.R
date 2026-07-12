context("cumevents / cumcensor tables as percentage (#499)")

# #499: the cumulative events/censor tables only showed absolute counts. They
# now accept the same character values as risk.table -- "absolute" (default,
# unchanged), "percentage", "abs_pct" -- shown as a percent of the stratum size.
library(survival)

fit <- survfit(Surv(time, status) ~ sex, data = lung)

lab_of <- function(p) {
  d <- ggplot2::ggplot_build(p)$data
  i <- which(vapply(d, function(x) "label" %in% names(x), logical(1)))[1]
  d[[i]]$label
}

test_that("no-regression: cumevents = TRUE equals cumevents = 'absolute' (#499)", {
  a <- lab_of(ggsurvplot(fit, data = lung, cumevents = TRUE, break.time.by = 250)$cumevents)
  b <- lab_of(ggsurvplot(fit, data = lung, cumevents = "absolute", break.time.by = 250)$cumevents)
  expect_equal(a, b)
})

test_that("cumevents = 'percentage' shows count / stratum size * 100 (#499)", {
  p   <- ggsurvplot(fit, data = lung, cumevents = "percentage", break.time.by = 250)
  ss  <- .get_timepoints_survsummary(fit, lung, c(0, 250, 500, 750, 1000))
  exp <- round(ss$cum.n.event / ss$strata_size * 100)
  expect_setequal(as.numeric(lab_of(p$cumevents)), exp)
  expect_match(p$cumevents$labels$title, "%", fixed = TRUE)   # title reflects %
})

test_that("cumevents = 'abs_pct' shows 'count (pct)' (#499)", {
  p <- ggsurvplot(fit, data = lung, cumevents = "abs_pct", break.time.by = 250)
  expect_true(any(grepl("\\(.*\\)", lab_of(p$cumevents))))
})

test_that("cumcensor accepts percentage too and builds (#499)", {
  p <- ggsurvplot(fit, data = lung, cumcensor = "percentage", break.time.by = 250)
  expect_error(ggplot2::ggplotGrob(p$ncensor.plot), NA)   # cumcensor returns as $ncensor.plot
})

test_that("an invalid cumevents string errors clearly (#499)", {
  expect_error(ggsurvplot(fit, data = lung, cumevents = "bogus"), "Allowed values")
})
