context("risk table strata_size / pct.risk for id-based counting-process fits (#592)")

# #592: for an id-based counting-process fit (Surv(start, stop, event) ~ g,
# id = subject; multi-row per subject), fit$n counts intervals (rows), not
# subjects, so strata_size and pct.risk over-counted (pct did not start at 100%).
# strata_size now uses fit$n.id (per-stratum unique-subject count) when present.
# fit$n.id is NULL for ordinary fits, so they are unchanged.
library(survival)

test_that("id counting-process fit: strata_size = unique subjects, pct starts at 100 (#592)", {
  fit <- survfit(Surv(start, stop, event) ~ rx, data = bladder2, id = id)
  st  <- ggsurvplot(fit, data = bladder2, risk.table = TRUE, break.time.by = 10)$data.survtable
  s0  <- st[st$time == 0, ]
  expect_equal(as.numeric(s0$strata_size), as.numeric(fit$n.id))  # subjects, not fit$n rows
  expect_true(all(as.numeric(fit$n.id) < fit$n))                  # intervals really exceed subjects
  expect_equal(as.numeric(s0$pct.risk), rep(100, nrow(s0)))       # starts at 100%
})

test_that("single-group id fit uses subject count (#592)", {
  f1 <- survfit(Surv(start, stop, event) ~ 1, data = bladder2, id = id)
  st <- ggsurvplot(f1, data = bladder2, risk.table = TRUE)$data.survtable
  expect_equal(unique(as.numeric(st$strata_size)), as.numeric(f1$n.id))
})

test_that("no-regression: ordinary fits keep strata_size = fit$n (#592)", {
  # standard right-censored, single-group, and plain left-truncated (NO id) all
  # have fit$n.id == NULL -> strata_size stays fit$n (byte-identical).
  fs <- survfit(Surv(time, status) ~ sex, data = lung)
  expect_null(fs$n.id)
  st <- ggsurvplot(fs, data = lung, risk.table = TRUE)$data.survtable
  expect_equal(sort(unique(as.numeric(st$strata_size))), sort(as.numeric(fs$n)))

  # left-truncated without id must NOT be treated as an id fit
  fl <- survfit(Surv(start, stop, event) ~ 1, data = heart)
  expect_null(fl$n.id)
  stl <- ggsurvplot(fl, data = heart, risk.table = TRUE)$data.survtable
  expect_equal(unique(as.numeric(stl$strata_size)), as.numeric(fl$n))
})
