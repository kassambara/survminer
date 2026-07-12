context("surv_fit() resolves bare data-column arguments like survfit (#644, #571)")

# #644 / #571: surv_fit() force-evaluated `...` in the calling frame, so a bare
# data-column argument (weights = weights, id = subject) failed with
# "object '<col>' not found" or matched an unrelated object, whereas
# survfit()/coxph() resolve such arguments inside `data`. surv_fit() now
# evaluates the survfit data-column arguments (weights/subset/id/istate/etype)
# inside `data`, falling back to the caller for external objects/literals.
library(survival)

set.seed(1)
d <- lung
d$w    <- runif(nrow(d), 0.5, 2)
d$subj <- seq_len(nrow(d))

test_that("bare `weights = <col>` resolves in data and matches survfit (#644)", {
  sf <- surv_fit(Surv(time, status) ~ sex, data = d, weights = w)
  rf <- survfit(Surv(time, status) ~ sex, data = d, weights = w)
  expect_equal(sf$surv, rf$surv)
  expect_equal(sf$n, rf$n)
})

test_that("bare `id = <col>` resolves in data and matches survfit (#571)", {
  # (start, stop] multi-row-per-subject data, where id actually matters
  sf <- surv_fit(Surv(start, stop, event) ~ rx, data = bladder2, id = id)
  rf <- survfit(Surv(start, stop, event) ~ rx, data = bladder2, id = id)
  expect_equal(sf$surv, rf$surv)
  expect_equal(sf$n, rf$n)
})

test_that("no-regression: an external weights vector still works (#644)", {
  sf <- surv_fit(Surv(time, status) ~ sex, data = d, weights = d$w)
  rf <- survfit(Surv(time, status) ~ sex, data = d, weights = d$w)
  expect_equal(sf$surv, rf$surv)
})

test_that("no-regression: no `...`, list, and group.by paths are unchanged", {
  # no dots -> identical to survfit
  expect_equal(surv_fit(Surv(time, status) ~ sex, data = lung)$n,
               survfit(Surv(time, status) ~ sex, data = lung)$n)
  # literal option still evaluated in the caller
  expect_s3_class(surv_fit(Surv(time, status) ~ sex, data = lung, conf.int = 0.9),
                  "survfit")
  # list of formulas / list of data / group.by still return named lists
  expect_length(surv_fit(list(a = Surv(time, status) ~ sex,
                              b = Surv(time, status) ~ ph.ecog), data = lung), 2)
  expect_length(surv_fit(Surv(time, status) ~ sex, data = list(lung, lung)), 2)
  expect_true(length(surv_fit(Surv(time, status) ~ sex, data = colon,
                              group.by = "rx")) >= 1)
})

test_that("no-regression: the retry path does not alter list/facet dispatch", {
  # bare-column resolution is scoped to the single-data path; list/grouped
  # dispatch keeps the standard behaviour so the ggsurvplot_facet refit is
  # unchanged (guards the #556 path / the survfit conf.int forwarding).
  expect_length(surv_fit(Surv(time, status) ~ sex,
                         data = list(one = lung, two = lung)), 2)
})
