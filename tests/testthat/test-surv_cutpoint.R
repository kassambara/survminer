context("surv_cutpoint")

# Regression tests for:
#  - #609: surv_categorize() failed to dichotomize a variable whose name
#    contains characters make.names() mangles (e.g. a hyphen "A1BG-AS1"),
#    because summary.surv_cutpoint() built its row names via as.data.frame()
#    without check.names = FALSE, so the name no longer matched.
#  - #598: surv_cutpoint() passed a non-existent `alpha` object to
#    maxstat::maxstat.test(); it only worked by lazy evaluation. The unused
#    argument is removed.

test_that("surv_cutpoint()/surv_categorize() handle names with hyphens (#609)", {
  set.seed(1)
  mye <- survminer::myeloma
  d <- data.frame(time = mye$time, event = mye$event,
                  `A1BG-AS1` = mye$DEPDC1, check.names = FALSE)
  cut <- surv_cutpoint(d, time = "time", event = "event", variables = "A1BG-AS1")
  # summary keeps the hyphenated name (not "A1BG.AS1")
  expect_identical(rownames(summary(cut)), "A1BG-AS1")
  # and surv_categorize dichotomizes it (not raw numeric values)
  cats <- surv_categorize(cut)[["A1BG-AS1"]]
  expect_setequal(unique(cats), c("high", "low"))
})

test_that("no-regression: ordinary variable names still work (#609/#598)", {
  mye <- survminer::myeloma
  cut <- surv_cutpoint(mye, time = "time", event = "event",
                       variables = c("DEPDC1", "WHSC1"))
  s <- summary(cut)
  expect_identical(rownames(s), c("DEPDC1", "WHSC1"))
  expect_true(is.numeric(s$cutpoint) && all(is.finite(s$cutpoint)))
  cats <- surv_categorize(cut)$DEPDC1
  expect_setequal(unique(cats), c("high", "low"))
})

test_that("surv_cutpoint() runs without the undefined alpha argument (#598)", {
  mye <- survminer::myeloma
  expect_error(surv_cutpoint(mye, time = "time", event = "event",
                             variables = "DEPDC1"), NA)
})
