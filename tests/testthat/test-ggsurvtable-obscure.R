context("ggsurvtable small-cell suppression (obscure.less.than)")

# Small-cell suppression for disclosure control (#637, requested by @jwallib):
# counts below `obscure.less.than` are shown as "<n" so small, potentially
# patient-identifying cell counts are hidden. Default (NULL) changes nothing.
library(survival)

# the drawn text labels of a survtable ggplot, in row order
tbl_labels <- function(p) ggplot2::ggplot_build(p)$data[[1]]$label

fit <- survfit(Surv(time, status) ~ sex, data = lung)

test_that("default (obscure.less.than = NULL) leaves the table unchanged", {
  a <- tbl_labels(ggsurvtable(fit, data = lung, survtable = "risk.table"))
  b <- tbl_labels(ggsurvtable(fit, data = lung, survtable = "risk.table",
                              obscure.less.than = NULL))
  expect_identical(a, b)
  expect_false(any(grepl("^<", a)))            # no masking marker at all
})

test_that("risk table masks small at-risk counts, keeps 0 and large counts", {
  labs <- tbl_labels(ggsurvtable(fit, data = lung, survtable = "risk.table",
                                 obscure.less.than = 5))
  # lung sex fit n.risk at the default breaks include 2 and 3 (-> "<5"), a 0
  # (kept), and large counts like 138 (kept, NOT masked -- guards against a
  # lexical "138" < "5" comparison, which was a bug in the original PR #637).
  expect_true("<5" %in% labs)
  expect_true("138" %in% labs)                 # numeric, not lexical, comparison
  expect_true("0" %in% labs)                   # zero kept by default
  expect_false(any(labs %in% c("2", "3")))     # the small counts are hidden
})

test_that("obscure.zero = TRUE also masks a count of 0", {
  labs <- tbl_labels(ggsurvtable(fit, data = lung, survtable = "risk.table",
                                 obscure.less.than = 5, obscure.zero = TRUE))
  expect_false("0" %in% labs)
  expect_true("<5" %in% labs)
})

test_that("cumcensor masks the censor count, not the event count (#637 fix)", {
  # The original PR modified cum.n.event in the cumcensor branch, so suppression
  # silently did nothing there. Threshold 20 catches the cum.n.censor values 11
  # and 13 for this fit; confirm they are masked.
  labs <- tbl_labels(ggsurvtable(fit, data = lung, survtable = "cumcensor",
                                 obscure.less.than = 20))
  expect_true("<20" %in% labs)
  expect_false(any(labs %in% c("11", "13")))
})

test_that("cumevents masks the cumulative event count", {
  labs <- tbl_labels(ggsurvtable(fit, data = lung, survtable = "cumevents",
                                 obscure.less.than = 30))
  # cum.n.event includes 24 (-> "<30") and 0 (kept) for this fit
  expect_true("<30" %in% labs)
  expect_true("0" %in% labs)
})

test_that("nrisk_cumevents masks each count in the cell independently", {
  labs <- tbl_labels(ggsurvtable(fit, data = lung, survtable = "risk.table",
                                 risk.table.type = "nrisk_cumevents",
                                 obscure.less.than = 70))
  # both components can be masked: a cell where n.risk and the event count are
  # both < 70 becomes "<70 (<70)"; a large n.risk beside a small count is not
  # over-hidden (only the small side is masked).
  expect_true(any(labs == "<70 (<70)"))
  expect_true(any(grepl("^138 ", labs)))       # n.risk 138 kept, not masked
})

test_that("a numeric-string threshold is coerced (numeric, not lexical, compare)", {
  # "5" must behave exactly like 5, not trigger a lexical comparison that would
  # mask 138 (as "138" < "5" lexically).
  a <- tbl_labels(ggsurvtable(fit, data = lung, survtable = "risk.table",
                              obscure.less.than = "5"))
  b <- tbl_labels(ggsurvtable(fit, data = lung, survtable = "risk.table",
                              obscure.less.than = 5))
  expect_identical(a, b)
  expect_true("138" %in% a)
})

test_that("a non-numeric threshold errors instead of mis-masking", {
  expect_error(
    ggsurvtable(fit, data = lung, survtable = "risk.table",
                obscure.less.than = "abc"),
    "must be a single number")
})

test_that("a factor threshold is read by its label, not its integer code", {
  # as.numeric(factor('5')) is the level code (1), which would silently
  # under-mask; the coercion reads the label so factor('5') == 5.
  a <- tbl_labels(ggsurvtable(fit, data = lung, survtable = "risk.table",
                              obscure.less.than = factor("5")))
  b <- tbl_labels(ggsurvtable(fit, data = lung, survtable = "risk.table",
                              obscure.less.than = 5))
  expect_identical(a, b)
})

test_that("a multi-value threshold errors (single number contract)", {
  expect_error(
    ggsurvtable(fit, data = lung, survtable = "risk.table",
                obscure.less.than = c(3, 5)),
    "must be a single number")
})

test_that("suppression forwards through ggsurvplot(risk.table = TRUE, ...)", {
  p  <- ggsurvplot(fit, data = lung, risk.table = TRUE, obscure.less.than = 5)
  p0 <- ggsurvplot(fit, data = lung, risk.table = TRUE)
  expect_true("<5" %in% tbl_labels(p$table))
  expect_false(any(grepl("^<", tbl_labels(p0$table))))   # default off
})
