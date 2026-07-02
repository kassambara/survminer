context("ggsurvplot_combine risk table type/fontsize")

# Regression tests for:
#  - #641: ggsurvplot_combine() ignored the risk-table type parsed from the
#    `risk.table` argument (e.g. "nrisk_cumcensor"), always using "absolute".
#  - #514: ggsurvplot_combine() ignored `risk.table.fontsize` (only `fontsize`
#    worked), so the risk-table text stayed at the default size.
library(survival)

make_fits <- function() {
  f1 <- survfit(Surv(time, status) ~ 1, data = lung[lung$sex == 1, ])
  f2 <- survfit(Surv(time, status) ~ 1, data = lung[lung$sex == 2, ])
  list(male = f1, female = f2)
}

risk_labels <- function(p) {
  lab <- ggplot2::ggplot_build(p$table)$data[[1]]$label
  lab[nzchar(lab)]
}
risk_size <- function(p) ggplot2::ggplot_build(p$table)$data[[1]]$size[1]

test_that("risk.table = 'nrisk_cumcensor' shows n (censored) in combine (#641)", {
  p   <- ggsurvplot_combine(make_fits(), data = lung, risk.table = "nrisk_cumcensor")
  lab <- risk_labels(p)
  expect_true(any(grepl("\\(", lab)))     # e.g. "138 (0)"
})

test_that("no-regression: default risk.table = TRUE stays absolute (#641)", {
  p   <- ggsurvplot_combine(make_fits(), data = lung, risk.table = TRUE)
  lab <- risk_labels(p)
  expect_false(any(grepl("\\(", lab)))    # plain counts, no "(censored)"
})

test_that("risk.table.fontsize is honoured in combine (#514)", {
  p_def <- ggsurvplot_combine(make_fits(), data = lung, risk.table = TRUE)
  p_big <- ggsurvplot_combine(make_fits(), data = lung, risk.table = TRUE,
                              risk.table.fontsize = 8)
  expect_equal(risk_size(p_big), 8)
  expect_false(isTRUE(all.equal(risk_size(p_def), 8)))  # default differs
})
