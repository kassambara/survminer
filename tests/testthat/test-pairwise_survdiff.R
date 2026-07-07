context("pairwise_survdiff")

# Regression test for #648: pairwise_survdiff() errored with "undefined columns
# selected" when the formula contained a strata() term (e.g. ~ rx + strata(sex)),
# because strata(sex) is not a data column. strata() terms are now separated
# from the grouping variable and kept in the survdiff formula (a stratified
# pairwise test).
library(survival)

test_that("pairwise_survdiff() works with a strata() term (#648)", {
  expect_error(
    res <- pairwise_survdiff(Surv(time, status) ~ rx + strata(sex), data = colon),
    NA
  )
  expect_s3_class(res, "pairwise.htest")
  # the comparison is over the grouping variable rx (3 levels -> 2x2 matrix)
  expect_equal(rownames(res$p.value), c("Lev", "Lev+5FU"))
  expect_equal(colnames(res$p.value), c("Obs", "Lev"))
})

test_that("no-regression: single grouping variable is unchanged (#648)", {
  res <- pairwise_survdiff(Surv(time, status) ~ rx, data = colon)
  expect_s3_class(res, "pairwise.htest")
  # Lev vs Obs is not significant; Lev+5FU vs Obs is highly significant
  expect_gt(res$p.value["Lev", "Obs"], 0.5)
  expect_lt(res$p.value["Lev+5FU", "Obs"], 1e-6)
})

test_that("no-regression: two grouping variables still work (#648)", {
  expect_error(
    pairwise_survdiff(Surv(time, status) ~ rx + adhere, data = colon), NA)
})

test_that("a formula with only strata() terms errors clearly (#648)", {
  expect_error(
    pairwise_survdiff(Surv(time, status) ~ strata(sex), data = colon),
    "at least one grouping variable"
  )
})

test_that("a survival::strata() adjustment term is handled like strata() (#672)", {
  # #672 (flagged by T. Therneau): a survival::strata() term in the formula was
  # not matched by the strata detector, so it was mistaken for a grouping
  # variable -> "undefined columns selected". The survival:: prefix is now
  # recognised and stripped; the result must equal the bare strata() form.
  expect_error(
    res_q <- pairwise_survdiff(Surv(time, status) ~ rx + survival::strata(sex),
                               data = colon, p.adjust.method = "none"),
    NA
  )
  expect_s3_class(res_q, "pairwise.htest")
  # comparison is still over the grouping variable rx (not the strata term)
  expect_equal(rownames(res_q$p.value), c("Lev", "Lev+5FU"))
  expect_equal(colnames(res_q$p.value), c("Obs", "Lev"))
  # identical to the bare strata() form
  res_b <- pairwise_survdiff(Surv(time, status) ~ rx + strata(sex),
                             data = colon, p.adjust.method = "none")
  expect_equal(res_q$p.value, res_b$p.value)
})

test_that("no-regression: a grouping variable is never misread as strata (#672)", {
  # the widened detector must still only match strata()/survival::strata(), not
  # ordinary grouping variables.
  res <- pairwise_survdiff(Surv(time, status) ~ rx, data = colon)
  expect_equal(rownames(res$p.value), c("Lev", "Lev+5FU"))
})

test_that("rows with a missing grouping value are dropped (#635)", {
  # pairwise_survdiff() removes rows with NA in any grouping column before the
  # pairwise tests. This locks that behavior after switching the row-wise check
  # to anyNA() (byte-identical to the previous `NA %in% .row` for grouping
  # variables). A run with NAs must match a run on the NA-free subset.
  d <- colon
  set.seed(42)
  na_idx <- sample(nrow(d), 20)
  d$rx[na_idx] <- NA
  res_na  <- pairwise_survdiff(Surv(time, status) ~ rx, data = d)
  res_cc  <- pairwise_survdiff(Surv(time, status) ~ rx, data = d[!is.na(d$rx), ])
  expect_equal(res_na$p.value, res_cc$p.value)
  # NA is not carried in as a spurious extra group
  expect_false(anyNA(rownames(res_na$p.value)))
  expect_false(any(c("NA", NA) %in% rownames(res_na$p.value)))
})

test_that("rows with a missing value in ANY of several grouping vars are dropped (#635)", {
  # The drop matters most for the multiple-grouping-variable path: an undropped
  # NA would form a spurious combined strata() level. NAs seeded into different
  # rows of two grouping columns must give the same result as the complete-case
  # run, with no NA-bearing combined group.
  d <- colon
  set.seed(7)
  d$rx[sample(nrow(d), 15)]     <- NA
  d$adhere[sample(nrow(d), 15)] <- NA
  res_na <- pairwise_survdiff(Surv(time, status) ~ rx + adhere, data = d)
  cc <- d[!is.na(d$rx) & !is.na(d$adhere), ]
  res_cc <- pairwise_survdiff(Surv(time, status) ~ rx + adhere, data = cc)
  expect_equal(res_na$p.value, res_cc$p.value)
  expect_false(any(grepl("NA", rownames(res_na$p.value))))
  expect_false(any(grepl("NA", colnames(res_na$p.value))))
})
