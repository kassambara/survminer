context("ggforest sample size excludes missing values")

# Regression test for #597: ggforest() reported a sample size counting all rows
# of `data`, but coxph() drops rows with missing values in any model variable
# (na.action = na.omit). The reported N now reflects the complete cases the model
# actually used (model$n).
library(survival)

# Collect all text labels drawn in a ggforest plot.
.ggforest_labels <- function(p) {
  g <- grid::grid.force(ggplot2::ggplotGrob(p))
  out <- character(0)
  walk <- function(gr) {
    if (!is.null(gr$children)) for (ch in gr$children) walk(ch)
    if (!is.null(gr$label)) out[[length(out) + 1]] <<- paste(gr$label, collapse = "|")
  }
  walk(g)
  unlist(out)
}

test_that("ggforest N reflects complete cases used by the model (#597)", {
  data(cancer, package = "survival")
  d <- subset(colon, select = c(sex, adhere, time, status))
  set.seed(1)
  d$adhere[sample(nrow(d), 200)] <- NA
  m <- coxph(Surv(time, status) ~ factor(sex) + factor(adhere), data = d)
  expect_true(m$n < nrow(d))                    # coxph dropped the NA rows

  labs <- .ggforest_labels(ggforest(m, data = d))
  # complete-case per-level counts (sum to model$n = 1658)
  expect_true(any(grepl("N=800", labs)))
  expect_true(any(grepl("N=858", labs)))
  # the old full-data counts (nrow = 1858; sex 890/968) must NOT appear
  expect_false(any(grepl("N=890", labs)))
  expect_false(any(grepl("N=968", labs)))
  expect_false(any(grepl("N=1858", labs)))
})

test_that("no-regression: ggforest N unchanged when there are no missing values (#597)", {
  m <- coxph(Surv(time, status) ~ factor(sex) + factor(adhere), data = colon)
  expect_equal(m$n, nrow(colon))                # nothing dropped
  labs <- .ggforest_labels(ggforest(m, data = colon))
  # full-data per-level counts appear unchanged (sex 890/968)
  expect_true(any(grepl("N=890", labs)))
  expect_true(any(grepl("N=968", labs)))
})
