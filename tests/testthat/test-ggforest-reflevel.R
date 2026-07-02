context("ggforest term extraction and reference level")

# Regression tests for:
#  - #240: ggforest() errored ("undefined columns selected") when the Cox
#    formula contained an in-formula transformation such as as.factor(x),
#    because the factor branch indexed data[, "as.factor(x)"] (a non-column).
#  - #312: the reference level of a factor inherited the statistics of a
#    prefix-colliding level (e.g. reference "grpBar" picked up "grpBarb"'s HR),
#    because coef[inds, ] used character row indexing, which partial-matches.
library(survival)

# collect all text labels drawn in a ggforest (grob-forced so annotations
# are materialised)
forest_labels <- function(p) {
  gt <- grid::grid.force(ggplot2::ggplotGrob(p))
  labs <- character(0)
  walk <- function(gr) {
    if (inherits(gr, "gTree") && !is.null(gr$children))
      for (nm in names(gr$children)) walk(gr$children[[nm]])
    if (!is.null(gr$label)) labs <<- c(labs, as.character(gr$label))
  }
  walk(gt)
  unique(labs[nzchar(labs)])
}

test_that("ggforest() works with an in-formula factor transformation (#240)", {
  m <- coxph(Surv(time, status) ~ age + as.factor(ph.ecog), data = lung)
  expect_error(ggforest(m, data = lung), NA)
})

test_that("no-regression: a factor column with a non-syntactic name renders (#240)", {
  set.seed(1)
  d <- lung
  d$`risk group` <- factor(sample(c("lo", "hi"), nrow(d), replace = TRUE))
  m <- coxph(Surv(time, status) ~ `risk group`, data = d)
  expect_error(p <- ggforest(m, data = d), NA)
  expect_true(any(grepl("reference", forest_labels(p))))
})

test_that("reference level does not inherit a prefix-colliding level (#312)", {
  set.seed(1)
  d <- lung
  d$grp <- factor(sample(c("Bar", "Barb", "Baz"), nrow(d), replace = TRUE))
  m <- coxph(Surv(time, status) ~ grp, data = d)      # "Bar" is the reference
  labs <- forest_labels(ggforest(m, data = d))
  # With the partial-match bug the reference row showed grpBarb's numeric HR, so
  # no "reference" label was drawn. Exact matching restores it.
  expect_true(any(grepl("reference", labs)))
})

test_that("no-regression: ordinary factor model renders with a reference (#312)", {
  m <- coxph(Surv(time, status) ~ sex + rx, data = colon)
  expect_error(p <- ggforest(m, data = colon), NA)
  expect_true(any(grepl("reference", forest_labels(p))))
})
