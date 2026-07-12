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

# count how many times a given label is actually drawn (NOT de-duplicated), so a
# plot with two "reference" rows is distinguishable from one with a single row.
forest_label_count <- function(p, value) {
  gt <- grid::grid.force(ggplot2::ggplotGrob(p))
  n <- 0L
  walk <- function(gr) {
    if (inherits(gr, "gTree") && !is.null(gr$children))
      for (nm in names(gr$children)) walk(gr$children[[nm]])
    if (!is.null(gr$label)) n <<- n + sum(as.character(gr$label) == value)
  }
  walk(gt)
  n
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

# #404: with a non-default treatment base (contr.treatment(base = k)), coxph
# names the factor coefficients var + the contrast-matrix column names (level
# indices), e.g. ph.ecog1/ph.ecog3/ph.ecog4 for base = 2 on levels 0/1/2/3. The
# old paste0(var, level) match then tagged TWO levels as reference and silently
# dropped the last coefficient. The contrast-aware mapping routes each fitted
# coefficient to the level it belongs to.
test_that("ggforest() maps factor levels under a non-default treatment base (#404)", {
  lung2 <- lung
  lung2$ph.ecog <- factor(lung2$ph.ecog)                 # levels 0,1,2,3
  contrasts(lung2$ph.ecog) <- contr.treatment(4, base = 2)  # reference = level "1"
  m <- coxph(Surv(time, status) ~ sex + ph.ecog, data = lung2)
  p <- ggforest(m, data = lung2)
  labs <- forest_labels(p)
  # exactly one reference row is DRAWN (the buggy path drew two; count without
  # de-duplicating so the two collapsed grobs are still distinguishable).
  expect_equal(forest_label_count(p, "reference"), 1L)
  # every non-reference hazard ratio is drawn, including the last level's, which
  # the buggy path dropped. Ground truth: exp(coef) = 0.66 (level 0), 1.70
  # (level 2), 5.11 (level 3), matched against summary(m).
  hr <- round(exp(coef(m)[grep("ph.ecog", names(coef(m)))]), 2)
  expect_setequal(unname(hr), c(0.66, 1.70, 5.11))
  for (v in format(hr, nsmall = 2))
    expect_true(any(grepl(trimws(v), labs, fixed = TRUE)),
                info = paste("missing HR label", v))
})

test_that("ggforest() routes a non-syntactic factor name under a custom base (#404)", {
  set.seed(1)
  d <- lung
  d$`risk grp` <- factor(sample(c("lo", "mid", "hi"), nrow(d), replace = TRUE),
                         levels = c("lo", "mid", "hi"))
  contrasts(d$`risk grp`) <- contr.treatment(3, base = 2)   # reference = "mid"
  m <- coxph(Surv(time, status) ~ `risk grp`, data = d)
  p <- ggforest(m, data = d)
  # Without the backtick-tolerant model$assign lookup, none of the three levels'
  # coefficients match (the name is stored quoted), so all three render as
  # "reference"; the fix leaves exactly one reference row (the base, "mid").
  expect_equal(forest_label_count(p, "reference"), 1L)
})

test_that("no-regression: default contrasts keep the first level as reference (#404)", {
  lung2 <- lung
  lung2$ph.ecog <- factor(lung2$ph.ecog)                 # default: reference = "0"
  m <- coxph(Surv(time, status) ~ sex + ph.ecog, data = lung2)
  p <- ggforest(m, data = lung2)
  labs <- forest_labels(p)
  expect_equal(forest_label_count(p, "reference"), 1L)
  # standard default output: levels 1/2/3 -> 1.52 / 2.58 / 7.76
  for (v in c("1.52", "2.58", "7.76"))
    expect_true(any(grepl(v, labs, fixed = TRUE)), info = paste("missing", v))
})

test_that("no-regression: non-treatment contrasts (helmert) still render (#404)", {
  lung2 <- lung
  lung2$ph.ecog <- factor(lung2$ph.ecog)
  contrasts(lung2$ph.ecog) <- contr.helmert(4)           # falls back, no crash
  m <- coxph(Surv(time, status) ~ ph.ecog, data = lung2)
  expect_error(ggforest(m, data = lung2), NA)
})
