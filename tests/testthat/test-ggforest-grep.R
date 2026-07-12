context("ggforest coefficient-to-term mapping")

# Regression test for #689: for non-factor/non-numeric terms (e.g. logical
# covariates), ggforest() found the matching coefficients with
# grep("^var*.", coef$term). The trailing digits of a name were treated as a
# regex quantifier, so prefix-colliding names such as add11 / add17 each matched
# BOTH coefficients, producing duplicated/crossed forest rows. Coefficients are
# now mapped to their term via model$assign.
library(survival)

# all drawn text labels in a ggforest (grob-forced)
forest_labels <- function(p) {
  gt <- grid::grid.force(ggplot2::ggplotGrob(p))
  labs <- character(0)
  walk <- function(gr) {
    if (inherits(gr, "gTree") && !is.null(gr$children))
      for (nm in names(gr$children)) walk(gr$children[[nm]])
    if (!is.null(gr$label)) labs <<- c(labs, as.character(gr$label))
  }
  walk(gt)
  labs[nzchar(labs)]
}

test_that("prefix-colliding logical covariates are not duplicated (#689)", {
  set.seed(1)
  d <- lung
  d$add11 <- lung$age > 60
  d$add17 <- lung$wt.loss > 10
  m <- coxph(Surv(time, status) ~ add11 + add17, data = d)
  labs <- forest_labels(ggforest(m, data = d))
  # one "(N=...)" label per forest row: two covariates -> two rows (was four)
  expect_equal(sum(grepl("^\\(N=", labs)), 2L)
  expect_true(any(grepl("add11", labs)) && any(grepl("add17", labs)))
})

test_that("no-regression: ordered factor and numeric+logical mix render (#689)", {
  set.seed(1)
  d <- lung
  d$grade <- ordered(sample(c("lo", "mid", "hi"), nrow(d), replace = TRUE),
                     levels = c("lo", "mid", "hi"))
  expect_error(ggforest(coxph(Surv(time, status) ~ grade, data = d), data = d), NA)
  expect_error(ggforest(coxph(Surv(time, status) ~ age + I(age > 60), data = lung),
                        data = lung), NA)
})
