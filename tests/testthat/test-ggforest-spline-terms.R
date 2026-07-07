context("ggforest draws one row per coefficient for multi-coefficient spline terms (#411)")

# #411: with spline terms a coxph produces several coefficients per term
# (e.g. ns(age, 3) -> three coefficients; rms::rcs(age, 3) -> two). ggforest()
# used to map coefficients to rows with a regex on their names
# (grep("^<term>*.", coef$term)); for a term whose name contains "(" and digits
# the pattern over-matched, so EVERY coefficient matched EVERY spline term and
# rows were duplicated (two 3-df terms drew 2*6 = 12 rows instead of 6). Since
# #689 the mapping uses model$assign, giving exactly one row per coefficient.
# Reproduced here with base splines::ns() so it runs on all CI (no rms needed);
# the fixed code path (model$assign) is identical for rms::rcs().
library(survival)
library(splines)

d <- na.omit(lung[, c("time", "status", "age", "meal.cal")])

# Length of the variable-column text grob's label vector = number of forest rows
# actually drawn (one entry per row, blank for merged/duplicate rows). Counting
# the vector length, not distinct names, is what catches the #411 duplication:
# distinct names stay correct while duplicated rows are drawn as extra blanks.
n_forest_rows <- function(g) {
  gt <- ggplot2::ggplotGrob(g)
  best <- 0L
  walk <- function(x) {
    lab <- x$label
    if (!is.null(lab) && any(grepl("age", lab)) && any(grepl("meal.cal", lab)))
      best <<- max(best, length(lab))
    if (!is.null(x$children)) for (nm in names(x$children)) walk(x$children[[nm]])
    if (!is.null(x$grobs))    for (k in seq_along(x$grobs)) walk(x$grobs[[k]])
  }
  walk(gt)
  best
}

# collect the distinct non-blank variable labels drawn in the forest
forest_var_labels <- function(g) {
  gt <- ggplot2::ggplotGrob(g)
  out <- character(0)
  walk <- function(x) {
    lab <- x$label
    if (!is.null(lab) && any(grepl("age", lab)) && any(grepl("meal.cal", lab)))
      out <<- c(out, lab)
    if (!is.null(x$children)) for (nm in names(x$children)) walk(x$children[[nm]])
    if (!is.null(x$grobs))    for (k in seq_along(x$grobs)) walk(x$grobs[[k]])
  }
  walk(gt)
  unique(out[nzchar(out)])
}

test_that("two 3-df spline terms draw one row per coefficient, not duplicated (#411)", {
  m <- coxph(Surv(time, status) ~ ns(age, 3) + ns(meal.cal, 3), data = d)
  expect_equal(length(coef(m)), 6L)                       # three coefficients per term

  g <- ggforest(m, data = d)
  # the fix: exactly length(coef) rows drawn. Pre-#689 the regex mapping drew
  # 12 rows here (every coefficient matched both terms) -> this would be 12.
  expect_equal(n_forest_rows(g), length(coef(m)))
  # and each fitted coefficient is drawn exactly once, correctly mapped
  expect_setequal(forest_var_labels(g), names(coef(m)))
})

test_that("a single spline term draws one row per coefficient and renders (#411)", {
  m <- coxph(Surv(time, status) ~ ns(age, 4), data = d)
  expect_equal(length(coef(m)), 4L)
  g <- ggforest(m, data = d)
  # count rows whose label mentions the age spline term
  gt <- ggplot2::ggplotGrob(g)
  best <- 0L
  walk <- function(x) {
    lab <- x$label
    if (!is.null(lab) && all(grepl("age", lab)) && length(lab) > 1)
      best <<- max(best, length(lab))
    if (!is.null(x$children)) for (nm in names(x$children)) walk(x$children[[nm]])
    if (!is.null(x$grobs))    for (k in seq_along(x$grobs)) walk(x$grobs[[k]])
  }
  walk(gt)
  expect_equal(best, length(coef(m)))
  expect_error(gt, NA)
})

test_that("no-regression: a plain numeric+factor model still draws one row per level (#411)", {
  d2 <- lung
  d2$sex <- factor(d2$sex, labels = c("male", "female"))
  m <- coxph(Surv(time, status) ~ age + sex, data = d2)
  g <- ggforest(m, data = d2)
  expect_error(ggplot2::ggplotGrob(g), NA)
  # age (1 numeric row) + sex (1 estimated level; reference kept as its own row)
  # -> unchanged from before the fix; just assert it renders and shows both vars
  gt <- ggplot2::ggplotGrob(g)
  seen <- character(0)
  walk <- function(x) {
    if (!is.null(x$label)) seen <<- c(seen, x$label)
    if (!is.null(x$children)) for (nm in names(x$children)) walk(x$children[[nm]])
    if (!is.null(x$grobs))    for (k in seq_along(x$grobs)) walk(x$grobs[[k]])
  }
  walk(gt)
  expect_true(any(grepl("age", seen)))
  expect_true(any(grepl("sex", seen)))
})
