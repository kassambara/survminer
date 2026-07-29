.forest_labels <- function(model, data) {
  p <- ggforest(model, data = data)
  gt <- grid::grid.force(ggplot2::ggplotGrob(p)); L <- character()
  w <- function(g) { if (inherits(g, "gTree") && !is.null(g$children))
    for (n in names(g$children)) w(g$children[[n]])
    if (!is.null(g$label)) L <<- c(L, as.character(g$label)) }
  w(gt); L
}

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

test_that("a pspline() term does not mis-key the terms after it", {
  # model$assign indexes design columns (12 for pspline(age)) while broom::tidy()
  # collapses the term to 2 rows, so the old mapping ran off the end of the
  # coefficient table: pspline picked up NA rows and `sex` lost its fitted level.
  d <- na.omit(survival::lung[, c("time", "status", "age", "sex")])
  d$sex <- factor(d$sex, labels = c("M", "F"))
  m <- survival::coxph(survival::Surv(time, status) ~ pspline(age) + sex, data = d)
  expect_gt(length(stats::coef(m)), nrow(broom::tidy(m)))   # the mismatch itself

  labs <- function(p) {
    gt <- grid::grid.force(ggplot2::ggplotGrob(p)); L <- character()
    w <- function(g) { if (inherits(g, "gTree") && !is.null(g$children))
      for (n in names(g$children)) w(g$children[[n]])
      if (!is.null(g$label)) L <<- c(L, as.character(g$label)) }
    w(gt); L
  }
  L <- labs(ggforest(m, data = d))

  # exactly two reference rows: pspline's nonlin, and sex's baseline level
  expect_equal(sum(L == "reference"), 2L)
  # the fitted level keeps its hazard ratio, and no stray "sexF" variable row
  ref <- summary(m)$conf.int["sexF", ]
  expect_true(any(grepl(sprintf("%.1f", ref[["exp(coef)"]]), L, fixed = TRUE)))
  expect_false("sexF" %in% L)
  # both levels of sex are drawn, once each
  expect_equal(sum(L == "M"), 1L)
  expect_equal(sum(L == "F"), 1L)
})

test_that("a frailty() term is mapped the same way", {
  # frailty() mismatches in the other direction -- tidy() adds a row the
  # coefficient vector does not have -- so the alignment test must catch both.
  d <- na.omit(survival::lung[, c("time", "status", "age", "inst", "sex")])
  d$sex <- factor(d$sex, labels = c("M", "F"))
  m <- survival::coxph(survival::Surv(time, status) ~ frailty(inst) + sex, data = d)
  expect_true(nrow(broom::tidy(m)) != length(stats::coef(m)))

  labs <- function(p) {
    gt <- grid::grid.force(ggplot2::ggplotGrob(p)); L <- character()
    w <- function(g) { if (inherits(g, "gTree") && !is.null(g$children))
      for (n in names(g$children)) w(g$children[[n]])
      if (!is.null(g$label)) L <<- c(L, as.character(g$label)) }
    w(gt); L
  }
  L <- labs(ggforest(m, data = d))
  # sex still gets both levels and keeps its fitted hazard ratio
  expect_equal(sum(L == "M"), 1L)
  expect_equal(sum(L == "F"), 1L)
  ref <- summary(m)$conf.int["sexF", ]
  expect_true(any(grepl(sprintf("%.1f", ref[["exp(coef)"]]), L, fixed = TRUE)))
})

test_that("name matching on the penalised path cannot claim a longer term's coefficients", {
  # With a penalised term the coefficient rows are matched by name, so a variable
  # whose name is a prefix of another ("flag" vs "flag2") must not absorb the
  # other's coefficients -- the collision that made "add11" match "add17TRUE"
  # (#689). Logical variables are used because they are routed by coefficient name
  # rather than by contrast matrix, so a wrong match is not caught downstream.
  d <- na.omit(survival::lung[, c("time", "status", "age", "sex", "wt.loss")])
  d$flag  <- d$age > 60
  d$flag2 <- d$wt.loss > 0
  d$grp  <- factor(ifelse(d$sex == 1, "a", "b"))
  d$grp2 <- factor(ifelse(d$wt.loss > 0, "x", "y"))
  ml <- survival::coxph(survival::Surv(time, status) ~ pspline(age) + flag + flag2,
                        data = d)
  Ll <- .forest_labels(ml, d)
  expect_equal(sum(Ll == "flagTRUE"), 1L)
  expect_equal(sum(Ll == "flag2TRUE"), 1L)

  m <- survival::coxph(survival::Surv(time, status) ~ pspline(age) + grp + grp2,
                       data = d)
  expect_true(nrow(broom::tidy(m)) != length(stats::coef(m)))   # penalised path
  expect_true(any(startsWith(broom::tidy(m)$term, "grp")))      # the collision

  L <- .forest_labels(m, d)
  # each factor keeps exactly its own two levels, drawn once each
  for (lv in c("a", "b", "x", "y")) expect_equal(sum(L == lv), 1L, info = lv)
  # one reference per factor plus pspline's nonlin row
  expect_equal(sum(L == "reference"), 3L)
  # and each fitted level keeps its own hazard ratio
  ci <- summary(m)$conf.int
  for (nm in c("grpb", "grp2y"))
    expect_true(any(grepl(sprintf("%.2f", ci[nm, "exp(coef)"]), L, fixed = TRUE)),
                info = nm)
})
