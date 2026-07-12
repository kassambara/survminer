context("ggforest draws interaction-term rows (#536)")

# #536: ggforest() iterated only over main-effect variables
# (attr(model$terms, "dataClasses")), so interaction terms (sex:ph.ecog) were
# silently dropped -- their coefficients never appeared in the plot or table.
# Interaction coefficients are now added as rows, mapped via model$assign.
# Models without interactions are unchanged.
library(survival)
library(broom)

d <- lung
d$sex <- factor(d$sex, labels = c("male", "female"))
d <- subset(d, !is.na(ph.ecog) & ph.ecog != 3)
d$ph.ecog <- factor(d$ph.ecog)

grob_labels <- function(g) {
  gt <- ggplot2::ggplotGrob(g)
  out <- character(0)
  walk <- function(x) {
    if (!is.null(x$label)) out[[length(out) + 1L]] <<- paste(x$label, collapse = "|")
    if (!is.null(x$children)) for (nm in names(x$children)) walk(x$children[[nm]])
    if (!is.null(x$grobs))    for (k in seq_along(x$grobs)) walk(x$grobs[[k]])
  }
  walk(gt)
  paste(out, collapse = " || ")
}

test_that("factor:factor interaction coefficients are drawn (#536)", {
  fit  <- coxph(Surv(time, status) ~ sex * ph.ecog, data = d)
  labs <- grob_labels(ggforest(fit, data = d))
  expect_true(grepl("sexfemale:ph.ecog1", labs, fixed = TRUE))
  expect_true(grepl("sexfemale:ph.ecog2", labs, fixed = TRUE))
})

test_that("numeric:factor and 3-way interactions render (#536)", {
  expect_true(grepl("age:sexfemale",
              grob_labels(ggforest(coxph(Surv(time, status) ~ age * sex, data = d), data = d)),
              fixed = TRUE))
  d3 <- d; d3$adh <- factor(ifelse(seq_len(nrow(d3)) %% 2 == 0, "y", "n"))
  expect_error(ggplot2::ggplotGrob(
    ggforest(coxph(Surv(time, status) ~ sex * ph.ecog * adh, data = d3), data = d3)), NA)
})

test_that("no-regression: a model without interactions is unchanged (#536)", {
  fit_main <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data = d)
  labs <- grob_labels(ggforest(fit_main, data = d))
  # no interaction-coefficient rows (an interaction label is alnum:alnum, which
  # the caption's "test): 3.4e-05" / "Index: 0.64" colons never match)
  expect_false(grepl("[[:alnum:]]:[[:alnum:]]", labs))
  expect_true(grepl("ph.ecog", labs, fixed = TRUE))
  expect_error(ggplot2::ggplotGrob(ggforest(fit_main, data = d)), NA)
})

# #594: for an interaction-only model (a term like sex:ph.ecog with no sex/ph.ecog
# main effect), ggforest() listed the main-effect factor levels as "reference"
# rows even though those main effects were never fit -- misleading. A variable
# that appears only inside an interaction (absent from names(model$assign)) is
# now skipped; its coefficients are still drawn by the interaction block. The
# number of drawn rows is read from the N-column text grob ("(N=...)" per row):
# pre-fix it exceeded the coefficient count by the spurious reference rows.
n_forest_rows <- function(g) {
  gt <- ggplot2::ggplotGrob(g)
  best <- 0L
  walk <- function(x) {
    lab <- x$label
    if (!is.null(lab) && length(lab) > 1L && all(grepl("^\\(N=", lab)))
      best <<- max(best, length(lab))
    if (!is.null(x$children)) for (nm in names(x$children)) walk(x$children[[nm]])
    if (!is.null(x$grobs))    for (k in seq_along(x$grobs)) walk(x$grobs[[k]])
  }
  walk(gt)
  best
}

test_that("interaction-only model drops spurious main-effect reference rows (#594)", {
  fit  <- coxph(Surv(time, status) ~ sex:ph.ecog, data = d)
  labs <- grob_labels(ggforest(fit, data = d))
  # the interaction coefficients are still drawn
  expect_true(grepl("sexfemale:ph.ecog1", labs, fixed = TRUE))
  # exactly one row per fitted coefficient -- no spurious sex/ph.ecog reference
  # rows (pre-#594 this drew length(coef)+5 rows for the sex & ph.ecog levels)
  expect_equal(n_forest_rows(ggforest(fit, data = d)), length(coef(fit)))
  expect_error(ggplot2::ggplotGrob(ggforest(fit, data = d)), NA)
})

test_that("mixed model keeps genuine main effects, drops interaction-only vars (#594)", {
  fit  <- coxph(Surv(time, status) ~ age + sex:ph.ecog, data = d)
  labs <- grob_labels(ggforest(fit, data = d))
  expect_true(grepl("age", labs, fixed = TRUE))                    # main effect kept
  expect_true(grepl("sexfemale:ph.ecog1", labs, fixed = TRUE))     # interaction drawn
  # age (1 numeric row = 1 coef) + interaction coefficients, one row each; no
  # spurious sex/ph.ecog reference rows -> total rows == number of coefficients
  expect_equal(n_forest_rows(ggforest(fit, data = d)), length(coef(fit)))
  expect_error(ggplot2::ggplotGrob(ggforest(fit, data = d)), NA)
})
