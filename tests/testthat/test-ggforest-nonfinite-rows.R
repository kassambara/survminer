context("ggforest omits non-finite (non-converged) rows from the drawn layers")

# Regression test for #406: a Cox model that did not converge (complete /
# quasi-complete separation) has a coefficient whose exp()-ed hazard ratio or
# confidence limit overflows to Inf / underflows to 0. Such a row cannot be
# placed on the log axis; drawing it produced a misleading full-width interval
# and a "log-10 transformation introduced infinite values" warning. ggforest()
# now (a) emits a clear message naming the term(s), (b) omits those rows from the
# point/error-bar layers (so no Inf reaches the log scale -> no log-10 warning),
# while (c) keeping them in the table. Converged models are unchanged.
#
# ggforest() returns ggpubr::as_ggplot(gtable), i.e. one wrapper grob, so the
# inner layers are inspected via the grob tree, and the row-dropping is asserted
# behaviourally through the absence of the log-10 warning.
library(survival)

sep_model <- function() {
  d <- lung
  d$sep <- ifelse(d$status == 2, "A", "B")   # perfectly predicts the event
  suppressWarnings(coxph(Surv(time, status) ~ age + sep, data = d))
}

collect_warnings <- function(expr) {
  w <- character(0)
  withCallingHandlers(expr,
    warning = function(cnd) { w <<- c(w, conditionMessage(cnd)); invokeRestart("muffleWarning") })
  w
}

# All text labels drawn anywhere in the assembled forest grob.
grob_labels <- function(g) {
  grob <- g$layers[[1]]$geom_params$grob
  labels <- character(0)
  rec <- function(x) {
    if (!is.null(x$label)) labels <<- c(labels, unlist(x$label))
    if (!is.null(x$grobs)) lapply(x$grobs, rec)
    if (!is.null(x$children)) lapply(x$children, rec)
    invisible(NULL)
  }
  rec(grob)
  labels
}

test_that("a non-finite term triggers a clear message and no log-10 warning (#406)", {
  d <- lung; d$sep <- ifelse(d$status == 2, "A", "B")
  m <- sep_model()
  w <- collect_warnings(print(ggforest(m, data = d)))
  expect_true(any(grepl("did not converge", w)))    # clear, specific message
  expect_true(any(grepl("sep", w, fixed = TRUE)))    # names the offending term
  expect_false(any(grepl("infinite values", w)))     # no residual log-10 warning
  # (the log-10 warning would fire iff an Inf/0 coordinate still reached the
  # log scale, i.e. iff the non-finite row were still drawn -> its absence is the
  # behavioural proof the row was dropped from the point/error-bar layers.)
})

test_that("the non-finite row is still shown in the table (#406)", {
  d <- lung; d$sep <- ifelse(d$status == 2, "A", "B")
  m <- sep_model()
  labs <- grob_labels(suppressWarnings(ggforest(m, data = d)))
  expect_true(any(grepl("Inf", labs)))               # the "(0.00 - Inf)" CI text
  expect_true(any(grepl("sep", labs, fixed = TRUE))) # the term name
})

test_that("a single separated factor covariate still renders (no axisTicks crash) (#406)", {
  # Regression guard: when the only drawable row is the NA-CI reference level
  # (single factor covariate, its other level non-finite), the axis range must
  # fall back so axisTicks() does not receive an empty/inverted range.
  d <- lung; d$sep <- ifelse(d$status == 2, "A", "B")
  m <- suppressWarnings(coxph(Surv(time, status) ~ sep, data = d))
  expect_error(suppressWarnings(ggplot2::ggplotGrob(ggforest(m, data = d))), NA)
})

test_that("no-regression: a converged model renders without the new message (#406)", {
  m <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data = lung)
  w <- collect_warnings(print(ggforest(m, data = lung)))
  expect_false(any(grepl("did not converge|infinite values", w)))
  expect_error(ggplot2::ggplotGrob(ggforest(m, data = lung)), NA)
})
