context("ggsurvplot_facet keeps case weights on the refit")

# Regression test for #556: when facet.by variables are not all in the survfit
# formula, ggsurvplot_facet() refits the curves; it used to drop the fit's
# `weights`, so faceted curves and the surv.median.line were computed UNWEIGHTED
# (silently wrong). The refit now forwards the recovered weight vector, so the
# medians match a direct per-subset weighted survfit. Unweighted fits are
# byte-identical (verified separately via ggplot_build). Per-panel p-values stay
# unweighted (survival::survdiff has no case-weights argument) and now warn.
#
# Note: ggsurvplot(..., facet.by=) returns a plain ggplot, so we introspect it
# with ggplot2::ggplot_build() (not $plot / $data.survplot).
library(survival)
library(ggplot2)

# The horizontal median segments of surv.median.line = "hv" end at each curve's
# median; collect the distinct segment x/xend positions (drop the x=0 origins).
drawn_medians <- function(p) {
  b <- ggplot_build(p)
  xs <- unlist(lapply(b$data, function(L)
    if (all(c("x", "xend", "y", "yend") %in% names(L))) c(L$x, L$xend)))
  sort(unique(round(xs[xs > 0])))
}

# weighted / unweighted medians for every facet x strata cell
cell_medians <- function(data, facet, weights = NULL) {
  vals <- integer(0)
  for (g in unique(data[[facet]])) {
    sub <- data[data[[facet]] == g, ]
    tab <- if (is.null(weights))
      summary(survfit(Surv(time, status) ~ sex, data = sub))$table
    else
      summary(survfit(Surv(time, status) ~ sex, data = sub, weights = sub[[weights]]))$table
    vals <- c(vals, tab[, "median"])
  }
  sort(unique(round(vals[is.finite(vals)])))
}

set.seed(1)
d <- lung
d$w   <- runif(nrow(d), 0.2, 3)
d$grp <- factor(sample(c("A", "B"), nrow(d), replace = TRUE))

test_that("weighted facet refit draws the WEIGHTED medians, not unweighted (#556)", {
  # weights given as a bare column of `data` (the reporter's form), which
  # resolves against `data` regardless of calling scope.
  fit <- survfit(Surv(time, status) ~ sex, data = d, weights = w)
  p <- suppressWarnings(ggsurvplot(fit, data = d, facet.by = "grp",
                                   surv.median.line = "hv"))
  drawn <- drawn_medians(p)
  mw <- cell_medians(d, "grp", weights = "w")
  mu <- cell_medians(d, "grp", weights = NULL)
  expect_true(mw[1] != mu[1] || length(mw) != length(mu))  # weighted != unweighted (test is meaningful)
  expect_setequal(drawn, mw)                               # drawn == weighted set
  expect_false(identical(drawn, mu))                       # drawn != unweighted set
})

test_that("no-regression: unweighted facet draws the ordinary (unweighted) medians (#556)", {
  # .weights is NULL for an unweighted fit -> refit takes the unchanged path.
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  dd <- lung[!is.na(lung$ph.ecog) & lung$ph.ecog != 3, ]   # drop tiny/NA cells
  dd$ph.ecog <- factor(dd$ph.ecog)
  p <- suppressWarnings(ggsurvplot(survfit(Surv(time, status) ~ sex, data = dd),
                                   data = dd, facet.by = "ph.ecog",
                                   surv.median.line = "hv"))
  drawn <- drawn_medians(p)
  mu <- cell_medians(dd, "ph.ecog", weights = NULL)
  expect_true(all(mu %in% drawn))                          # unchanged path = unweighted medians
})

test_that("per-panel p-values under weights warn (survdiff has no case weights) (#556)", {
  fit <- survfit(Surv(time, status) ~ sex, data = d, weights = w)
  ws <- character(0)
  withCallingHandlers(
    suppressMessages(ggsurvplot(fit, data = d, facet.by = "grp", pval = TRUE)),
    warning = function(w) { ws <<- c(ws, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  expect_true(any(grepl("p-values are computed unweighted", ws)))
})

test_that("no-regression: unweighted facet + pval does NOT warn about weights (#556)", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  ws <- character(0)
  withCallingHandlers(
    suppressMessages(ggsurvplot(fit, data = lung, facet.by = "ph.ecog", pval = TRUE)),
    warning = function(w) { ws <<- c(ws, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  expect_false(any(grepl("p-values are computed unweighted", ws)))
})

test_that("unresolvable weights fall back (with a warning), no error (#556)", {
  # weights given as an out-of-scope object -> can't be re-resolved -> the refit
  # falls back to unweighted AND warns (explicit, not silent), still drawing.
  local_w <- runif(nrow(d), 0.2, 3)
  fit <- survfit(Surv(time, status) ~ sex, data = d, weights = local_w)
  rm(local_w)
  ws <- character(0)
  p <- withCallingHandlers(
    ggsurvplot(fit, data = d, facet.by = "grp"),
    warning = function(w) { ws <<- c(ws, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  expect_true(any(grepl("could not recover the fit's `weights`", ws)))
  expect_error(ggplot2::ggplot_build(p), NA)
})
