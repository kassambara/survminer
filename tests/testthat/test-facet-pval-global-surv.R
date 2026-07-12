context("ggsurvplot_facet pval with a Surv object built outside the data")

# Regression test for #467: when the model is built from a Surv object created
# OUTSIDE the data (a bare-symbol formula LHS, e.g. `Survival <- Surv(...)`,
# survfit(Survival ~ x, data = D)) rather than `Surv(time, status) ~ x`, the
# per-panel p-value refit row-subset the data and re-evaluated the full-length
# global response against a shorter subset -> "variable lengths differ". The
# response is now materialised once on the full data and the p-value formula
# points at it. Ordinary in-formula fits are unchanged.
#
# The reported scenario builds the Surv object in the global environment, so the
# tests do the same (with cleanup) to mirror it faithfully; the fix resolves the
# response from that accessible scope.
library(survival)

drew_ok <- function(p) {
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)
  tryCatch({ suppressWarnings(print(p)); TRUE }, error = function(e) FALSE)
}

panel_pvals <- function(p) {
  b <- ggplot2::ggplot_build(p)
  labs <- unlist(lapply(b$data, function(L) if ("label" %in% names(L)) as.character(L$label)))
  labs[grepl("^p", labs)]
}

with_global_Survival <- function(D, code) {
  assign("Survival", Surv(time = D$time, event = D$status), envir = globalenv())
  on.exit(if (exists("Survival", envir = globalenv(), inherits = FALSE))
    rm("Survival", envir = globalenv()), add = TRUE)
  force(code)
}

test_that("facet.by + pval works with a global Surv LHS, facet var IN the formula (#467)", {
  D <- colon[!is.na(colon$time), ]
  with_global_Survival(D, {
    fit <- survfit(data = D, formula = Survival ~ rx + sex)
    p <- suppressWarnings(ggsurvplot(fit, data = D, pval = TRUE, facet.by = "sex"))
    expect_true(drew_ok(p))
    pv <- panel_pvals(p)
    expect_length(pv, length(unique(D$sex)))
    expect_true(any(grepl("0.18", pv, fixed = TRUE)))       # sex==0 (matches survdiff)
    expect_true(any(grepl("< 0.0001", pv, fixed = TRUE)))   # sex==1 (matches survdiff)
  })
})

test_that("facet.by + pval works with a global Surv LHS, facet var NOT in the formula (#467)", {
  D <- colon[!is.na(colon$time), ]
  with_global_Survival(D, {
    fit <- survfit(data = D, formula = Survival ~ rx + sex)
    p <- suppressWarnings(ggsurvplot(fit, data = D, pval = TRUE, facet.by = "adhere"))
    expect_true(drew_ok(p))
  })
})

test_that("no-regression: in-formula Surv(...) facet + pval is unchanged (#467)", {
  # gate must NOT fire (LHS is a call, not a symbol): output identical to before.
  D <- colon[!is.na(colon$time), ]
  fit <- survfit(Surv(time, status) ~ rx + sex, data = D)
  p <- suppressWarnings(ggsurvplot(fit, data = D, pval = TRUE, facet.by = "sex"))
  expect_true(drew_ok(p))
  pv <- panel_pvals(p)
  expect_true(any(grepl("0.18", pv, fixed = TRUE)))
  expect_true(any(grepl("< 0.0001", pv, fixed = TRUE)))
})
