context("ggforest with extreme/near-infinite coefficients")

# Regression test for #570 / #590: a Cox model with (quasi-)complete separation
# produces near-infinite coefficients; exp() of the confidence limits overflows
# and axisTicks() errored with "'at' creation, _LARGE_ range", so ggforest()
# crashed. The axis range is now clamped to a finite window (with a warning) so
# the plot still renders.
library(survival)

test_that("ggforest() renders (with a warning) for a separated model (#570, #590)", {
  d <- lung
  # 'sep' is (near) perfectly associated with the event -> coef ~ -Inf
  d$sep <- ifelse(d$status == 2, "event", "noevent")
  m <- suppressWarnings(coxph(Surv(time, status) ~ age + sep, data = d))
  expect_warning(p <- ggforest(m, data = d), "separation|clamped")
  # and it actually draws (previously errored in axisTicks at build time)
  expect_error(ggplot2::ggplotGrob(p), NA)
})

test_that("no-regression: ordinary model does not warn or change (#570)", {
  m <- coxph(Surv(time, status) ~ age + sex, data = lung)
  w <- character(0)
  withCallingHandlers(
    ggforest(m, data = lung),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd)); invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("separation|clamped", w)))
})
