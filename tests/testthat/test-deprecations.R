# survminer's own code should not trigger ggplot2/lifecycle deprecation warnings.
# Regression guard for the `size` line aesthetic (deprecated in ggplot2 3.4.0):
# passing `size` to ggsurvplot()/ggflexsurvplot() must set `linewidth`, not the
# deprecated `size` aesthetic on the line geom.

library(survival)

# collect deprecation warnings emitted while evaluating `expr`
.deprecations <- function(expr) {
  old <- options(lifecycle_verbosity = "warning")
  on.exit(options(old), add = TRUE)
  msgs <- character()
  withCallingHandlers(
    force(expr),
    warning = function(w) {
      m <- conditionMessage(w)
      if (grepl("deprecat", m, ignore.case = TRUE)) msgs <<- c(msgs, m)
      invokeRestart("muffleWarning")
    }
  )
  msgs
}

test_that("ggsurvplot(size=) sets linewidth, not the deprecated size aesthetic", {
  km <- survfit(Surv(time, status) ~ sex, data = lung)
  for (s in c(0.5, 1, 2)) {
    p <- ggsurvplot(km, data = lung, size = s)
    lyr <- p$plot$layers[[1]]
    expect_equal(lyr$aes_params$linewidth, s)
    expect_false("size" %in% names(lyr$aes_params))
  }
  # and building the plot emits no deprecation warning from our code
  msgs <- .deprecations(ggplot2::ggplot_build(
    ggsurvplot(km, data = lung, size = 0.8)$plot))
  expect_false(any(grepl("size", msgs) & grepl("deprecat", msgs, ignore.case = TRUE)))
})

test_that("ggflexsurvplot() draws lines without the deprecated size aesthetic", {
  skip_if_not_installed("flexsurv")
  fit <- flexsurv::flexsurvreg(Surv(time, status) ~ sex, data = lung, dist = "weibull")
  msgs <- .deprecations(
    ggplot2::ggplot_build(ggflexsurvplot(fit, data = lung, conf.int.flex = TRUE)$plot))
  expect_false(any(grepl("deprecat", msgs, ignore.case = TRUE)))
})
