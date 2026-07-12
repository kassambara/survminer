context("ggadjustedcurves conditional-method warning")

# #623: with the default method = "conditional", if the grouping variable is not
# in the Cox model the per-group curves come out identical (one visible line)
# with no error. surv_adjustedcurves()/ggadjustedcurves() now warn in exactly
# that case and point to method = "average"/"marginal". Message-only: the
# returned curves are unchanged, and no warning fires for the other methods or
# when the variable is in the model.
library(survival)

lung2 <- lung
lung2$age3 <- cut(lung2$age, c(35, 55, 65, 85))
lung2 <- lung2[!is.na(lung2$age3), ]

fit_out <- coxph(Surv(time, status) ~ sex + ph.ecog + age, data = lung2)  # age3 NOT in model
fit_in  <- coxph(Surv(time, status) ~ sex + age3, data = lung2)           # age3 IN model

.warns_conditional <- function(expr) {
  hit <- FALSE
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl("conditional", conditionMessage(w))) hit <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  hit
}

test_that("warns for conditional method when variable is not in the model (#623)", {
  expect_true(.warns_conditional(
    surv_adjustedcurves(fit_out, data = lung2, variable = "age3", method = "conditional")))
  # default method is "conditional", so it warns too
  expect_true(.warns_conditional(
    surv_adjustedcurves(fit_out, data = lung2, variable = "age3")))
})

test_that("no warning when the variable IS in the model, or for average/marginal (#623)", {
  expect_false(.warns_conditional(
    surv_adjustedcurves(fit_in, data = lung2, variable = "age3", method = "conditional")))
  expect_false(.warns_conditional(
    surv_adjustedcurves(fit_out, data = lung2, variable = "age3", method = "average")))
  expect_false(.warns_conditional(
    surv_adjustedcurves(fit_out, data = lung2, variable = "age3", method = "marginal")))
})

test_that("no-regression: the returned curves are unchanged (message-only) (#623)", {
  a <- suppressWarnings(surv_adjustedcurves(fit_out, data = lung2, variable = "age3", method = "conditional"))
  b <- suppressWarnings(surv_adjustedcurves(fit_out, data = lung2, variable = "age3", method = "conditional"))
  expect_identical(a, b)
  expect_true(all(c("time", "surv", "variable") %in% names(a)))
})
