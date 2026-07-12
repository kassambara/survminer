context("formula stored in a variable")

# Regression test for #324 / #341 / #602: when a survfit is built from a formula
# held in a variable (e.g. `frm <- Surv(time,status)~sex; survfit(frm, data)`),
# fit$call$formula is an unevaluated symbol, and as.formula() could not subset it
# ("object of type 'symbol' is not subsettable") -- breaking ggsurvplot() and
# surv_pvalue(). .get_fit_formula()/.extract.survfit() now resolve it via
# stats::formula(fit). The formula variable is assigned in the global environment
# to reproduce the reported interactive scenario (where it is reachable, as in a
# user session); a variable local to an already-exited function is out of scope
# for either survival or survminer and is a separate, documented limitation.
test_that("ggsurvplot()/surv_pvalue() handle a formula stored in a variable (#324, #341, #602)", {
  library(survival)
  assign("frm_324", as.formula("Surv(time, status) ~ sex"), envir = globalenv())
  on.exit(rm("frm_324", envir = globalenv()), add = TRUE)

  fit_var    <- survfit(frm_324, data = lung)                  # $call$formula is a symbol
  fit_inline <- survfit(Surv(time, status) ~ sex, data = lung) # $call$formula is a call
  expect_true(is.name(fit_var$call$formula))

  # ggsurvplot() builds without the 'symbol not subsettable' error
  expect_error(ggsurvplot(fit_var, data = lung), NA)

  # surv_pvalue() works and equals the inline-formula result (no-regression)
  expect_equal(
    surv_pvalue(fit_var, data = lung)$pval,
    surv_pvalue(fit_inline, data = lung)$pval
  )

  # the helper resolves the symbol to the same formula as the inline fit
  expect_equal(
    deparse(.get_fit_formula(fit_var)),
    deparse(.get_fit_formula(fit_inline))
  )
})
