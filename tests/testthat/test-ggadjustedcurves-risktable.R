# Native Kaplan-Meier number-at-risk table under ggadjustedcurves() (#286/#354).

library(survival)

cox <- coxph(Surv(time, status) ~ sex + age + ph.ecog, data = lung)

test_that("risk.table = FALSE (default) returns the plain adjusted-curves ggplot", {
  p <- ggadjustedcurves(cox, variable = "sex", data = lung)
  expect_s3_class(p, "ggplot")
  expect_false(inherits(p, "ggsurvplot"))
  # No message now that the argument is a real feature (was a #286 pointer before).
  expect_message(ggadjustedcurves(cox, variable = "sex", data = lung), NA)
})

test_that("risk.table = TRUE returns a ggsurvplot with an aligned risk table", {
  r <- suppressWarnings(suppressMessages(
    ggadjustedcurves(cox, variable = "sex", data = lung, risk.table = TRUE)
  ))
  expect_s3_class(r, "ggsurvplot")
  expect_true(!is.null(r$plot) && !is.null(r$table))
  pdf(NULL); on.exit(dev.off())
  expect_error(print(r), NA)
})

test_that("the risk-table baseline n matches the Cox model's complete cases", {
  # The KM table is built on the rows the Cox model actually used, so its baseline
  # number at risk equals fit$n rather than over-counting rows dropped for missing
  # covariates. lung has one row with an NA covariate here (n = 227, not 228).
  r <- suppressWarnings(suppressMessages(
    ggadjustedcurves(cox, variable = "sex", data = lung, risk.table = TRUE)
  ))
  tb <- r$table$data
  n0 <- tapply(seq_len(nrow(tb)), tb$strata,
               function(i) { s <- tb[i, ]; s$n.risk[which.min(s$time)] })
  expect_equal(sum(n0), cox$n)
  expect_lt(cox$n, nrow(lung))   # confirms a row was actually dropped (real test)
})

test_that("the table labels the strata by the group value and carries a KM caveat", {
  lung2 <- transform(lung, sex = factor(sex, labels = c("Male", "Female")))
  cox2  <- coxph(Surv(time, status) ~ sex + age + ph.ecog, data = lung2)
  r <- suppressWarnings(suppressMessages(
    ggadjustedcurves(cox2, variable = "sex", data = lung2, risk.table = TRUE)
  ))
  # Strata labels are the clean group values (no "sex=" prefix).
  expect_setequal(as.character(unique(r$table$data$strata)), c("Male", "Female"))
  # The table title makes the Kaplan-Meier (unadjusted) nature explicit.
  expect_match(r$table$labels$title, "Kaplan-Meier", fixed = TRUE)
})

test_that("curve and risk table share the same x range (aligned)", {
  r <- suppressWarnings(suppressMessages(
    ggadjustedcurves(cox, variable = "sex", data = lung, risk.table = TRUE)
  ))
  xr_plot  <- ggplot2::ggplot_build(r$plot)$layout$panel_params[[1]]$x.range
  xr_table <- ggplot2::ggplot_build(r$table)$layout$panel_params[[1]]$x.range
  expect_equal(xr_plot, xr_table, tolerance = 1e-6)
})

test_that("method = 'single' gives an overall (~1) KM table", {
  r <- suppressWarnings(suppressMessages(
    ggadjustedcurves(cox, data = lung, method = "single", risk.table = TRUE)
  ))
  expect_s3_class(r, "ggsurvplot")
  expect_match(r$table$labels$title, "Kaplan-Meier", fixed = TRUE)
  # One (overall) stratum.
  expect_length(unique(as.character(r$table$data$strata)), 1L)
})

test_that("a numeric grouping variable works", {
  coxn <- coxph(Surv(time, status) ~ sex + age, data = lung)  # sex is numeric 1/2
  r <- suppressWarnings(suppressMessages(
    ggadjustedcurves(coxn, variable = "sex", data = lung, risk.table = TRUE)
  ))
  expect_s3_class(r, "ggsurvplot")
  expect_length(unique(as.character(r$table$data$strata)), 2L)
})

test_that("user break.time.by / xlim flow to both panels", {
  r <- suppressWarnings(suppressMessages(
    ggadjustedcurves(cox, variable = "sex", data = lung, risk.table = TRUE,
                     xlim = c(0, 800), break.time.by = 200)
  ))
  xr_plot  <- ggplot2::ggplot_build(r$plot)$layout$panel_params[[1]]$x.range
  xr_table <- ggplot2::ggplot_build(r$table)$layout$panel_params[[1]]$x.range
  expect_equal(xr_plot, xr_table, tolerance = 1e-6)
  expect_lte(max(xr_plot), 800 * 1.06)  # respects the requested xlim (+ expansion)
})
