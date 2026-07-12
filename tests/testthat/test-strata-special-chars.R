context("strata parsing with special characters in factor levels")

# Regression tests for the strata-parsing cluster: factor LEVELS containing "=",
# ">", "$", etc. used to break survminer's strata parsing, which split each
# stratum name on every "=" / "," (and mis-detected "$"). The parser now scopes
# the split to the KNOWN formula variable names, so levels may contain those
# characters. Covers #291, #430, #616, #599, #680. A no-regression block at the
# end locks the ordinary (no special char) behavior.
library(survival)

drew_ok <- function(p) {
  # Force draw-time evaluation (compound survminer objects only fail when drawn).
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)
  res <- tryCatch({ grid::grid.draw(p); TRUE }, error = function(e) FALSE)
  res
}

test_that("'=' in a factor level: faceting works (#291)", {
  test <- colon
  levels(test$rx)[3] <- "Lev = 54FU"
  fit <- survfit(Surv(time, status) ~ rx, data = test)
  p <- suppressWarnings(ggsurvplot(fit, test, facet.by = "sex", pval = TRUE))
  expect_true(drew_ok(p))
})

test_that("'>=' in a factor level: ggsurvplot_facet works (#430)", {
  cc <- colon
  cc$sex_fact <- factor(ifelse(cc$sex == 1, ">=m", "<f"))
  fit <- survfit(Surv(time, status) ~ sex_fact, data = cc)
  p <- suppressWarnings(ggsurvplot_facet(fit, cc, facet.by = "rx", pval = TRUE))
  expect_true(drew_ok(p))
})

test_that("'>='/'<' level used as facet.by works (#616)", {
  cc <- colon
  cc$agecat <- ifelse(cc$age < median(cc$age), "< 61", ">=61")
  fit <- survfit(Surv(time, status) ~ sex + agecat, data = cc)
  p <- suppressWarnings(ggsurvplot_facet(fit, cc, facet.by = "agecat"))
  expect_true(drew_ok(p))
})

test_that("'=' in a factor level: median lines are still drawn (#599)", {
  lg <- lung
  lg$sex_eq <- factor(lg$sex, levels = c(1, 2), labels = c("value = 1", "value = 2"))
  fit <- survfit(Surv(time, status) ~ sex_eq, data = lg)
  b <- ggplot2::ggplot_build(
    suppressWarnings(ggsurvplot(fit, data = lg, surv.median.line = "hv"))$plot)
  n_seg <- sum(vapply(b$data,
                      function(L) if (all(c("x", "xend") %in% names(L))) nrow(L) else 0L,
                      integer(1)))
  expect_gt(n_seg, 0)  # 2 groups + shared horizontal -> 3 segments; 0 was the bug
})

test_that("'$' in a factor level: variables are parsed correctly, no reordering (#680)", {
  fd <- data.frame(
    marker = factor(rep(c("Marker $< 50%$", "Marker $>= 50%$"), each = 8),
                    levels = c("Marker $< 50%$", "Marker $>= 50%$")),
    arm    = factor(rep(c("Experimental", "Standard"), times = 8),
                    levels = c("Standard", "Experimental")),
    time   = c(1, 2, 2, 4, 3, 6, 4, 8, 10, 5, 12, 6, 14, 7, 16, 8),
    event  = rep(1, 16))
  fit <- survfit(Surv(time, event) ~ marker + arm, data = fd)
  ss <- surv_summary(fit, data = fd)

  # both variable columns are recovered without NA and with the data's own levels
  expect_false(any(is.na(ss$marker)))
  expect_false(any(is.na(ss$arm)))
  expect_identical(levels(ss$marker), levels(fd$marker))
  expect_identical(levels(ss$arm), levels(fd$arm))

  # the "$" inside a level must NOT be stripped (that was the mangling): the
  # strata column keeps the full "marker=..., arm=..." names
  expect_true(all(grepl("^marker=Marker \\$", as.character(ss$strata))))
  expect_true(any(grepl("arm=Standard", as.character(ss$strata))))
})

test_that("'$' in a factor level: median lines and risk-table columns are correct (#680)", {
  lg <- lung
  lg$marker <- factor(ifelse(lg$age < median(lg$age), "M $lo$", "M $hi$"),
                      levels = c("M $lo$", "M $hi$"))
  fit <- survfit(Surv(time, status) ~ marker, data = lg)

  # median lines must still be drawn (the "$" must not mangle the median-table
  # strata via the no-fit .clean_strata path)
  b <- ggplot2::ggplot_build(
    suppressWarnings(ggsurvplot(fit, data = lg, surv.median.line = "hv"))$plot)
  n_seg <- sum(vapply(b$data,
                      function(L) if (all(c("x", "xend") %in% names(L))) nrow(L) else 0L,
                      integer(1)))
  expect_gt(n_seg, 0)

  # the risk-table summary must recover the variable column without NA
  p <- suppressWarnings(ggsurvplot(fit, data = lg, risk.table = TRUE))
  expect_true("marker" %in% colnames(p$data.survtable))
  expect_false(any(is.na(p$data.survtable$marker)))
})

# ---------------------------------------------------------------------------
# No-regression: ordinary data (no special characters) is unchanged.
# ---------------------------------------------------------------------------

test_that("no-regression: ordinary single- and multi-variable strata parse as before", {
  f1 <- survfit(Surv(time, status) ~ sex, data = lung)
  s1 <- surv_summary(f1, data = lung)
  expect_equal(as.character(unique(s1$sex)), c("1", "2"))

  f2 <- survfit(Surv(time, status) ~ sex + ph.ecog, data = lung)
  s2 <- surv_summary(f2, data = lung)
  expect_false(any(is.na(s2$sex)))
  expect_false(any(is.na(s2$ph.ecog)))
  expect_setequal(as.character(unique(s2$ph.ecog)),
                  as.character(stats::na.omit(unique(lung$ph.ecog))))
})

test_that("no-regression: survival right-padded level names are trimmed (kidney facet)", {
  # kidney$disease levels have unequal widths -> survival right-pads them in the
  # strata labels; the parser must trim so all 4 levels are recovered (this is
  # what the padding fix protects; cf. test-ggsurvplot_facet.R).
  fit <- survfit(Surv(time, status) ~ sex + disease, data = kidney)
  ss <- surv_summary(fit, data = kidney)
  expect_setequal(as.character(unique(ss$disease)), levels(kidney$disease))
  expect_false(any(is.na(ss$disease)))
})

test_that("no-regression: the data\\$variable fit form still adds the variable column", {
  fit <- survfit(Surv(lung$time, lung$status) ~ lung$sex)
  ss <- surv_summary(fit, data = lung)
  # dollar-prefixed strata are cleaned and the underlying column is recovered
  expect_true("sex" %in% colnames(ss))
  expect_false(any(is.na(ss$sex)))
})

test_that("a factor level with a trailing/leading space is recovered, not NA (#616)", {
  # survival right-pads level names in strata labels, so the parser must trim;
  # matching against the trimmed factor levels (while keeping the original level
  # as the value) lets a level with genuine surrounding whitespace still match.
  for (labs in list(c("Obs, ", "Lev", "Lev+5FU"), c(" Obs", "Lev", "Lev+5FU"))) {
    cc <- colon
    cc$rx <- factor(cc$rx, levels = levels(cc$rx), labels = labs)
    ss <- surv_summary(survfit(Surv(time, status) ~ rx, data = cc), data = cc)
    expect_false(any(is.na(ss$rx)))
    expect_identical(levels(ss$rx), labs)          # original (untrimmed) levels kept
    expect_setequal(as.character(unique(ss$rx)), labs)
  }
})
