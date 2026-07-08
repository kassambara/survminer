context("ggcoxzph beta reference line and var_pval selection (#767)")

# #767: two default-preserving additions to ggcoxzph() --
#  - var_pval: select terms by their Grambsch-Therneau PH-test p-value
#  - add.beta.line: overlay a horizontal line at the fitted coefficient per panel
# Both default off/NULL, so an ordinary ggcoxzph() call is unchanged.
library(survival)

fit <- coxph(Surv(futime, fustat) ~ age + ecog.ps + rx, data = ovarian)
zph <- cox.zph(fit)

has_hline <- function(g)
  any(vapply(ggplot2::ggplot_build(g)$plot$layers,
             function(l) inherits(l$geom, "GeomHline"), logical(1)))

test_that("defaults are unchanged: all terms, no beta line (#767)", {
  p <- ggcoxzph(zph)
  expect_equal(length(p), ncol(zph$y))          # one panel per variable
  expect_false(any(vapply(p, has_hline, logical(1))))
})

test_that("var_pval selects terms below the PH-test threshold (#767)", {
  # ovarian PH-test p: age .637, ecog.ps .102, rx .378 -> only ecog.ps < 0.2
  p <- ggcoxzph(fit, var_pval = 0.2)
  expect_equal(unname(names(p)), "2")           # ecog.ps is the 2nd term
  # var and var_pval are mutually exclusive
  expect_error(ggcoxzph(fit, var = "age", var_pval = 0.5), "either")
  # a threshold nothing meets is a clear error
  expect_error(ggcoxzph(fit, var_pval = 1e-4), "No variable")
})

test_that("add.beta.line draws a line at the fitted coefficient (#767)", {
  p <- ggcoxzph(fit, add.beta.line = TRUE)
  expect_true(all(vapply(p, has_hline, logical(1))))
  # the line for term rx (panel 3) sits at coef(fit)["rx"]
  hl <- Filter(function(l) inherits(l$geom, "GeomHline"),
               ggplot2::ggplot_build(p[["3"]])$plot$layers)[[1]]
  expect_equal(hl$data$yintercept, unname(coef(fit)["rx"]))
})

test_that("beta line is drawn for single-coef terms, skipped for factor terms (#767)", {
  d <- lung; d$ph.ecog <- factor(d$ph.ecog)
  ffit <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data = d)
  # cox.zph groups the factor into one panel (no single coefficient), so:
  # age + sex get a line, ph.ecog does not; and it renders without error.
  p <- expect_warning(ggcoxzph(ffit, add.beta.line = TRUE), NA)
  expect_equal(vapply(p, has_hline, logical(1)),
               c("1" = TRUE, "2" = TRUE, "3" = FALSE))
})

test_that("add.beta.line on a cox.zph object warns and skips the line (#767)", {
  expect_warning(p <- ggcoxzph(zph, add.beta.line = TRUE), "needs a coxph model")
  expect_false(any(vapply(p, has_hline, logical(1))))
})

test_that("no-regression: default output is byte-identical for the plotted data (#767)", {
  # the built layer data of a default call must match a call that only sets the
  # new args to their defaults (i.e. the new code paths are truly inert)
  a <- lapply(ggcoxzph(zph), function(g) ggplot2::ggplot_build(g)$data)
  b <- lapply(ggcoxzph(zph, var_pval = NULL, add.beta.line = FALSE),
              function(g) ggplot2::ggplot_build(g)$data)
  expect_equal(a, b)
})
