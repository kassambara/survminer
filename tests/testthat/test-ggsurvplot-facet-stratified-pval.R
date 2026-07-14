# ggsurvplot_facet(pval.stratified = TRUE): pooled stratified log-rank p as a
# figure-level subtitle. Default (FALSE) is unchanged.

skip_if_not_installed("survival")

.fac_fit <- function() {
  d <- survival::lung
  d <- d[!is.na(d$ph.ecog) & d$ph.ecog < 3, ]
  d$ph.ecog <- factor(d$ph.ecog)
  list(fit = survival::survfit(survival::Surv(time, status) ~ sex, data = d), data = d)
}

test_that("the pooled p matches a hand survdiff(~ group + strata(facet))", {
  f <- .fac_fit()
  p <- ggsurvplot_facet(f$fit, f$data, facet.by = "ph.ecog", pval.stratified = TRUE)
  sd <- survival::survdiff(survival::Surv(time, status) ~ sex + strata(ph.ecog),
                           data = f$data)
  pv <- stats::pchisq(sd$chisq, length(sd$n) - 1, lower.tail = FALSE)
  expect_true(grepl("Stratified log-rank", p$labels$subtitle))
  expect_true(grepl(format.pval(pv, digits = 2, eps = 1e-4), p$labels$subtitle,
                    fixed = TRUE))
  expect_true(grepl("ph.ecog", p$labels$subtitle))
})

test_that("pval.stratified is independent of and combinable with per-panel pval", {
  f <- .fac_fit()
  # stratified only: subtitle present, no per-panel geom_text
  p1 <- ggsurvplot_facet(f$fit, f$data, facet.by = "ph.ecog", pval.stratified = TRUE)
  has_text1 <- any(vapply(p1$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
  expect_false(has_text1)
  # both: subtitle AND per-panel p
  p2 <- ggsurvplot_facet(f$fit, f$data, facet.by = "ph.ecog",
                         pval = TRUE, pval.stratified = TRUE)
  has_text2 <- any(vapply(p2$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
  expect_true(has_text2)
  expect_true(grepl("Stratified log-rank", p2$labels$subtitle))
})

test_that("a user subtitle is preserved (appended, not clobbered)", {
  f <- .fac_fit()
  p <- ggsurvplot_facet(f$fit, f$data, facet.by = "ph.ecog",
                        pval.stratified = TRUE, subtitle = "My subtitle")
  expect_true(grepl("My subtitle", p$labels$subtitle))
  expect_true(grepl("Stratified log-rank", p$labels$subtitle))
})

test_that("several facet variables are combined into one strata()", {
  fit <- survival::survfit(survival::Surv(time, status) ~ sex, data = survival::colon)
  p <- ggsurvplot_facet(fit, survival::colon, facet.by = c("rx", "adhere"),
                        pval.stratified = TRUE)
  sd <- survival::survdiff(survival::Surv(time, status) ~ sex + strata(rx, adhere),
                           data = survival::colon)
  pv <- stats::pchisq(sd$chisq, length(sd$n) - 1, lower.tail = FALSE)
  expect_true(grepl(format.pval(pv, digits = 2, eps = 1e-4), p$labels$subtitle,
                    fixed = TRUE))
  expect_true(grepl("rx, adhere", p$labels$subtitle))
})

test_that("a ~ 1 fit (no group) is skipped with a message, no subtitle", {
  f <- .fac_fit()
  f1 <- survival::survfit(survival::Surv(time, status) ~ 1, data = f$data)
  expect_message(
    p <- ggsurvplot_facet(f1, f$data, facet.by = "ph.ecog", pval.stratified = TRUE),
    "needs a grouping variable")
  expect_null(p$labels$subtitle)
})

test_that("a weighted method is ignored for the pooled test with a message", {
  fit <- survival::survfit(survival::Surv(time, status) ~ sex, data = survival::colon)
  expect_message(
    suppressWarnings(ggsurvplot_facet(fit, survival::colon, facet.by = "rx",
                     pval.stratified = TRUE, method = "FH_p=1_q=1")),
    "log-rank / Peto")
})
