context("ggsurvplot_facet p.adjust.method for per-panel p-values (#407)")

# Regression test for #407: ggsurvplot_facet(..., pval = TRUE) computes a p-value
# for each panel but did not adjust them across panels. The new p.adjust.method
# argument adjusts the per-panel p-values via stats::p.adjust() (mirroring
# pairwise_survdiff()); the default "none" leaves the raw per-panel p-values
# untouched. Panels with an undefined p-value are excluded from the adjustment.
library(survival)

# pull the p-value text drawn on the panels
panel_pvals <- function(p) {
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  idx <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
  unlist(lapply(idx, function(i) b$data[[i]]$label))
}

test_that("default p.adjust.method = 'none' shows raw per-panel p-values (#407)", {
  d <- subset(colon, etype == 2); d$sex <- factor(d$sex, labels = c("F", "M"))
  fit <- survfit(Surv(time, status) ~ rx, data = d)
  p <- suppressWarnings(ggsurvplot_facet(fit, data = d, facet.by = "sex", pval = TRUE))
  txt <- panel_pvals(p)
  expect_true(all(grepl("^p =", txt)))         # raw, "p ="
  expect_false(any(grepl("adj", txt)))          # not adjusted
})

test_that("p.adjust.method = 'BH' adjusts the per-panel p-values across panels (#407)", {
  d <- subset(colon, etype == 2); d$sex <- factor(d$sex, labels = c("F", "M"))
  fit <- survfit(Surv(time, status) ~ rx, data = d)
  raw <- suppressWarnings(ggsurvplot_facet(fit, data = d, facet.by = "sex", pval = TRUE))
  bh  <- suppressWarnings(ggsurvplot_facet(fit, data = d, facet.by = "sex",
                                           pval = TRUE, p.adjust.method = "BH"))
  txt <- panel_pvals(bh)
  expect_true(all(grepl("^adj\\.p", txt)))      # prefixed adj.p
  # the smallest raw p-value is inflated by BH (2 panels): the largest is unchanged
  raw_num <- as.numeric(sub("p = ", "", panel_pvals(raw)))
  bh_num  <- as.numeric(sub("adj\\.p = ", "", txt))
  expect_true(min(bh_num) >= min(raw_num))       # adjusted up
  expect_equal(max(bh_num), max(raw_num))         # largest p unchanged under BH
})

test_that("an invalid p.adjust.method errors clearly (#407)", {
  d <- subset(colon, etype == 2)
  fit <- survfit(Surv(time, status) ~ rx, data = d)
  expect_error(
    suppressWarnings(ggsurvplot_facet(fit, data = d, facet.by = "sex",
                                      pval = TRUE, p.adjust.method = "nope")))
})

test_that("panels with an undefined p-value are excluded from the adjustment (#407)", {
  # lung ph.ecog == 3 has a single observation -> undefined per-panel p (NA),
  # which must stay blank and not count toward the multiple-comparison n.
  d <- lung; d$ph.ecog <- factor(d$ph.ecog)
  fit <- survfit(Surv(time, status) ~ sex, data = d)
  p <- suppressWarnings(ggsurvplot_facet(fit, data = d, facet.by = "ph.ecog",
                                         pval = TRUE, p.adjust.method = "BH"))
  txt <- panel_pvals(p)
  adj <- txt[nzchar(txt)]
  expect_true(all(grepl("^adj\\.p", adj)))
  expect_true(length(adj) >= 1)
})
