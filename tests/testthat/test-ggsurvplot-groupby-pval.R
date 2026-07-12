context("per-subgroup p-value for grouped ggsurvplot (#799)")

# Regression test for #799: plotting a grouped fit from surv_fit(..., group.by=)
# with ggsurvplot(..., pval = TRUE) printed the SAME pooled log-rank p-value on
# every panel, because ggsurvplot_list() forced the pooled `data =` into each
# panel's surv_pvalue(). surv_fit() now tags the grouped result with the per-group
# data subsets ("surv_fit_group_data"), and ggsurvplot_list() uses them so each
# panel's p-value is computed on its own subgroup. Genuine same-data multi-fit
# lists are unchanged.
library(survival)

panel_pvals <- function(sp) {
  vapply(sp, function(p) {
    b <- suppressWarnings(ggplot2::ggplot_build(p$plot))
    idx <- which(vapply(p$plot$layers, function(l) inherits(l$geom, "GeomText"),
                        logical(1)))
    txt <- unlist(lapply(idx, function(i) b$data[[i]]$label))
    txt[grepl("^p [=<]", txt)][1]
  }, character(1))
}

test_that("surv_fit(group.by=) tags the result with per-group data (#799)", {
  d <- lung; d$sex <- factor(d$sex)
  fits <- surv_fit(Surv(time, status) ~ sex, data = d, group.by = "ph.ecog")
  expect_false(is.null(attr(fits, "surv_fit_group_data")))
  # a plain (non-grouped) list of fits is NOT tagged
  li <- list(a = survfit(Surv(time, status) ~ sex, data = d))
  expect_null(attr(li, "surv_fit_group_data"))
})

test_that("grouped ggsurvplot shows a per-subgroup p-value on each panel (#799)", {
  d <- lung; d$sex <- factor(d$sex, labels = c("Male", "Female"))
  d <- d[d$ph.ecog %in% c(0, 1, 2), ]
  fits <- surv_fit(Surv(time, status) ~ sex, data = d, group.by = "ph.ecog")
  sp <- suppressWarnings(ggsurvplot(fits, data = d, pval = TRUE))
  shown <- panel_pvals(sp)
  # independent per-subgroup truth
  truth <- vapply(c(0, 1, 2), function(e) {
    s <- d[d$ph.ecog == e, ]
    sd <- survdiff(Surv(time, status) ~ sex, data = s)
    signif(pchisq(sd$chisq, length(sd$n) - 1, lower.tail = FALSE), 2)
  }, numeric(1))
  shown_num <- as.numeric(sub("p = ", "", shown))
  expect_equal(sort(shown_num), sort(truth))
  # they are NOT all the pooled value (the bug) -> more than one distinct p
  expect_gt(length(unique(shown_num)), 1L)
})

test_that("no-regression: same-data multi-fit list keeps its per-fit p-values (#799)", {
  d <- lung; d$sex <- factor(d$sex)
  f1 <- survfit(Surv(time, status) ~ sex, data = d)
  f2 <- survfit(Surv(time, status) ~ ph.ecog, data = d)
  sp <- suppressWarnings(ggsurvplot(list(bySex = f1, byEcog = f2), data = d,
                                    pval = TRUE))
  shown <- unname(panel_pvals(sp))
  # each panel shows its own fit's p-value text (computed on the shared data,
  # exactly as before this change), including the "p < 0.0001" formatting
  truth <- c(surv_pvalue(f1, d)$pval.txt, surv_pvalue(f2, d)$pval.txt)
  expect_equal(shown, truth)
})
