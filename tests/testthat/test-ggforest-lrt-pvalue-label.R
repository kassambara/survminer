context("ggforest global p-value label")

# Regression test for #640: ggforest() labeled its global p-value as "Log-Rank",
# but the value shown (broom::glance()$p.value.log) is the likelihood-ratio test
# p-value (survival stores it in `logtest`); the score/log-rank test is
# `p.value.sc` (`sctest`). The caption now names the test correctly and the value
# it reports is unchanged.
library(survival)

# Collect all text labels drawn in the assembled forest grob (ggforest() returns
# ggpubr::as_ggplot(gtable); the forest lives in a single GeomCustomAnn layer).
caption_labels <- function(g) {
  grob <- g$layers[[1]]$geom_params$grob
  labels <- character(0)
  rec <- function(x) {
    if (!is.null(x$label)) labels <<- c(labels, unlist(x$label))
    if (!is.null(x$grobs)) lapply(x$grobs, rec)
    if (!is.null(x$children)) lapply(x$children, rec)
    invisible(NULL)
  }
  rec(grob)
  labels
}

test_that("ggforest() labels the global p-value as the likelihood ratio test (#640)", {
  cm <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data = lung)
  labs <- caption_labels(ggforest(cm, data = lung))
  caption <- labs[grepl("Global p-value", labs)]
  expect_length(caption, 1)
  expect_match(caption, "Likelihood ratio test", fixed = TRUE)
  # the old, incorrect "Log-Rank" wording must be gone
  expect_false(any(grepl("Log-Rank", labs, fixed = TRUE)))
})

test_that("ggforest() global p-value value equals the likelihood-ratio-test p-value, not the log-rank p-value (#640)", {
  cm <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data = lung)
  labs <- caption_labels(ggforest(cm, data = lung))
  caption <- labs[grepl("Global p-value", labs)]

  # the printed value matches broom's p.value.log == summary()$logtest (LRT),
  # and is distinguishable from the score/log-rank p-value here
  g <- broom::glance(cm)
  s <- summary(cm)
  expect_equal(unname(g$p.value.log), unname(s$logtest["pvalue"]))
  shown <- format.pval(g$p.value.log, eps = ".001")
  expect_match(caption, shown, fixed = TRUE)
})
