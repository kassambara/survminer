context("ggsurvplot_facet pval.size")

# Regression test for #338: pval.size was ignored in ggsurvplot_facet() — the
# per-facet p-value geom_text was drawn with ggplot2's default text size and the
# pval.size argument (silently routed into ...) had no effect. It is now an
# explicit argument applied to the p-value (and pval.method) text.
library(survival)

get_text_sizes <- function(p) {
  szs <- lapply(p$layers, function(l)
    if (inherits(l$geom, "GeomText")) l$aes_params$size else NULL)
  unlist(szs)
}

test_that("ggsurvplot_facet() honors pval.size (#338)", {
  fit <- survfit(Surv(time, status) ~ sex, data = colon)

  p10 <- ggsurvplot_facet(fit, colon, facet.by = "rx", pval = TRUE, pval.size = 10)
  expect_true(all(get_text_sizes(p10) == 10))

  p4 <- ggsurvplot_facet(fit, colon, facet.by = "rx", pval = TRUE, pval.size = 4)
  expect_true(all(get_text_sizes(p4) == 4))

  # pval.method text is sized too
  pm <- ggsurvplot_facet(fit, colon, facet.by = "rx", pval = TRUE,
                         pval.method = TRUE, pval.size = 7)
  expect_true(all(get_text_sizes(pm) == 7))
})

test_that("default facet p-value size is 5, consistent with ggsurvplot (#338)", {
  fit <- survfit(Surv(time, status) ~ sex, data = colon)
  p <- ggsurvplot_facet(fit, colon, facet.by = "rx", pval = TRUE)
  expect_true(all(get_text_sizes(p) == 5))
})

test_that("no-regression: a facet plot without pval has no p-value text layer (#338)", {
  fit <- survfit(Surv(time, status) ~ sex, data = colon)
  p <- ggsurvplot_facet(fit, colon, facet.by = "rx")
  has_text <- any(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
  expect_false(has_text)
})
