context("ggsurvplot add.all pval.method")

# Regression test for #673: ggsurvplot(..., add.all = TRUE, pval = TRUE,
# pval.method = TRUE) drew the p-value but rendered the pval.method (test name)
# as an empty string. The p-value is pre-computed on the original fit and
# forwarded to ggsurvplot_core() as text, so core re-derived the method from the
# "all"-augmented fit and got "". The method is now drawn by ggsurvplot_add_all().
library(survival)

plot_labels <- function(p) {
  b <- ggplot2::ggplot_build(p$plot)
  labs <- unlist(lapply(b$data, function(d) if ("label" %in% names(d)) d$label))
  labs[nzchar(trimws(labs))]
}

test_that("add.all draws the pval.method (test name), not an empty string (#673)", {
  fit <- survfit(Surv(time, status) ~ sex, data = colon)
  p <- ggsurvplot(fit, data = colon, add.all = TRUE, pval = TRUE, pval.method = TRUE)
  labs <- plot_labels(p)
  expect_true(any(grepl("[Ll]og.?rank", labs)))   # method name present
  expect_true(any(grepl("^p", labs)))             # p-value still present
})

test_that("no-regression: add.all + pval (no method) shows only the p-value (#673)", {
  fit <- survfit(Surv(time, status) ~ sex, data = colon)
  p <- ggsurvplot(fit, data = colon, add.all = TRUE, pval = TRUE)
  labs <- plot_labels(p)
  expect_true(any(grepl("^p", labs)))
  expect_false(any(grepl("[Ll]og.?rank", labs)))  # no method drawn
})

test_that("no-regression: add.all default still renders (#673)", {
  fit <- survfit(Surv(time, status) ~ sex, data = colon)
  expect_error(ggplot2::ggplotGrob(
    ggsurvplot(fit, data = colon, add.all = TRUE)$plot), NA)
})
