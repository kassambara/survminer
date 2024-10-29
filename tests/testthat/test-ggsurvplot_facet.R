test_that("ggsurvplot_facet creates the correct quanitty of subplots", {
  fit <- survfit(Surv(time, status) ~ sex, data=kidney)
  p <- ggsurvplot_facet(fit, kidney, facet.by='disease')
  .build <- ggplot_build(p)
  expect_equal(nrow(.build$data[[2]]),
               length(unique(kidney[['disease']])))

  fit <- survfit(Surv(time, status) ~ disease, data=kidney)
  p <- ggsurvplot_facet(fit, kidney, facet.by='sex')
  .build <- ggplot_build(p)
  expect_equal(nrow(.build$data[[2]]),
               length(unique(kidney[['sex']])))
})

test_that("ggsurvplot_facet calculates pvalue for each facet", {
  fit <- survfit(Surv(time, status) ~ sex, data=kidney)
  p <- ggsurvplot_facet(fit, kidney, facet.by='disease', pval = TRUE)
  .build <- ggplot_build(p)
  expect_equal(nrow(.build$plot$layer[[4]][['data']]),
               length(unique(kidney[['disease']])))
})
