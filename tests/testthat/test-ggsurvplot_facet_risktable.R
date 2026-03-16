# Test faceted risk tables and p-values in labels

test_that("ggsurvplot_facet works with risk tables", {
  skip_on_cran()
  
  library(survival)
  
  # Basic setup
  fit <- survfit(Surv(time, status) ~ sex, data = colon)
  
  # Test 1: Basic facet with risk table
  p1 <- ggsurvplot_facet(fit, colon, facet.by = "rx",
                         palette = "jco", 
                         risk.table = TRUE)
  
  # With risk table, returns a gtable (arranged grob)
  expect_s3_class(p1, "ggsurvplot_facet")
  expect_s3_class(p1, "gtable")
})

test_that("ggsurvplot_facet works with pval in labels", {
  skip_on_cran()
  
  library(survival)
  
  # Basic setup
  fit <- survfit(Surv(time, status) ~ sex, data = colon)
  
  # Test 2: Facet with p-value in label (no risk table)
  p2 <- ggsurvplot_facet(fit, colon, facet.by = "rx",
                         palette = "jco", 
                         pval = TRUE,
                         pval.in.label = TRUE)
  
  # Without risk table, returns a ggplot object
  expect_s3_class(p2, "gg")
})

test_that("ggsurvplot_facet works with both risk table and pval in label", {
  skip_on_cran()
  
  library(survival)
  
  # Basic setup
  fit <- survfit(Surv(time, status) ~ sex, data = colon)
  
  # Test 3: Both features combined
  p3 <- ggsurvplot_facet(fit, colon, facet.by = "rx",
                         palette = "jco", 
                         pval = TRUE,
                         pval.in.label = TRUE,
                         risk.table = TRUE,
                         risk.table.height = 0.3)
  
  # With risk table, returns a gtable (arranged grob)
  expect_s3_class(p3, "ggsurvplot_facet")
  expect_s3_class(p3, "gtable")
})

test_that("ggsurvplot_facet works with two faceting variables", {
  skip_on_cran()
  
  library(survival)
  
  # Basic setup
  fit <- survfit(Surv(time, status) ~ sex, data = colon)
  
  # Test 4: Two faceting variables with risk table
  p4 <- ggsurvplot_facet(fit, colon, facet.by = c("rx", "adhere"),
                         palette = "jco", 
                         pval = TRUE,
                         risk.table = TRUE)
  
  # With risk table, returns a gtable (arranged grob)
  expect_s3_class(p4, "ggsurvplot_facet")
  expect_s3_class(p4, "gtable")
})

test_that("ggsurvplot_facet without risk table returns ggplot", {
  skip_on_cran()
  
  library(survival)
  
  # Basic setup
  fit <- survfit(Surv(time, status) ~ sex, data = colon)
  
  # Test 5: Without risk table, should return ggplot
  p5 <- ggsurvplot_facet(fit, colon, facet.by = "rx",
                         palette = "jco")
  
  expect_s3_class(p5, "gg")
  expect_s3_class(p5, "ggplot")
})
