context("test-ggsurvtable")



test_that("survtable y axis label colors work", {
  library(dplyr)
  library("survival")
  #data("lung", package = "survival")
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  p <- ggrisktable(fit, data = lung, color = "strata")
  .build <- ggplot_build(p)
  .build_data <- .build$data[[1]]
  .table <- .build_data[, c("colour", "y")] %>%
    dplyr::distinct()
  expect_equal(.table$colour, c("#F8766D", "#00BFC4"))
  expect_equal(as.numeric(.table$y), c(2, 1))
})
