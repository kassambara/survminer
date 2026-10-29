context("ggsurvtable colour legend title")

# The survival tables titled the colour legend even when colour is a fixed value
# (the default color = "black"), so there was no colour scale to title. ggplot2
# >= 4.0 reports this as "Ignoring unknown labels: colour" each time a default
# ggsurvplot(risk.table = TRUE) is drawn.
library(survival)

fit <- survfit(Surv(time, status) ~ sex, data = lung)

test_that("a fixed table colour does not add an unused colour label", {
  for (survtable in c("risk.table", "cumevents", "cumcensor")) {
    p <- ggsurvtable(fit, data = lung, survtable = survtable)
    expect_null(p$labels$colour)
    expect_no_message(ggplot2::ggplot_build(p))
  }
  ggsurv <- ggsurvplot(fit, data = lung, risk.table = TRUE, legend.title = "Sex")
  expect_no_message(ggplot2::ggplot_build(ggsurv$table))
})

test_that("a table coloured by strata keeps its colour legend title", {
  p <- ggsurvtable(fit, data = lung, survtable = "risk.table",
                   color = "strata", legend.title = "Sex")
  expect_equal(p$labels$colour, "Sex")
  expect_equal(p$labels$shape, "Sex")
})
