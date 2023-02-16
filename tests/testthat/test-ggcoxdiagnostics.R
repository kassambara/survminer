test_that('ggcoxdiagnostics creates plot with all the observations', {
  cph <- coxph(Surv(futime, fustat) ~ rx + age, data=ovarian)
  p <- ggcoxdiagnostics(cph, type="deviance")
  .build <- ggplot_build(p)
  expect_equal(nrow(.build$data[[1]]), nrow(ovarian))
})

test_that('ggcoxdiagnostics with second type two rows for each observed event*term', {
  cph <- coxph(Surv(futime, fustat) ~ rx + age, data=ovarian)
  qty_terms <- length(attr(terms(cph$formula), "term.labels"))
  qty_events <- sum(ovarian$fustat==1)
  p <- ggcoxdiagnostics(cph, type="schoenfeld")
  .build <- ggplot_build(p)
  expect_equal(nrow(.build$data[[1]]), qty_terms*qty_events)
})

