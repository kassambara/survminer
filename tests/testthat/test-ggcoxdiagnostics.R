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

test_that('ggcoxdiagnostics with ox.scale="time" shows correct event times for schoenfeld residuals', {
  cph <- coxph(Surv(futime, fustat) ~ rx + age, data=ovarian)
  p <- ggcoxdiagnostics(cph, type="schoenfeld", ox.scale="time")
  .build <- ggplot_build(p)
  
  # Extract expected time values from residuals attributes
  residuals_obj <- resid(cph, type = "schoenfeld")
  expected_times <- as.numeric(attr(residuals_obj, "dimnames")[[1]])
  
  # Check that x-axis contains the expected time values (may be repeated for each covariate)
  x_values <- unique(.build$data[[1]]$x)
  expect_equal(sort(x_values), sort(expected_times))
})

test_that('ggcoxdiagnostics with ox.scale="time" shows correct event times for scaledsch residuals', {
  cph <- coxph(Surv(futime, fustat) ~ rx + age, data=ovarian)
  p <- ggcoxdiagnostics(cph, type="scaledsch", ox.scale="time")
  .build <- ggplot_build(p)
  
  # Extract expected time values from residuals attributes
  residuals_obj <- resid(cph, type = "scaledsch")
  expected_times <- as.numeric(attr(residuals_obj, "dimnames")[[1]])
  
  # Check that x-axis contains the expected time values (may be repeated for each covariate)
  x_values <- unique(.build$data[[1]]$x)
  expect_equal(sort(x_values), sort(expected_times))
})

test_that('ggcoxdiagnostics with ox.scale="time" works with univariate model', {
  # Test with single covariate to check NCOL==1 case
  cph <- coxph(Surv(futime, fustat) ~ age, data=ovarian)
  p <- ggcoxdiagnostics(cph, type="schoenfeld", ox.scale="time")
  .build <- ggplot_build(p)
  
  # Extract expected time values from residuals attributes
  residuals_obj <- resid(cph, type = "schoenfeld")
  expected_times <- as.numeric(attr(residuals_obj, "names"))
  
  # Check that x-axis values match residuals time attributes
  x_values <- .build$data[[1]]$x
  expect_equal(x_values, expected_times)
})

