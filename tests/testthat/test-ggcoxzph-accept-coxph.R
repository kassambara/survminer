context("ggcoxzph accepts a coxph object")

# Regression test for #410: ggcoxzph() now accepts a coxph model directly and
# runs survival::cox.zph() on it, instead of erroring "Can't handle an object of
# class coxph". Passing a cox.zph object (the previous input) is unchanged.
library(survival)

fit <- coxph(Surv(futime, fustat) ~ age + ecog.ps + rx, data = ovarian)
zph <- cox.zph(fit)

drew_ok <- function(p) {
  tmp <- tempfile(fileext = ".pdf"); grDevices::pdf(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)
  tryCatch({ print(p); TRUE }, error = function(e) FALSE)
}

test_that("ggcoxzph() accepts a coxph model (#410)", {
  expect_error(p <- ggcoxzph(fit), NA)
  expect_s3_class(p, "ggcoxzph")
  expect_true(drew_ok(p))
})

test_that("a coxph model gives the same panels as its cox.zph (#410)", {
  p_coxph <- ggcoxzph(fit)
  p_zph   <- ggcoxzph(zph)
  expect_identical(names(p_coxph), names(p_zph))
  expect_identical(length(p_coxph), length(p_zph))
})

test_that("no-regression: a cox.zph object still works and is unchanged (#410)", {
  # the coxph branch must not fire for a cox.zph input (it is not a coxph)
  expect_false(methods::is(zph, "coxph"))
  p <- ggcoxzph(zph)
  expect_s3_class(p, "ggcoxzph")
  expect_true(drew_ok(p))
})

test_that("a non-coxph, non-cox.zph object still errors clearly (#410)", {
  expect_error(ggcoxzph(lm(1 ~ 1)), "Can't handle an object of class")
})
