context("ggsurvplot linejoin argument")

# Feature test for #653: ggsurvplot() gains a `linejoin` argument passed to the
# survival geom. Default "round" reproduces geom_step()'s own default (curve
# unchanged); "mitre" gives sharp corners at event times.
library(survival)

fit <- survfit(Surv(time, status) ~ sex, data = lung)

.surv_linejoin <- function(p) p$plot$layers[[1]]$geom_params$linejoin

test_that("default linejoin is 'round' and unchanged (#653)", {
  p <- ggsurvplot(fit, data = lung)
  expect_identical(.surv_linejoin(p), "round")
  expect_error(ggplot2::ggplotGrob(p$plot), NA)
})

test_that("linejoin = 'mitre' is applied to the survival curve (#653)", {
  p <- ggsurvplot(fit, data = lung, linejoin = "mitre")
  expect_identical(.surv_linejoin(p), "mitre")
  expect_error(ggplot2::ggplotGrob(p$plot), NA)
})

test_that("linejoin also works with a smooth (geom_line) survival curve (#653)", {
  p <- ggsurvplot(fit, data = lung, surv.geom = ggplot2::geom_line, linejoin = "mitre")
  expect_identical(.surv_linejoin(p), "mitre")
  expect_error(ggplot2::ggplotGrob(p$plot), NA)
})
