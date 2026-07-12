context("surv_summary / ggsurvplot on a fit without confidence limits (#639)")

# A survfit built without confidence limits (conf.type = "none", or any fit that
# stored no CI) has NULL $upper/$lower. surv_summary() used to cbind those length-0
# columns and throw a cryptic "arguments imply differing number of rows: N, 0",
# which broke EVERY ggsurvplot() on such a fit -- even conf.int = FALSE -- because
# surv_summary() runs unconditionally. The absent CI columns are now coalesced to
# NA so the curve draws with no confidence band.
library(survival)

fit_noci <- survfit(Surv(time, status) ~ rx, data = colon, conf.type = "none")

test_that("surv_summary() handles a fit with no confidence limits (#639)", {
  expect_error(s <- surv_summary(fit_noci, data = colon), NA)
  expect_true(all(is.na(s$upper)))
  expect_true(all(is.na(s$lower)))
  expect_equal(nrow(s), length(fit_noci$time))
})

test_that("ggsurvplot() renders a no-CI fit with conf.int TRUE and FALSE (#639)", {
  # the reporter's exact call shape: conf.int = TRUE + legend.labs on a CI-less fit
  expect_error(
    p <- ggsurvplot(fit_noci, data = colon, conf.int = TRUE,
                    legend.labs = c("Obs", "Lev", "Lev+5FU")), NA)
  expect_error(ggplot2::ggplotGrob(p$plot), NA)           # draws, no row-mismatch
  expect_error(ggplot2::ggplotGrob(ggsurvplot(fit_noci, data = colon)$plot), NA)
})

test_that("no-regression: a fit WITH CI keeps real confidence limits (#639)", {
  s <- surv_summary(survfit(Surv(time, status) ~ rx, data = colon), data = colon)
  expect_false(any(is.na(s$upper)))
  expect_false(any(is.na(s$lower)))
})
