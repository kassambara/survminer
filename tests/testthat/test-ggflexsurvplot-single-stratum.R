context("ggflexsurvplot() single-stratum summary.flexsurv / fun (#400)")

# #400: for a single-stratum flexsurvreg fit, .summary_flexsurv() discarded the
# selected summary and recomputed summary(fit)[[1]] with the DEFAULT t and type.
# So a user-supplied summary.flexsurv meant to extend the curve (wider xlim) was
# ignored, and fun = "cumhaz" fell back to the survival curve. It now uses the
# already-selected summary (summary.flexsurv, or summary(fit, type = fun)).
skip_if_not_installed("flexsurv")
library(survival)

fit1 <- flexsurv::flexsurvreg(Surv(futime, fustat) ~ 1, data = ovarian, dist = "weibull")

test_that("a user summary.flexsurv with extended t reaches the curve (#400)", {
  us <- summary(fit1, t = seq(0, 3000, by = 50))
  s  <- .summary_flexsurv(fit1, type = "survival", summary.flexsurv = us)
  expect_equal(max(s$time), 3000)                 # extends to the user grid
  # default (no summary.flexsurv) still stops at observed follow-up (unchanged)
  s0 <- .summary_flexsurv(fit1, type = "survival")
  expect_equal(max(s0$time), max(ovarian$futime))
})

test_that("fun/type is honored for a single-stratum fit (#400)", {
  s_ch <- .summary_flexsurv(fit1, type = "cumhaz")
  ref  <- summary(fit1, type = "cumhaz")[[1]]
  expect_equal(s_ch$est, ref$est)                 # cumulative hazard, not survival
  # and it differs from the survival curve
  s_su <- .summary_flexsurv(fit1, type = "survival")
  expect_false(isTRUE(all.equal(s_ch$est, s_su$est)))
})

test_that("no-regression: multi-stratum summary is unchanged (#400)", {
  d <- ovarian; d$rx <- factor(d$rx)
  fit2 <- flexsurv::flexsurvreg(Surv(futime, fustat) ~ rx, data = d, dist = "weibull")
  s <- .summary_flexsurv(fit2, type = "survival")
  expect_setequal(as.character(unique(s$strata)), c("rx=1", "rx=2"))
})

test_that("ggflexsurvplot() renders for a single-stratum fit (#400)", {
  # suppressWarnings: the ggflexsurvplot render path emits an unrelated,
  # pre-existing ggplot2/ggpubr "use linewidth instead of size" deprecation.
  expect_error(
    suppressWarnings(ggplot2::ggplotGrob(ggflexsurvplot(fit1, data = ovarian)$plot)),
    NA)
})
