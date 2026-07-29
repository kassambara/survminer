# Parametric-over-KM overlay, ggsurvparametric().

library(survival)

test_that("a survreg fit returns a ggsurvplot with a dashed parametric overlay", {
  fit <- survreg(Surv(time, status) ~ sex, data = lung, dist = "weibull")
  p <- ggsurvparametric(fit, data = lung)
  expect_s3_class(p, "ggsurvplot")
  geoms <- vapply(p$plot$layers, function(l) class(l$geom)[1], character(1))
  # a solid KM step + the dashed parametric line
  expect_true("GeomStep" %in% geoms)
  expect_true("GeomLine" %in% geoms)
  # the overlay data is attached and has one curve per group
  par <- attr(p$plot, "parametric")
  expect_setequal(levels(par$strata), levels(p$plot$data$strata))
})

test_that("the survreg curve equals 1 - psurvreg(t, lp, scale, dist)", {
  fit <- survreg(Surv(time, status) ~ sex, data = lung, dist = "weibull")
  p <- ggsurvparametric(fit, data = lung)
  par <- attr(p$plot, "parametric")
  # reference for the first group (sex = 1) at its linear predictor
  lp <- predict(fit, newdata = data.frame(sex = 1), type = "lp")
  g1 <- par[par$.row == 1, ]
  ref <- 1 - psurvreg(g1$time, mean = lp, scale = fit$scale,
                      distribution = fit$dist)
  expect_equal(g1$surv, ref, tolerance = 1e-8)
})

test_that("the parametric overlay is not extrapolated beyond the follow-up", {
  fit <- survreg(Surv(time, status) ~ sex, data = lung, dist = "weibull")
  p <- ggsurvparametric(fit, data = lung)
  par <- attr(p$plot, "parametric")
  km <- survfit(Surv(time, status) ~ sex, data = lung)
  expect_lte(max(par$time), max(km$time))
})

test_that("overlay colours align with the KM even after legend.labs / palette", {
  fit <- survreg(Surv(time, status) ~ sex, data = lung, dist = "weibull")
  p <- ggsurvparametric(fit, data = lung, legend.labs = c("Male", "Female"),
                    palette = c("red", "blue"))
  par <- attr(p$plot, "parametric")
  # the overlay uses the relabelled strata, positionally matched (no NA levels)
  expect_setequal(levels(par$strata), c("Male", "Female"))
  expect_false(any(is.na(par$strata)))
})

test_that("conf.int adds a band within [0, 1]", {
  fit <- survreg(Surv(time, status) ~ sex, data = lung, dist = "weibull")
  p <- ggsurvparametric(fit, data = lung, conf.int = TRUE, nsim = 300)
  par <- attr(p$plot, "parametric")
  expect_true(all(c("lower", "upper") %in% names(par)))
  expect_gte(min(par$lower, na.rm = TRUE), 0)
  expect_lte(max(par$upper, na.rm = TRUE), 1)
  expect_true(all(par$lower <= par$surv + 1e-6 & par$surv <= par$upper + 1e-6,
                  na.rm = TRUE))
})

test_that("an intercept-only fit gives a single parametric curve", {
  fit <- survreg(Surv(time, status) ~ 1, data = lung, dist = "weibull")
  p <- ggsurvparametric(fit, data = lung)
  par <- attr(p$plot, "parametric")
  expect_equal(length(unique(par$.row)), 1L)
})

test_that("lognormal and exponential fits work", {
  for (d in c("lognormal", "exponential", "loglogistic")) {
    fit <- survreg(Surv(time, status) ~ sex, data = lung, dist = d)
    expect_s3_class(ggsurvparametric(fit, data = lung), "ggsurvplot")
  }
})

test_that("conf.int works for a fixed-scale distribution (exponential)", {
  # exponential/rayleigh have no estimated scale in vcov(); the bootstrap band
  # must not index a scale parameter that is not there.
  fit <- survreg(Surv(time, status) ~ sex, data = lung, dist = "exponential")
  p <- ggsurvparametric(fit, data = lung, conf.int = TRUE, nsim = 200)
  par <- attr(p$plot, "parametric")
  expect_true(all(c("lower", "upper") %in% names(par)))
  expect_gte(min(par$lower, na.rm = TRUE), 0)
})

test_that("a per-stratum-scale survreg model is rejected clearly", {
  fit <- survreg(Surv(time, status) ~ sex + strata(sex), data = lung,
                 dist = "weibull")
  expect_error(ggsurvparametric(fit, data = lung), "per-stratum scale")
})

test_that("a flexsurvreg fit works and equals its own survival summary", {
  skip_if_not_installed("flexsurv")
  fit <- flexsurv::flexsurvreg(Surv(time, status) ~ sex, data = lung,
                               dist = "weibull")
  p <- ggsurvparametric(fit, data = lung)
  expect_s3_class(p, "ggsurvplot")
  par <- attr(p$plot, "parametric")
  s <- summary(fit, newdata = data.frame(sex = 1),
               t = unique(par$time), tidy = TRUE)
  expect_equal(par$surv[par$.row == 1], s$est, tolerance = 1e-8)
})

test_that("non-parametric inputs error with a helpful message", {
  expect_error(ggsurvparametric(coxph(Surv(time, status) ~ sex, data = lung)),
               "ggadjustedcurves")
  expect_error(ggsurvparametric(survfit(Surv(time, status) ~ sex, data = lung)),
               "ggsurvplot")
  expect_error(ggsurvparametric(lm(1 ~ 1)), "survreg or flexsurvreg")
})

test_that("a transformed or multi-variable right-hand side still draws every curve", {
  skip_if_not_installed("survival")
  # The overlay used to be matched to the KM by rebuilding the strata label as
  # paste0(variable, "=", value). That misses a transform -- survfit stores
  # "factor(sex)=1", not "sex=1" -- and misses the padding survival::strata()
  # applies with several variables, so every covariate row came out NA and the
  # curve was silently never drawn while the legend still advertised the fit.
  n_na <- function(p) {
    ps <- attr(p$plot, "parametric")
    c(rows = nrow(ps), na = sum(is.na(ps$surv)))
  }

  # (a) a transform on the right-hand side: every row was NA, zero curves drawn
  f1 <- survival::survreg(survival::Surv(time, status) ~ factor(sex),
                          data = survival::lung, dist = "weibull")
  r1 <- n_na(suppressWarnings(ggsurvparametric(f1, data = survival::lung)))
  expect_gt(r1[["rows"]], 0)
  expect_equal(r1[["na"]], 0)

  # (b) two grouping variables: survival::strata() pads the labels to a common
  # width, so half the rows were NA and half the curves were missing
  d <- survival::lung[!is.na(survival::lung$ph.ecog), ]
  d$sexf <- factor(d$sex, labels = c("Male", "F"))     # unequal label widths
  d$e <- factor(ifelse(d$ph.ecog > 0, "bad", "good"))
  f2 <- survival::survreg(survival::Surv(time, status) ~ sexf + e, data = d,
                          dist = "weibull")
  p2 <- suppressWarnings(ggsurvparametric(f2, data = d))
  r2 <- n_na(p2)
  expect_equal(r2[["na"]], 0)

  # one fitted curve per stratum, aligned to the fit's own strata
  km <- survival::survfit(survival::Surv(time, status) ~ sexf + e, data = d)
  expect_equal(length(unique(attr(p2$plot, "parametric")$strata)),
               length(km$strata))
})

test_that("the parametric overlay is on the curve it belongs to", {
  skip_if_not_installed("survival")
  # a drawn-but-misaligned overlay is worse than a missing one: check the fitted
  # value tracks its own arm rather than merely being non-NA
  d <- survival::lung[!is.na(survival::lung$sex), ]
  d$sexf <- factor(d$sex, labels = c("M", "F"))
  f <- survival::survreg(survival::Surv(time, status) ~ sexf, data = d,
                         dist = "weibull")
  ps <- attr(suppressWarnings(ggsurvparametric(f, data = d))$plot, "parametric")
  # predict() for each stratum's own covariate row, computed independently
  for (lv in levels(d$sexf)) {
    got <- ps[as.character(ps$strata) == lv, ]
    got <- got[order(got$time), ]
    ref <- predict(f, newdata = data.frame(sexf = factor(lv, levels = levels(d$sexf))),
                   type = "quantile", p = 1 - got$surv)
    expect_equal(unname(as.numeric(ref)), got$time, tolerance = 1e-6, info = lv)
  }
})
