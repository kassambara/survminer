# Restricted mean survival time: ggrmst() / ggrmst_difference().

library(survival)

test_that("ggrmst_difference matches survRM2::rmst2() (numeric parity)", {
  skip_if_not_installed("survRM2")
  # colon has 0/1 status -- no 1/2 recoding ambiguity.
  d <- colon[colon$etype == 2 & colon$rx %in% c("Obs", "Lev+5FU"), ]
  d$rx <- droplevels(d$rx)
  fit <- survfit(Surv(time, status) ~ rx, data = d)
  tab <- ggrmst_difference(fit, data = d)

  arm <- as.integer(d$rx == "Lev+5FU")   # rmst2: 0/1 arm
  ref <- survRM2::rmst2(time = d$time, status = d$status, arm = arm)

  # default tau
  expect_equal(tab$tau[1], ref$tau, tolerance = 1e-6)
  # per-arm RMST + SE
  r_obs <- tab[tab$group == "Obs", ]
  r_trt <- tab[tab$group == "Lev+5FU", ]
  expect_equal(r_obs$rmst, ref$RMST.arm0$rmst[[1]], tolerance = 1e-4)
  expect_equal(r_obs$se,   ref$RMST.arm0$rmst[[2]], tolerance = 1e-4)
  expect_equal(r_trt$rmst, ref$RMST.arm1$rmst[[1]], tolerance = 1e-4)
  expect_equal(r_trt$se,   ref$RMST.arm1$rmst[[2]], tolerance = 1e-4)
  # difference + CI + p (rmst2 unadjusted.result row 1 = RMST diff arm1 - arm0)
  dr <- tab[grepl(" - ", tab$group), ]
  expect_equal(dr$rmst,    ref$unadjusted.result[1, 1], tolerance = 1e-4)
  expect_equal(dr$lower,   ref$unadjusted.result[1, 2], tolerance = 1e-4)
  expect_equal(dr$upper,   ref$unadjusted.result[1, 3], tolerance = 1e-4)
  expect_equal(dr$p.value, ref$unadjusted.result[1, 4], tolerance = 1e-4)
})

test_that("ggrmst_difference structure and columns", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  tab <- ggrmst_difference(fit, data = lung)
  expect_true(all(c("group", "rmst", "se", "lower", "upper", "tau", "p.value") %in% names(tab)))
  expect_equal(nrow(tab), 3L)                 # 2 groups + 1 difference row
  expect_true(any(grepl(" - ", tab$group)))
  expect_true(is.na(tab$p.value[tab$group == "1"]))   # per-group rows have no p
})

test_that("tau default is the admissible max; out-of-range tau errors", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  tab <- ggrmst_difference(fit, data = lung)
  # both lung arms end censored -> tau = min of the arm max times (965)
  expect_equal(tab$tau[1], 965)
  expect_error(ggrmst_difference(fit, data = lung, tau = 5000), "beyond the range")
  expect_error(ggrmst_difference(fit, data = lung, tau = -1), "positive")
  # a smaller in-range tau is honored
  expect_equal(ggrmst_difference(fit, data = lung, tau = 500)$tau[1], 500)
})

test_that("ggrmst() returns a themeable ggplot with the RMST band and tau line (2 groups)", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  p <- ggrmst(fit, data = lung)
  expect_s3_class(p, "ggplot")
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomRibbon" %in% geoms)   # the delta band
  expect_true("GeomVline" %in% geoms)    # the tau line
  expect_match(p$labels$subtitle, "RMST")
  expect_error(ggplot2::ggplot_build(p + ggplot2::theme_bw()), NA)
})

test_that("ggrmst() facets per arm for 3+ groups (no invented pairwise difference)", {
  d <- lung[lung$ph.ecog %in% 0:2 & !is.na(lung$ph.ecog), ]
  fit <- survfit(Surv(time, status) ~ ph.ecog, data = d)
  p <- suppressWarnings(ggrmst(fit, data = d))
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(b$layout$layout), 3L)   # 3 panels, not 6
  tab <- ggrmst_difference(fit, data = d)
  expect_equal(nrow(tab), 3L)               # 3 groups, no difference rows by default
  expect_false(any(grepl(" - ", tab$group)))
  # with a reference group, differences vs the reference appear
  tab2 <- ggrmst_difference(fit, data = d, ref.group = "0")
  expect_equal(sum(grepl(" - ", tab2$group)), 2L)
})

test_that("competing-risks / multi-state and left-censored data are refused", {
  # multi-state (competing risks): status factor -> survfitms, Surv type "mright"
  set.seed(1)
  d <- data.frame(time = c(2, 4, 6, 8, 3, 5, 7, 9),
                  ev = factor(c(0, 1, 2, 1, 0, 2, 1, 2)),
                  g = rep(c("a", "b"), each = 4))
  fms <- survfit(Surv(time, ev) ~ g, data = d)
  expect_error(ggrmst_difference(fms, data = d), "right-censored")
  # left-censored
  fl <- survfit(Surv(time, rep(1, 8), type = "left") ~ g, data = d)
  expect_error(ggrmst_difference(fl, data = d), "right-censored")
})

test_that("tau default matches survRM2 when a group's max time has an event+censor tie", {
  # arm a max time 8 carries both an event and a censoring -> censored-ending (tau
  # limited to 8), matching survRM2's min(status at max) rule.
  d <- data.frame(time   = c(2, 4, 8, 8, 3, 6, 9, 10),
                  status = c(1, 1, 1, 0, 1, 1, 1, 0),
                  arm    = rep(c("a", "b"), each = 4))
  fit <- survfit(Surv(time, status) ~ arm, data = d)
  expect_equal(ggrmst_difference(fit, data = d)$tau[1], 8)
})

test_that("a single-group fit gives an overall RMST", {
  fit <- survfit(Surv(time, status) ~ 1, data = lung)
  tab <- ggrmst_difference(fit, data = lung)
  expect_equal(nrow(tab), 1L)
  expect_true(is.finite(tab$rmst))
  p <- ggrmst(fit, data = lung)
  expect_s3_class(p, "ggplot")
})
