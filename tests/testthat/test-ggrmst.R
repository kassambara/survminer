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
  r_obs <- tab[tab$group == "rx=Obs", ]
  r_trt <- tab[tab$group == "rx=Lev+5FU", ]
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
  expect_true(is.na(tab$p.value[tab$group == "sex=1"]))   # per-group rows have no p
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
  tab2 <- ggrmst_difference(fit, data = d, ref.group = "ph.ecog=0")
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

# ---- regression: arm with no observation at or before tau ---------------------

test_that("RMST is tau, not 0, for an arm with no observation before tau", {
  # arm B's earliest observation (7) is after tau = 6.5, so its curve is
  # identically 1 on [0, tau] and its RMST is exactly tau. Reading it as 0 also
  # inverted the sign of the reported difference.
  d <- data.frame(time   = c(6, 10, 14, 20,  7, 11, 15, 22),
                  status = c(1,  1,  1,  0,  1,  1,  1,  0),
                  arm    = rep(c("A", "B"), each = 4))
  fit <- survival::survfit(survival::Surv(time, status) ~ arm, data = d)
  tab <- ggrmst_difference(fit, data = d, tau = 6.5)

  expect_equal(tab$rmst[tab$group == "arm=B"], 6.5)
  expect_equal(tab$se[tab$group == "arm=B"], 0)
  dr <- tab[grepl(" - ", tab$group), ]
  expect_equal(dr$rmst, 6.5 - tab$rmst[tab$group == "arm=A"])
  expect_gt(dr$rmst, 0)                       # B is better than A here
})

test_that("groups come from the fit's strata: order, labels and expressions", {
  d <- survival::lung[!is.na(survival::lung$ph.ecog) &
                        survival::lung$ph.ecog %in% c(0, 1), ]
  d$e <- factor(d$ph.ecog)
  d$s <- factor(d$sex, labels = c("M", "F"))
  fit <- survival::survfit(survival::Surv(time, status) ~ s + e, data = d)
  tab <- ggrmst_difference(fit, data = d)
  # labels AND order must match survfit's own strata, else the plot remap
  # attaches each arm's statistics to the wrong curve
  expect_equal(tab$group, unname(names(fit$strata)))

  # a transformed right-hand side is one group per stratum, not per distinct value
  f2 <- survival::survfit(survival::Surv(time, status) ~ (age > 60),
                          data = survival::lung)
  t2 <- ggrmst_difference(f2, data = survival::lung)
  expect_equal(t2$group[1:2], unname(names(f2$strata)))
  expect_equal(nrow(t2), 3L)                  # 2 groups + 1 difference row
})

test_that("a weighted survfit is refused rather than silently ignored", {
  d <- survival::lung
  d$w <- rep(c(1, 6), length.out = nrow(d))
  fw <- survival::survfit(survival::Surv(time, status) ~ sex, data = d, weights = w)
  expect_error(ggrmst_difference(fw, data = d), "does not support a weighted")
  expect_error(ggrmst(fw, data = d), "does not support a weighted")
})

test_that("conf.level is validated and is the confidence level, not a flag", {
  fit <- survival::survfit(survival::Surv(time, status) ~ sex, data = survival::lung)
  expect_error(ggrmst_difference(fit, data = survival::lung, conf.level = TRUE),
               "single number in \\(0, 1\\)")
  expect_error(ggrmst_difference(fit, data = survival::lung, conf.level = 42),
               "single number in \\(0, 1\\)")
  # a missing level must give the clear message, not R's "missing value where
  # TRUE/FALSE needed" from the comparison below it
  expect_error(ggrmst_difference(fit, data = survival::lung,
                                 conf.level = NA_real_),
               "single number in \\(0, 1\\)")
  # a narrower level gives a strictly narrower interval
  w95 <- ggrmst_difference(fit, data = survival::lung, conf.level = 0.95)
  w80 <- ggrmst_difference(fit, data = survival::lung, conf.level = 0.80)
  expect_true(all(w80$upper - w80$lower < w95$upper - w95$lower))
  # conf.int is NOT captured here: it reaches ggsurvplot() and draws the KM band.
  # ggrmst() always draws its own RMST ribbon, so counting GeomRibbon proves
  # nothing -- the band is a GeomConfint layer that is absent without conf.int.
  has_band <- function(p) any(vapply(p$layers,
                    function(l) inherits(l$geom, "GeomConfint"), logical(1)))
  expect_false(has_band(ggrmst(fit, data = survival::lung)))
  expect_true(has_band(ggrmst(fit, data = survival::lung, conf.int = TRUE)))
})

test_that("the ggrmst() subtitle names the contrast and honours legend.labs", {
  fit <- survival::survfit(survival::Surv(time, status) ~ sex, data = survival::lung)
  # the annotation must name the arms the legend actually shows
  p <- ggrmst(fit, data = survival::lung, legend.labs = c("Male", "Female"))
  expect_match(p$labels$subtitle, "Delta RMST (Female - Male)", fixed = TRUE)

  # a group label containing " - " must not be mistaken for a difference row:
  # the contrast is selected by position, never by parsing the label
  d <- survival::lung
  d$g <- factor(ifelse(d$sex == 1, "pre - post", "ctrl"))
  f2 <- survival::survfit(survival::Surv(time, status) ~ g, data = d)
  tab <- ggrmst_difference(f2, data = d)
  p2 <- ggrmst(f2, data = d)
  # the number in the subtitle is the difference row, not a per-arm RMST
  expect_match(p2$labels$subtitle,
               sprintf("= %.1f ", tab$rmst[nrow(tab)]), fixed = TRUE)
})

test_that("the strata helper survives cluster terms, hostile names and a bare formula env", {
  d <- survival::lung
  d$sexf <- factor(d$sex, labels = c("M", "F"))
  d$id   <- seq_len(nrow(d))

  # a cluster() term is not a stratum -- survfit drops it, so we must too,
  # otherwise every subject becomes its own "group"
  fc <- survival::survfit(survival::Surv(time, status) ~ sexf + cluster(id), data = d)
  expect_equal(ggrmst_difference(fc, data = d)$group[1:2],
               unname(names(fc$strata)))
  expect_equal(nrow(ggrmst_difference(fc, data = d)), 3L)   # 2 groups + difference

  # a grouping variable whose name collides with survival::strata()'s own
  # formals must not be passed to it as a named argument
  for (nm in c("sep", "shortlabel", "na.group")) {
    dd <- d; dd[[nm]] <- dd$sexf
    f <- eval(bquote(survival::survfit(
      survival::Surv(time, status) ~ .(as.name(nm)), data = dd)))
    expect_equal(ggrmst_difference(f, data = dd)$group[1:2],
                 unname(names(f$strata)),
                 info = paste("grouping variable named", nm))
  }

  # the grouping must not re-evaluate the Surv() response. Unqualified Surv() in
  # an environment that cannot resolve it is exactly the saved-and-reloaded fit:
  # the right-hand side is still perfectly usable.
  f2 <- survival::survfit(survival::Surv(time, status) ~ sexf, data = d)
  bare <- stats::as.formula("Surv(time, status) ~ sexf",
                            env = new.env(parent = baseenv()))
  expect_error(stats::model.frame(stats::terms(bare), data = d), "Surv")  # the trap
  expect_equal(levels(survminer:::.strata_group_from_formula(bare, d)),
               unname(names(f2$strata)))
})

test_that("ggrmst() rejects a numeric conf.int instead of silently banding", {
  fit <- survival::survfit(survival::Surv(time, status) ~ sex, data = survival::lung)
  # the stale spelling from before the rename must not be read as TRUE
  expect_error(ggrmst(fit, data = survival::lung, conf.int = 0.9),
               "must be TRUE or FALSE")
  expect_error(ggrmst(fit, data = survival::lung, conf.int = NA), "TRUE or FALSE")
})

test_that("a collapsed grouping variable with NAs keeps the fit's cohort", {
  # fit$strata is NULL here, so the group is relabelled "All" -- but the NAs must
  # survive, since they are what reproduces survfit()'s na.omit. Overwriting the
  # whole vector would compute every statistic on rows the fit excluded.
  d <- survival::lung
  d$g <- factor(ifelse(d$age > 65, "hi", NA))
  fit <- survival::survfit(survival::Surv(time, status) ~ g, data = d)
  expect_null(fit$strata)
  expect_lt(fit$n, nrow(d))                      # the fit dropped the NA rows

  tab <- ggrmst_difference(fit, data = d, tau = 500)
  expect_equal(tab$group, "All")
  # the estimate must be the one the plotted curve carries
  ref <- survminer:::.rmst_one_arm(
    d$time[!is.na(d$g)], d$status[!is.na(d$g)], tau = 500, alpha = 0.05)
  expect_equal(tab$rmst, unname(ref[["rmst"]]))
  expect_equal(tab$se,   unname(ref[["se"]]))

  ms <- attr(ggmilestone(fit, data = d, milestone.times = 365)$plot,
             "milestone.table")
  expect_equal(ms$surv, unname(summary(fit, times = 365)$surv))

  lm <- attr(suppressMessages(gglandmark(fit, data = d, landmark.time = 200))$plot,
             "landmark")
  # read the event indicator off the Surv response, as gglandmark() does: lung
  # codes status as 1 = censored / 2 = event, so a literal `status == 1` here
  # would mean "censored" and pass only by accident
  keep <- !is.na(d$g)
  resp <- survival::Surv(d$time[keep], d$status[keep])
  tt <- as.numeric(resp[, 1]); ss <- as.numeric(resp[, 2])
  expect_equal(lm$n.at.risk, sum(tt >= 200) - sum(tt == 200 & ss == 1))
})

test_that("the Surv-type error names the function the user called", {
  # a counting-process (start, stop] response is not right-censored Surv(time,
  # status); the message must name the entry point, not always ggrmst()
  d <- data.frame(t1 = c(0, 1, 0, 2, 0, 3), t2 = c(2, 3, 4, 5, 6, 7),
                  st = c(1, 0, 1, 0, 1, 1), g = factor(rep(c("a", "b"), 3)))
  f <- survival::survfit(survival::Surv(t1, t2, st) ~ g, data = d)
  expect_error(ggrmst_difference(f, data = d),
               "ggrmst_difference\\(\\) supports right-censored")
  expect_error(ggrmst(f, data = d), "ggrmst\\(\\) supports right-censored")
})
