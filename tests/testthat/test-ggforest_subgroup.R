# Subgroup forest plot: ggforest_subgroup().

library(survival)

# Shared fixture: colon, Lev+5FU vs Obs, categorical subgroups (matches the
# worked example from issue #366).
.cc <- function() {
  d <- colon[colon$etype == 2 & colon$rx %in% c("Obs", "Lev+5FU"), ]
  d$rx <- droplevels(d$rx)
  d$sex <- factor(d$sex, labels = c("Female", "Male"))
  d$age.grp <- factor(ifelse(d$age >= 60, ">=60", "<60"), levels = c("<60", ">=60"))
  d$differ <- factor(d$differ, labels = c("well", "moderate", "poor"))
  d
}

test_that("within-level hazard ratios match a manual coxph on each subset", {
  d <- .cc()
  fit <- coxph(Surv(time, status) ~ rx, data = d)
  tab <- survminer:::.subgroup_forest_table(fit, d, "rx",
            c("sex", "age.grp", "differ"), conf.level = 0.95, show.overall = TRUE)

  # overall
  m0 <- coxph(Surv(time, status) ~ rx, data = d)
  o <- tab[tab$type == "overall", ]
  expect_equal(o$hr, unname(exp(coef(m0))), tolerance = 1e-6)

  # a couple of levels, recomputed by hand
  for (spec in list(c("sex", "Male"), c("age.grp", ">=60"), c("differ", "well"))) {
    v <- spec[1]; lv <- spec[2]
    di <- d[!is.na(d[[v]]) & d[[v]] == lv, ]
    mi <- coxph(Surv(time, status) ~ rx, data = di)
    row <- tab[tab$type == "level" & tab$label == lv, ]
    expect_equal(row$hr[1], unname(exp(coef(mi))), tolerance = 1e-6)
    ci <- exp(confint(mi))
    expect_equal(row$lower[1], unname(ci[1]), tolerance = 1e-4)
    expect_equal(row$upper[1], unname(ci[2]), tolerance = 1e-4)
  }
})

test_that("interaction p-values match a manual likelihood-ratio test", {
  d <- .cc()
  fit <- coxph(Surv(time, status) ~ rx, data = d)
  tab <- survminer:::.subgroup_forest_table(fit, d, "rx",
            c("sex", "age.grp", "differ"), show.overall = TRUE)
  manual_p <- function(v) {
    dd <- d[stats::complete.cases(d[, c("time", "status", "rx", v)]), ]
    a <- coxph(as.formula(paste0("Surv(time, status) ~ rx + ", v)), data = dd)
    i <- coxph(as.formula(paste0("Surv(time, status) ~ rx * ", v)), data = dd)
    anova(a, i)[["Pr(>|Chi|)"]][2]
  }
  hdr <- tab[tab$type == "header", ]
  # headers are in subgroup order: sex, age.grp, differ
  expect_equal(hdr$pint[1], manual_p("sex"),     tolerance = 1e-6)
  expect_equal(hdr$pint[2], manual_p("age.grp"), tolerance = 1e-6)
  expect_equal(hdr$pint[3], manual_p("differ"),  tolerance = 1e-6)
})

test_that("ggforest_subgroup returns a ggplot that builds, and honours ggtheme", {
  d <- .cc()
  fit <- coxph(Surv(time, status) ~ rx, data = d)
  p <- ggforest_subgroup(fit, data = d, treatment = "rx",
                         subgroups = c(Sex = "sex", Age = "age.grp"))
  expect_s3_class(p, "ggplot")                       # composed panels, class ggplot
  expect_error(ggplot2::ggplot_build(p), NA)         # renders without error
  # a custom forest-panel theme is accepted
  expect_error(
    ggforest_subgroup(fit, data = d, treatment = "rx", subgroups = "sex",
                      ggtheme = ggplot2::theme_bw()), NA)
  # favours / no-precision / no-pinteraction variants all build
  expect_error(ggplot2::ggplot_build(
    ggforest_subgroup(fit, data = d, treatment = "rx", subgroups = "sex",
                      favours = c("A", "B"), point.size.by.precision = FALSE,
                      show.pinteraction = FALSE)), NA)
})

test_that("non-syntactic (backticked) subgroup / treatment names do not crash", {
  d <- .cc()
  d$"risk group" <- factor(ifelse(d$nodes >= 4, "high", "low"))
  fit <- coxph(Surv(time, status) ~ rx, data = d)
  expect_error(
    p <- ggforest_subgroup(fit, data = d, treatment = "rx",
                           subgroups = c(Risk = "risk group")), NA)
  # the interaction p for the non-syntactic variable is computed (not NA-by-crash)
  tab <- survminer:::.subgroup_forest_table(fit, d, "rx", "risk group")
  expect_true(is.finite(tab$pint[tab$type == "header"]))
})

test_that("named subgroups become the header labels", {
  d <- .cc()
  fit <- coxph(Surv(time, status) ~ rx, data = d)
  tab <- survminer:::.subgroup_forest_table(fit, d, "rx",
            c(Sex = "sex", Age = "age.grp"), show.overall = FALSE)
  expect_setequal(tab$label[tab$type == "header"], c("Sex", "Age"))
})

test_that("the N column counts match the subgroup subsets and the total", {
  d <- .cc()
  fit <- coxph(Surv(time, status) ~ rx, data = d)
  tab <- survminer:::.subgroup_forest_table(fit, d, "rx",
            c("sex", "age.grp"), show.overall = TRUE)
  # overall N = rows of data; per-level N = rows in that level
  expect_equal(tab$n[tab$type == "overall"], nrow(d))
  expect_equal(tab$n[tab$type == "level" & tab$label == "Male"],
               sum(d$sex == "Male"))
  expect_equal(tab$n[tab$type == "level" & tab$label == ">=60"],
               sum(d$age.grp == ">=60"))
  expect_true(is.na(tab$n[tab$type == "header"][1]))
  expect_equal(attr(tab, "n.overall"), nrow(d))
  # show.n = FALSE builds without the column (still a valid ggplot)
  expect_error(ggplot2::ggplot_build(
    ggforest_subgroup(fit, data = d, treatment = "rx",
                      subgroups = "sex", show.n = FALSE)), NA)
})

test_that("show.overall toggles the overall row", {
  d <- .cc()
  fit <- coxph(Surv(time, status) ~ rx, data = d)
  with_o <- survminer:::.subgroup_forest_table(fit, d, "rx", c("sex"), show.overall = TRUE)
  no_o   <- survminer:::.subgroup_forest_table(fit, d, "rx", c("sex"), show.overall = FALSE)
  expect_equal(sum(with_o$type == "overall"), 1L)
  expect_equal(sum(no_o$type == "overall"), 0L)
})

test_that("a treatment with more than two levels is refused", {
  d <- colon[colon$etype == 2, ]           # rx has 3 levels: Obs, Lev, Lev+5FU
  d$sex <- factor(d$sex, labels = c("Female", "Male"))
  fit <- coxph(Surv(time, status) ~ rx, data = d)
  expect_error(ggforest_subgroup(fit, data = d, treatment = "rx", subgroups = "sex"),
               "single hazard ratio")
})

test_that("a continuous subgroup variable is refused", {
  d <- .cc()
  fit <- coxph(Surv(time, status) ~ rx, data = d)
  expect_error(ggforest_subgroup(fit, data = d, treatment = "rx", subgroups = "age"),
               "continuous")
})

test_that("an unstratifiable level is dropped with a warning, not an error", {
  d <- .cc()
  # a level present in only ONE treatment arm has no treatment contrast, so its
  # within-level Cox fit cannot be estimated -> dropped, not an error.
  d$grp <- factor(ifelse(d$rx == "Obs" & seq_len(nrow(d)) %% 2 == 0,
                         "obs.only", "mixed"))
  fit <- coxph(Surv(time, status) ~ rx, data = d)
  expect_warning(
    p <- ggforest_subgroup(fit, data = d, treatment = "rx", subgroups = "grp"),
    "Dropped subgroup level")
  expect_s3_class(p, "ggplot")
})

test_that("non-coxph input and unknown variables are refused", {
  d <- .cc()
  fit <- coxph(Surv(time, status) ~ rx, data = d)
  expect_error(survminer:::.subgroup_forest_table(lm(time ~ status, data = d), d, "rx", "sex"),
               "coxph")
  expect_error(ggforest_subgroup(fit, data = d, treatment = "rx", subgroups = "nope"),
               "Not found")
})

test_that("conf.level is validated and the CI column header tracks it", {
  d <- survival::colon[survival::colon$etype == 2 &
                         survival::colon$rx %in% c("Obs", "Lev+5FU"), ]
  d$rx  <- droplevels(d$rx)
  d$sex <- factor(d$sex, labels = c("F", "M"))
  fit <- survival::coxph(survival::Surv(time, status) ~ rx, data = d)

  expect_error(ggforest_subgroup(fit, d, "rx", "sex", conf.level = TRUE),
               "single number in \\(0, 1\\)")
  expect_error(ggforest_subgroup(fit, d, "rx", "sex", conf.level = 42),
               "single number in \\(0, 1\\)")

  # the header must state the level actually used, not a hardcoded 95%. The plot
  # is an assembled multi-panel object, so walk the rendered grob tree for the
  # text labels (same technique as the ggforest_models() header test).
  labs <- function(p) {
    gt <- grid::grid.force(ggplot2::ggplotGrob(p)); L <- character()
    w <- function(g) { if (inherits(g, "gTree") && !is.null(g$children))
      for (n in names(g$children)) w(g$children[[n]])
      if (!is.null(g$label)) L <<- c(L, as.character(g$label)) }
    w(gt); L
  }
  L <- labs(ggforest_subgroup(fit, d, "rx", c(Sex = "sex"), conf.level = 0.80))
  expect_true(any(grepl("HR (80% CI)", L, fixed = TRUE)))
  expect_false(any(grepl("HR (95% CI)", L, fixed = TRUE)))
})

test_that("subgroup hazard ratios come from the model the user fitted", {
  # every row is a refit on a subset; dropping the model's weights reported an
  # unweighted hazard ratio under a weighted model, wrong by 15% here and in
  # opposite directions for the two subgroups
  cc <- survival::colon[survival::colon$etype == 2 &
                          survival::colon$rx %in% c("Obs", "Lev+5FU"), ]
  cc$rx  <- droplevels(cc$rx)
  cc$sex <- factor(cc$sex, labels = c("Female", "Male"))
  set.seed(9)
  cc$w <- runif(nrow(cc), 0.2, 3)
  m <- survival::coxph(survival::Surv(time, status) ~ rx, data = cc, weights = w)

  tab <- survminer:::.subgroup_forest_table(m, cc, "rx", c(Sex = "sex"),
                                            conf.level = 0.95, show.overall = TRUE)
  est <- tab[tab$type != "header", ]
  expect_gt(nrow(est), 0)

  for (i in seq_len(nrow(est))) {
    lab <- est$label[i]
    ref <- if (lab == "Overall") {
      exp(stats::coef(m))[[1]]
    } else {
      sub <- cc[cc$sex == lab, ]
      exp(stats::coef(survival::coxph(survival::Surv(time, status) ~ rx,
                                      data = sub, weights = w)))[[1]]
    }
    expect_equal(est$hr[i], ref, tolerance = 1e-8, info = lab)
  }
})

test_that("the tie handling of the fit is carried into the refits too", {
  cc <- survival::colon[survival::colon$etype == 2 &
                          survival::colon$rx %in% c("Obs", "Lev+5FU"), ]
  cc$rx  <- droplevels(cc$rx)
  cc$sex <- factor(cc$sex, labels = c("Female", "Male"))
  m <- survival::coxph(survival::Surv(time, status) ~ rx, data = cc,
                       ties = "breslow")
  tab <- survminer:::.subgroup_forest_table(m, cc, "rx", c(Sex = "sex"),
                                            conf.level = 0.95, show.overall = TRUE)
  ov <- tab$hr[tab$label == "Overall"]
  expect_equal(ov, exp(stats::coef(m))[[1]], tolerance = 1e-8)
  # and it is genuinely breslow, not efron
  expect_false(isTRUE(all.equal(
    ov, exp(stats::coef(survival::coxph(survival::Surv(time, status) ~ rx,
                                        data = cc, ties = "efron")))[[1]])))
})

test_that("an unweighted model is unaffected", {
  cc <- survival::colon[survival::colon$etype == 2 &
                          survival::colon$rx %in% c("Obs", "Lev+5FU"), ]
  cc$rx  <- droplevels(cc$rx)
  cc$sex <- factor(cc$sex, labels = c("Female", "Male"))
  m <- survival::coxph(survival::Surv(time, status) ~ rx, data = cc)
  tab <- survminer:::.subgroup_forest_table(m, cc, "rx", c(Sex = "sex"),
                                            conf.level = 0.95, show.overall = TRUE)
  expect_equal(tab$hr[tab$label == "Overall"], exp(stats::coef(m))[[1]],
               tolerance = 1e-8)
})

test_that("a model covariate named like the carried weights is not clobbered", {
  skip_if_not_installed("survival")
  cc <- survival::colon[survival::colon$etype == 2 &
                        survival::colon$rx %in% c("Obs", "Lev+5FU"), ]
  cc$rx <- droplevels(cc$rx)
  cc$sex <- factor(cc$sex, labels = c("F", "M"))
  set.seed(9)
  cc$w <- stats::runif(nrow(cc), 0.2, 3)
  cc$.ggf_weights <- cc$age          # a real covariate carrying the internal name
  m <- survival::coxph(
    survival::Surv(time, status) ~ rx + .ggf_weights, data = cc, weights = w
  )
  tab <- survminer:::.subgroup_forest_table(m, cc, "rx", c(Sex = "sex"),
                                conf.level = 0.95, show.overall = TRUE)

  got <- tab$hr[tab$label == "Overall"]
  expect_length(got, 1L)
  expect_equal(got, unname(exp(stats::coef(m))[["rxLev+5FU"]]), tolerance = 1e-10)

  # each subgroup must still refit on age, not on the weights
  for (lv in c("F", "M")) {
    sub <- cc[cc$sex == lv, ]
    ref <- survival::coxph(
      survival::Surv(time, status) ~ rx + .ggf_weights, data = sub, weights = w
    )
    shown <- tab$hr[tab$label == lv]
    expect_length(shown, 1L)
    expect_equal(shown, unname(exp(stats::coef(ref))[["rxLev+5FU"]]), tolerance = 1e-10)
  }
})

test_that("the interaction p-value is computed on the weighted fit, not an unweighted one", {
  cc <- survival::colon[survival::colon$etype == 2 &
                          survival::colon$rx %in% c("Obs", "Lev+5FU"), ]
  cc$rx  <- droplevels(cc$rx)
  cc$sex <- factor(cc$sex, labels = c("Female", "Male"))
  set.seed(9)
  cc$w <- runif(nrow(cc), 0.2, 3)
  m <- survival::coxph(survival::Surv(time, status) ~ rx, data = cc, weights = w)
  tab <- survminer:::.subgroup_forest_table(m, cc, "rx", c(Sex = "sex"),
                                            conf.level = 0.95, show.overall = TRUE)
  got <- tab$pint[tab$type == "header" & !is.na(tab$pint)][1]
  expect_false(is.na(got))

  # the weighted fit carries a robust variance, so the test is a Wald
  # chi-square on the interaction coefficients taken from it
  i <- survival::coxph(survival::Surv(time, status) ~ rx + sex + rx:sex, data = cc,
                       weights = w)
  k <- grep(":", names(stats::coef(i)), fixed = TRUE)
  b <- stats::coef(i)[k]
  V <- stats::vcov(i)[k, k, drop = FALSE]
  ref <- stats::pchisq(drop(t(b) %*% solve(V, b)), df = length(k), lower.tail = FALSE)
  expect_equal(got, ref, tolerance = 1e-10)

  # and it is not the unweighted value
  au <- survival::coxph(survival::Surv(time, status) ~ rx + sex, data = cc)
  iu <- survival::coxph(survival::Surv(time, status) ~ rx + sex + rx:sex, data = cc)
  expect_false(isTRUE(all.equal(got, stats::anova(au, iu)[["Pr(>|Chi|)"]][2])))
})

test_that("a robust = TRUE model gets robust intervals in every row", {
  skip_if_not_installed("survival")
  cc <- survival::colon[survival::colon$etype == 2 &
                        survival::colon$rx %in% c("Obs", "Lev+5FU"), ]
  cc$rx <- droplevels(cc$rx)
  cc$sex <- factor(cc$sex, labels = c("F", "M"))
  m <- survival::coxph(survival::Surv(time, status) ~ rx + sex, data = cc,
                       robust = TRUE)
  tab <- survminer:::.subgroup_forest_table(m, cc, "rx", c(Sex = "sex"),
                                            conf.level = 0.95, show.overall = TRUE)
  ci <- exp(stats::confint(m))["rxLev+5FU", ]
  ov <- tab[tab$label == "Overall", ]
  expect_equal(nrow(ov), 1L)
  expect_equal(ov$lower, unname(ci[1]), tolerance = 1e-10)
  expect_equal(ov$upper, unname(ci[2]), tolerance = 1e-10)

  for (lv in c("F", "M")) {
    ref <- survival::coxph(survival::Surv(time, status) ~ rx, data = cc[cc$sex == lv, ],
                           robust = TRUE)
    rci <- exp(stats::confint(ref))["rxLev+5FU", ]
    row <- tab[tab$label == lv, ]
    expect_equal(nrow(row), 1L)
    expect_equal(row$lower, unname(rci[1]), tolerance = 1e-10)
    expect_equal(row$upper, unname(rci[2]), tolerance = 1e-10)
  }
})

test_that("the clustering of the model is carried into the refits", {
  skip_if_not_installed("survival")
  cc <- survival::colon[survival::colon$etype == 2 &
                        survival::colon$rx %in% c("Obs", "Lev+5FU"), ]
  cc$rx <- droplevels(cc$rx)
  cc$sex <- factor(cc$sex, labels = c("F", "M"))
  m <- survival::coxph(survival::Surv(time, status) ~ rx + sex + cluster(id), data = cc)
  tab <- survminer:::.subgroup_forest_table(m, cc, "rx", c(Sex = "sex"),
                                            conf.level = 0.95, show.overall = TRUE)
  ci <- exp(stats::confint(m))["rxLev+5FU", ]
  ov <- tab[tab$label == "Overall", ]
  expect_equal(nrow(ov), 1L)
  # cluster-robust, not the naive interval
  expect_equal(ov$lower, unname(ci[1]), tolerance = 1e-10)
  expect_equal(ov$upper, unname(ci[2]), tolerance = 1e-10)
  naive <- exp(stats::coef(m)[["rxLev+5FU"]] +
               c(-1, 1) * stats::qnorm(0.975) * sqrt(diag(m$naive.var))[1])
  expect_false(isTRUE(all.equal(ov$lower, naive[1], tolerance = 1e-8)))
})

test_that("a robust fit gets a Wald interaction test, an integer-weighted one keeps the LRT", {
  skip_if_not_installed("survival")
  cc <- survival::colon[survival::colon$etype == 2 &
                        survival::colon$rx %in% c("Obs", "Lev+5FU"), ]
  cc$rx <- droplevels(cc$rx)
  cc$sex <- factor(cc$sex, labels = c("F", "M"))
  set.seed(9)
  cc$w  <- stats::runif(nrow(cc), 0.2, 3)     # non-integer -> robust variance
  cc$iw <- sample(1:3, nrow(cc), replace = TRUE)

  # --- robust (weighted) fit: Wald chi-square on the interaction coefficients
  mw <- survival::coxph(survival::Surv(time, status) ~ rx + sex, data = cc, weights = w)
  tw <- survminer:::.subgroup_forest_table(mw, cc, "rx", c(Sex = "sex"),
                                           conf.level = 0.95, show.overall = TRUE)
  got <- tw$pint[tw$type == "header"]
  got <- got[!is.na(got)]
  expect_length(got, 1L)

  iw <- survival::coxph(survival::Surv(time, status) ~ rx + sex + rx:sex,
                        data = cc, weights = w)
  k <- grep(":", names(stats::coef(iw)), fixed = TRUE)
  b <- stats::coef(iw)[k]
  V <- stats::vcov(iw)[k, k, drop = FALSE]
  ref <- stats::pchisq(drop(t(b) %*% solve(V, b)), df = length(k), lower.tail = FALSE)
  expect_equal(got, ref, tolerance = 1e-10)

  # it must NOT be the likelihood-ratio value, which is anticonservative here
  a0 <- survival::coxph(survival::Surv(time, status) ~ rx + sex, data = cc,
                        weights = w, robust = FALSE)
  i0 <- survival::coxph(survival::Surv(time, status) ~ rx + sex + rx:sex, data = cc,
                        weights = w, robust = FALSE)
  lrt <- stats::anova(a0, i0)[["Pr(>|Chi|)"]][2]
  expect_false(isTRUE(all.equal(got, lrt, tolerance = 1e-6)))

  # --- integer (frequency) weights: variance stays model-based, LRT retained
  mi <- survival::coxph(survival::Surv(time, status) ~ rx + sex, data = cc, weights = iw)
  ti <- survminer:::.subgroup_forest_table(mi, cc, "rx", c(Sex = "sex"),
                                           conf.level = 0.95, show.overall = TRUE)
  goti <- ti$pint[ti$type == "header"]
  goti <- goti[!is.na(goti)]
  expect_length(goti, 1L)
  ai <- survival::coxph(survival::Surv(time, status) ~ rx + sex, data = cc, weights = iw)
  ii <- survival::coxph(survival::Surv(time, status) ~ rx + sex + rx:sex, data = cc,
                        weights = iw)
  expect_equal(goti, stats::anova(ai, ii)[["Pr(>|Chi|)"]][2], tolerance = 1e-10)
})

test_that("weights that cannot be matched to the rows of data are an error", {
  skip_if_not_installed("survival")
  cc <- survival::colon[survival::colon$etype == 2 &
                        survival::colon$rx %in% c("Obs", "Lev+5FU"), ]
  cc$rx <- droplevels(cc$rx)
  cc$sex <- factor(cc$sex, labels = c("F", "M"))
  set.seed(9)
  cc$w <- stats::runif(nrow(cc), 0.2, 3)
  m <- survival::coxph(survival::Surv(time, status) ~ rx + sex, data = cc, weights = w)

  # a reordered data frame would attach each weight to the wrong subject
  set.seed(1)
  expect_error(
    survminer:::.subgroup_forest_table(m, cc[sample(nrow(cc)), ], "rx", c(Sex = "sex"),
                                       conf.level = 0.95, show.overall = TRUE),
    "case weights"
  )
  # so would a different number of rows
  expect_error(
    survminer:::.subgroup_forest_table(m, cc[1:600, ], "rx", c(Sex = "sex"),
                                       conf.level = 0.95, show.overall = TRUE),
    "case weights"
  )
  # the matching data frame is fine
  expect_silent(
    survminer:::.subgroup_forest_table(m, cc, "rx", c(Sex = "sex"),
                                       conf.level = 0.95, show.overall = TRUE)
  )
})

test_that("the tie handling carried is the model's own, not a same-named object in scope", {
  skip_if_not_installed("survival")
  cc <- survival::colon[survival::colon$etype == 2 &
                        survival::colon$rx %in% c("Obs", "Lev+5FU"), ]
  cc$rx <- droplevels(cc$rx)
  cc$sex <- factor(cc$sex, labels = c("F", "M"))

  # the model is built inside a wrapper, so `tie.method` exists only in that
  # wrapper's frame; the plot is then requested from HERE, where it does not.
  # Resolving `ties` from the caller's frame would fail or find something else --
  # it has to come from the model's own environment.
  mk <- function(d, tie.method = "breslow") {
    survival::coxph(survival::Surv(time, status) ~ rx + sex, data = d, ties = tie.method)
  }
  m <- mk(cc)
  tie.method <- "efron"          # a decoy visible from this frame
  tab <- survminer:::.subgroup_forest_table(m, cc, "rx", c(Sex = "sex"),
                                            conf.level = 0.95, show.overall = TRUE)
  got <- tab$hr[tab$label == "Overall"]
  expect_length(got, 1L)

  br <- unname(exp(stats::coef(survival::coxph(
    survival::Surv(time, status) ~ rx + sex, data = cc, ties = "breslow")))[["rxLev+5FU"]])
  ef <- unname(exp(stats::coef(survival::coxph(
    survival::Surv(time, status) ~ rx + sex, data = cc, ties = "efron")))[["rxLev+5FU"]])
  expect_false(isTRUE(all.equal(br, ef, tolerance = 1e-9)))   # the two must differ
  expect_equal(got, br, tolerance = 1e-10)
  expect_false(isTRUE(all.equal(got, ef, tolerance = 1e-9)))
})

test_that("integer weights reproduce the same fit as replicating each row", {
  skip_if_not_installed("survival")
  # A frequency weight of w is, by definition, w copies of that row. This checks
  # the weighted subgroup estimates against replication, which needs no second
  # implementation to be believed.
  cc <- survival::colon[survival::colon$etype == 2 &
                        survival::colon$rx %in% c("Obs", "Lev+5FU"), ]
  cc$rx <- droplevels(cc$rx)
  cc$sex <- factor(cc$sex, labels = c("F", "M"))
  set.seed(9)
  cc$iw <- sample(1:3, nrow(cc), replace = TRUE)

  # breslow: replication manufactures tied event times, which efron handles
  # differently from a weight (a known coxph difference, not one of ours)
  m <- survival::coxph(survival::Surv(time, status) ~ rx + age, data = cc,
                       weights = iw, ties = "breslow")
  tab <- survminer:::.subgroup_forest_table(m, cc, "rx", c(Sex = "sex"),
                                            conf.level = 0.95, show.overall = TRUE)

  for (lv in c("Overall", "F", "M")) {
    d  <- if (lv == "Overall") cc else cc[cc$sex == lv, ]
    ex <- d[rep(seq_len(nrow(d)), d$iw), ]
    ref <- survival::coxph(survival::Surv(time, status) ~ rx + age, data = ex,
                           ties = "breslow")
    got <- tab$hr[tab$label == lv]
    expect_length(got, 1L)
    expect_equal(got, unname(exp(stats::coef(ref))[["rxLev+5FU"]]), tolerance = 1e-7)
  }
})

test_that("the Wald interaction test uses the right degrees of freedom", {
  skip_if_not_installed("survival")
  cc <- survival::colon[survival::colon$etype == 2 &
                        survival::colon$rx %in% c("Obs", "Lev+5FU"), ]
  cc$rx <- droplevels(cc$rx)
  cc <- cc[!is.na(cc$differ), ]
  cc$differ3 <- factor(cc$differ, labels = c("well", "mod", "poor"))
  set.seed(9)
  cc$w <- stats::runif(nrow(cc), 0.2, 3)          # non-integer -> robust variance

  m <- survival::coxph(survival::Surv(time, status) ~ rx, data = cc, weights = w)
  tab <- survminer:::.subgroup_forest_table(m, cc, "rx", c(Diff = "differ3"),
                                            conf.level = 0.95, show.overall = FALSE)
  got <- tab$pint[tab$type == "header"]
  got <- got[!is.na(got)]
  expect_length(got, 1L)

  i <- survival::coxph(survival::Surv(time, status) ~ rx + differ3 + rx:differ3,
                       data = cc, weights = w)
  k <- grep(":", names(stats::coef(i)), fixed = TRUE)
  expect_equal(length(k), 2L)                     # a 3-level subgroup gives 2 df
  b <- stats::coef(i)[k]
  V <- stats::vcov(i)[k, k, drop = FALSE]
  chi <- drop(t(b) %*% solve(V, b))
  expect_equal(got, stats::pchisq(chi, df = 2, lower.tail = FALSE), tolerance = 1e-10)
  # a df of 1 would be a different, smaller p-value
  expect_false(isTRUE(all.equal(got, stats::pchisq(chi, df = 1, lower.tail = FALSE),
                                tolerance = 1e-6)))
})

test_that("an unestimable interaction coefficient does not discard the whole test", {
  skip_if_not_installed("survival")
  cc <- survival::colon[survival::colon$etype == 2 &
                        survival::colon$rx %in% c("Obs", "Lev+5FU"), ]
  cc$rx <- droplevels(cc$rx)
  cc <- cc[!is.na(cc$differ), ]
  set.seed(9)
  cc$w <- stats::runif(nrow(cc), 0.2, 3)
  # a declared level with no rows leaves an aliased interaction coefficient
  cc$dif4 <- factor(c("well", "mod", "poor")[cc$differ],
                    levels = c("well", "mod", "poor", "EMPTY"))
  cc$dif3 <- droplevels(cc$dif4)

  m <- survival::coxph(survival::Surv(time, status) ~ rx, data = cc, weights = w)
  t4 <- suppressWarnings(
    survminer:::.subgroup_forest_table(m, cc, "rx", c(Diff = "dif4"),
                                       conf.level = 0.95, show.overall = FALSE))
  t3 <- survminer:::.subgroup_forest_table(m, cc, "rx", c(Diff = "dif3"),
                                           conf.level = 0.95, show.overall = FALSE)
  g4 <- t4$pint[t4$type == "header"]; g4 <- g4[!is.na(g4)]
  g3 <- t3$pint[t3$type == "header"]; g3 <- g3[!is.na(g3)]
  expect_length(g4, 1L)                       # not NA, so the column survives
  expect_length(g3, 1L)
  expect_equal(g4, g3, tolerance = 1e-10)     # and equals the droplevels answer
})

test_that("weights survive rows the model dropped for missing covariates", {
  skip_if_not_installed("survival")
  cc <- survival::colon[survival::colon$etype == 2 &
                        survival::colon$rx %in% c("Obs", "Lev+5FU"), ]
  cc$rx <- droplevels(cc$rx)
  cc$sex <- factor(cc$sex, labels = c("F", "M"))
  set.seed(9)
  cc$w <- stats::runif(nrow(cc), 0.2, 3)
  cc$nodes[1:12] <- NA                     # coxph drops these 12 rows
  m <- survival::coxph(survival::Surv(time, status) ~ rx + nodes, data = cc, weights = w)
  expect_lt(m$n, nrow(cc))                 # the fit really is short

  # the weights must be put back on the right rows of the full frame
  tab <- survminer:::.subgroup_forest_table(m, cc, "rx", c(Sex = "sex"),
                                            conf.level = 0.95, show.overall = TRUE)
  got <- tab$hr[tab$label == "Overall"]
  expect_length(got, 1L)
  expect_equal(got, unname(exp(stats::coef(m))[["rxLev+5FU"]]), tolerance = 1e-10)
})

test_that("clustering that cannot be matched to data is an error", {
  skip_if_not_installed("survival")
  cc <- survival::colon[survival::colon$etype == 2 &
                        survival::colon$rx %in% c("Obs", "Lev+5FU"), ]
  cc$rx <- droplevels(cc$rx)
  cc$sex <- factor(cc$sex, labels = c("F", "M"))
  m <- survival::coxph(survival::Surv(time, status) ~ rx + sex + cluster(id), data = cc)
  expect_error(
    survminer:::.subgroup_forest_table(m, cc[1:600, ], "rx", c(Sex = "sex"),
                                       conf.level = 0.95, show.overall = TRUE),
    "cluster"
  )
})

test_that("an id-clustered model gets id-clustered intervals", {
  skip_if_not_installed("survival")
  set.seed(3)
  ng <- 150
  d <- data.frame(id = rep(seq_len(ng), each = 3))
  d$rx  <- factor(rep(stats::rbinom(ng, 1, 0.5), each = 3), labels = c("Obs", "Trt"))
  d$sex <- factor(rep(stats::rbinom(ng, 1, 0.5), each = 3), labels = c("F", "M"))
  u <- rep(stats::rnorm(ng, 0, 0.8), each = 3)
  d$t1 <- rep(c(0, 5, 10), ng)
  d$t2 <- d$t1 + stats::rexp(nrow(d), exp(0.4 * (d$rx == "Trt") + u))
  d$status <- 1L
  m <- survival::coxph(survival::Surv(t1, t2, status) ~ rx + sex, data = d, id = id)

  tab <- survminer:::.subgroup_forest_table(m, d, "rx", c(Sex = "sex"),
                                            conf.level = 0.95, show.overall = TRUE)
  ov <- tab[tab$label == "Overall", ]
  expect_equal(nrow(ov), 1L)
  expect_false(is.na(ov$hr))                       # rows must be estimable at all
  ci <- exp(stats::confint(m))["rxTrt", ]
  expect_equal(ov$lower, unname(ci[1]), tolerance = 1e-10)
  expect_equal(ov$upper, unname(ci[2]), tolerance = 1e-10)
  # and not the naive interval
  naive <- exp(stats::coef(m)[["rxTrt"]] +
               c(-1, 1) * stats::qnorm(0.975) * sqrt(diag(m$naive.var))[1])
  expect_false(isTRUE(all.equal(ov$lower, naive[1], tolerance = 1e-8)))
})

test_that("a data frame reordered only within tied responses is caught by row count", {
  skip_if_not_installed("survival")
  # documented limit: the guard compares responses, so a swap between rows with
  # identical time and status cannot be seen. This locks the documented scope.
  cc <- survival::colon[survival::colon$etype == 2 &
                        survival::colon$rx %in% c("Obs", "Lev+5FU"), ]
  cc$rx <- droplevels(cc$rx)
  cc$sex <- factor(cc$sex, labels = c("F", "M"))
  set.seed(9)
  cc$w <- stats::runif(nrow(cc), 0.2, 3)
  m <- survival::coxph(survival::Surv(time, status) ~ rx, data = cc, weights = w)
  # a wholesale reorder IS caught
  set.seed(1)
  expect_error(
    survminer:::.subgroup_forest_table(m, cc[sample(nrow(cc)), ], "rx", c(Sex = "sex"),
                                       conf.level = 0.95, show.overall = TRUE),
    "case weights"
  )
  # near-tied times must not trigger a false alarm (coxph's timefix snaps them)
  d <- cc
  d$time[2] <- d$time[1] + 1e-9
  m2 <- survival::coxph(survival::Surv(time, status) ~ rx, data = d, weights = w)
  expect_silent(
    survminer:::.subgroup_forest_table(m2, d, "rx", c(Sex = "sex"),
                                       conf.level = 0.95, show.overall = TRUE)
  )
})

test_that("a model with no stored response cannot smuggle mismatched weights through", {
  skip_if_not_installed("survival")
  # y = FALSE means there is no response to check `data` against, so the weights
  # cannot be shown to belong to these rows. That must be refused, not assumed.
  cc <- survival::colon[survival::colon$etype == 2 &
                        survival::colon$rx %in% c("Obs", "Lev+5FU"), ]
  cc$rx <- droplevels(cc$rx)
  cc$sex <- factor(cc$sex, labels = c("F", "M"))
  set.seed(9)
  cc$w <- stats::runif(nrow(cc), 0.2, 3)
  m <- survival::coxph(survival::Surv(time, status) ~ rx, data = cc,
                       weights = w, y = FALSE)
  expect_null(m$y)
  set.seed(3)
  expect_error(
    survminer:::.subgroup_forest_table(m, cc[sample(nrow(cc)), ], "rx", c(Sex = "sex"),
                                       conf.level = 0.95, show.overall = TRUE),
    "case weights"
  )
  # and the in-order frame is refused too, since it still cannot be verified
  expect_error(
    survminer:::.subgroup_forest_table(m, cc, "rx", c(Sex = "sex"),
                                       conf.level = 0.95, show.overall = TRUE),
    "case weights"
  )
})
