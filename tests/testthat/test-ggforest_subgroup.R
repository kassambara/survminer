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
