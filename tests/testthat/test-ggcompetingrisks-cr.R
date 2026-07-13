# Modernized ggcompetingrisks(): tidycmprsk input, Gray's test, naive-KM overlay.

library(survival)

.cr_data <- function() {
  set.seed(2)
  n <- 250
  data.frame(time = rexp(n),
             grp = factor(sample(c("A", "B"), n, replace = TRUE)),
             status = factor(sample(0:2, n, replace = TRUE), 0:2,
                             c("censor", "death", "relapse")))
}

test_that("a tidycmprsk::cuminc object is accepted and plotted", {
  skip_if_not_installed("tidycmprsk")
  d <- .cr_data()
  tc <- tidycmprsk::cuminc(Surv(time, status) ~ grp, data = d)
  p <- ggcompetingrisks(tc)
  expect_s3_class(p, "ggplot")
  # the CIF data covers both causes and both groups
  cr <- attr(p, "cr.data")
  expect_setequal(as.character(unique(cr$event)), c("death", "relapse"))
  expect_setequal(as.character(unique(cr$group)), c("A", "B"))
})

test_that("an ungrouped (~1) tidycmprsk::cuminc renders as one group", {
  skip_if_not_installed("tidycmprsk")
  d <- .cr_data()
  tc1 <- tidycmprsk::cuminc(Surv(time, status) ~ 1, data = d)
  expect_silent(p <- ggcompetingrisks(tc1))
  expect_s3_class(p, "ggplot")
  expect_setequal(as.character(unique(attr(p, "cr.data")$group)), "all")
  # pval has no between-group test to show, so it is skipped with a message
  expect_message(ggcompetingrisks(tc1, pval = TRUE), "no Gray's test")
  # the naive overlay works on the single cohort (the natural teaching case)
  expect_silent(p2 <- ggcompetingrisks(tc1, add.naive.km = TRUE, cause = "death"))
  expect_setequal(as.character(unique(attr(p2, "naive.km")$group)), "all")
})

test_that("pval annotates Gray's test with the correct per-cause p-values", {
  skip_if_not_installed("tidycmprsk")
  d <- .cr_data()
  tc <- tidycmprsk::cuminc(Surv(time, status) ~ grp, data = d)
  p <- ggcompetingrisks(tc, pval = TRUE)
  expect_match(p$labels$subtitle, "Gray's test")
  expect_match(p$labels$subtitle, "death")
  expect_match(p$labels$subtitle, "relapse")
  # the numbers equal cmprsk's Gray test
  gt <- attr(p, "grays.test")
  expect_equal(unname(gt[, "pv"]), unname(tc$cmprsk$Tests[, "pv"]))
  # a custom string is used verbatim
  p2 <- ggcompetingrisks(tc, pval = "Gray p = 0.04")
  expect_equal(p2$labels$subtitle, "Gray p = 0.04")
})

test_that("Gray's test is skipped (message) for survfitms and single-group inputs", {
  d <- .cr_data()
  f2 <- survfit(Surv(time, status, type = "mstate") ~ grp, data = d)
  expect_message(p <- ggcompetingrisks(f2, pval = TRUE), "no Gray's test")
  expect_null(p$labels$subtitle)
})

test_that("add.naive.km overlays a naive 1-KM that overestimates the CIF", {
  skip_if_not_installed("tidycmprsk")
  d <- .cr_data()
  tc <- tidycmprsk::cuminc(Surv(time, status) ~ grp, data = d)
  p <- ggcompetingrisks(tc, add.naive.km = TRUE, cause = "death")
  naive <- attr(p, "naive.km")
  expect_true(!is.null(naive))
  expect_setequal(as.character(unique(naive$event)), "death")
  # the naive curve is >= the CIF for the same cause/group at matched times
  cr <- attr(p, "cr.data")
  for (g in levels(naive$group)) {
    cif_g <- cr[cr$event == "death" & cr$group == g, ]
    nv_g  <- naive[naive$group == g, ]
    # step-interpolate the CIF to the naive times and compare
    cif_at <- stats::approx(cif_g$time, cif_g$est, xout = nv_g$time,
                            method = "constant", rule = 2)$y
    expect_true(all(nv_g$est >= cif_at - 1e-8))
  }
})

test_that("add.naive.km requires a cause when there are several", {
  skip_if_not_installed("tidycmprsk")
  d <- .cr_data()
  tc <- tidycmprsk::cuminc(Surv(time, status) ~ grp, data = d)
  expect_error(ggcompetingrisks(tc, add.naive.km = TRUE), "single cause")
  expect_error(ggcompetingrisks(tc, add.naive.km = TRUE, cause = "nope"),
               "must be one of")
})

test_that("add.naive.km is ignored (message) for a plain cmprsk::cuminc", {
  skip_if_not_installed("cmprsk")
  d <- .cr_data()
  cc <- cmprsk::cuminc(d$time, d$status, d$grp)
  expect_message(ggcompetingrisks(cc, add.naive.km = TRUE, cause = "death"),
                 "needs a tidycmprsk")
})

test_that("the conf.int ribbon stays within [0, 1]", {
  skip_if_not_installed("cmprsk")
  d <- .cr_data()
  cc <- cmprsk::cuminc(d$time, d$status, d$grp)
  p <- ggcompetingrisks(cc, conf.int = TRUE)
  b <- ggplot2::ggplot_build(p)
  rib <- b$data[[which(vapply(b$data, function(x) "ymin" %in% names(x),
                              logical(1)))[1]]]
  expect_gte(min(rib$ymin, na.rm = TRUE), 0)
  expect_lte(max(rib$ymax, na.rm = TRUE), 1)
})

test_that("default cuminc / survfitms output is unchanged (no new layers)", {
  skip_if_not_installed("cmprsk")
  d <- .cr_data()
  cc <- cmprsk::cuminc(d$time, d$status, d$grp)
  p <- ggcompetingrisks(cc)
  # exactly one line layer, no ribbon, no subtitle/caption by default
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomLine" %in% geoms)
  expect_false("GeomRibbon" %in% geoms)
  expect_null(p$labels$subtitle)
})
