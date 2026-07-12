context("ggsurvplot negative survival times rendering")

# Regression tests for #389: with negative survival times, ggsurvplot() used to
# clip the x-axis at 0 (hiding the negative-time region) and inject a spurious
# (0, 1) origin point that sorted into the middle of the curve. It now behaves
# like base plot.survfit(): the panel spans the true time range and each curve
# starts at S = 1 from its first observed time. The #523 negative-time warning is
# retained (these tests suppress it). For all non-negative data the output is
# byte-identical (see the explicit no-regression tests at the bottom).
library(survival)

neg_df <- data.frame(
  time   = c(-10, -6, -3, 2, 5, 8, 12, 15),
  status = c(1, 1, 1, 0, 1, 1, 0, 1),
  grp    = rep(c("a", "b"), each = 4)
)

test_that("negative single-stratum: panel spans the true range, curve starts at first time (#389)", {
  fit <- survfit(Surv(time, status) ~ 1, data = neg_df)
  p <- suppressWarnings(ggsurvplot(fit, data = neg_df))
  b <- ggplot2::ggplot_build(p$plot)

  # panel left edge reaches at least the smallest observed time (not clipped at 0)
  expect_lte(b$layout$panel_params[[1]]$x.range[1], min(neg_df$time))

  ld <- b$data[[1]]
  # the connected curve now begins at the smallest observed time...
  expect_equal(min(ld$x, na.rm = TRUE), min(c(0, neg_df$time)))
  # ...at survival = 1 (the origin sits at the true left edge, not at x = 0)
  first <- ld[order(ld$x), ][1, ]
  expect_equal(first$x, min(neg_df$time))
  expect_equal(first$y, 1)
  # no spurious back-jump to x = 0 injected into the middle of the curve
  expect_false(any(ld$x == 0 & ld$y == 1))
})

test_that("negative multi-strata: every stratum starts at the shared global origin (#389)", {
  fit <- survfit(Surv(time, status) ~ grp, data = neg_df)
  p <- suppressWarnings(ggsurvplot(fit, data = neg_df))
  b <- ggplot2::ggplot_build(p$plot)
  origin <- min(c(0, neg_df$time))
  per_group_min <- tapply(b$data[[1]]$x, b$data[[1]]$group, min, na.rm = TRUE)
  expect_true(all(per_group_min == origin))
})

test_that("all-negative times: builds with a panel entirely below zero (#389)", {
  df <- data.frame(time = c(-10, -8, -6, -4, -2), status = c(1, 1, 0, 1, 1))
  fit <- survfit(Surv(time, status) ~ 1, data = df)
  p <- suppressWarnings(ggsurvplot(fit, data = df))
  b <- ggplot2::ggplot_build(p$plot)
  expect_s3_class(p, "ggsurvplot")
  expect_true(max(b$data[[1]]$x, na.rm = TRUE) < 0)
  expect_true(b$layout$panel_params[[1]]$x.range[2] < 0)
})

test_that("conf.int ribbon begins at the origin with no gap in the negative region (#389)", {
  fit <- survfit(Surv(time, status) ~ grp, data = neg_df)
  p <- suppressWarnings(ggsurvplot(fit, data = neg_df, conf.int = TRUE))
  b <- ggplot2::ggplot_build(p$plot)
  rib <- b$data[[which(sapply(b$data, function(L) all(c("ymin", "ymax") %in% names(L))))[1]]]
  origin <- min(c(0, neg_df$time))
  # ribbon starts at the origin, at upper = lower = 1, for every group
  by_g <- split(rib, rib$group)
  for (g in by_g) {
    gg <- g[order(g$x), ]
    expect_equal(gg$x[1], origin)
    expect_equal(gg$ymin[1], 1)
    expect_equal(gg$ymax[1], 1)
  }
  # no NA gap anywhere in the negative-time region
  neg_region <- rib[rib$x < 0, ]
  expect_false(any(is.na(neg_region$ymin) | is.na(neg_region$ymax)))
})

test_that("risk.table x-range is aligned with the curve panel for negative times (#389)", {
  fit <- survfit(Surv(time, status) ~ 1, data = neg_df)
  p <- suppressWarnings(ggsurvplot(fit, data = neg_df, risk.table = TRUE))
  curve_range <- ggplot2::ggplot_build(p$plot)$layout$panel_params[[1]]$x.range
  table_range <- ggplot2::ggplot_build(p$table)$layout$panel_params[[1]]$x.range
  expect_equal(curve_range, table_range)
})

# ---------------------------------------------------------------------------
# No-regression: non-negative data must be unchanged (origin exactly 0).
# ---------------------------------------------------------------------------

test_that("no-regression: non-negative data keeps the x-origin at exactly 0 (#389)", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  b <- ggplot2::ggplot_build(ggsurvplot(fit, data = lung)$plot)
  # every stratum still starts at x = 0 with survival = 1 (min(c(0, time)) == 0
  # for non-negative data, so the injected origin is unchanged)
  first_x <- tapply(b$data[[1]]$x, b$data[[1]]$group, min, na.rm = TRUE)
  expect_true(all(first_x == 0))
  first_rows <- do.call(rbind, lapply(split(b$data[[1]], b$data[[1]]$group),
                                      function(g) g[which.min(g$x), ]))
  expect_true(all(first_rows$y == 1))
})
