# ggsurvplot(output = "ggplot") -- a single composable survival-curve ggplot (P0.1a).

library(survival)

fit <- survfit(Surv(time, status) ~ sex, data = lung)

test_that("output = 'ggplot' returns a single ggplot, not the compound object", {
  p <- ggsurvplot(fit, data = lung, output = "ggplot")
  expect_s3_class(p, "ggplot")
  expect_false(inherits(p, "ggsurvplot"))
})

test_that("the returned ggplot composes with layers, scales, facets and ggsave", {
  p <- ggsurvplot(fit, data = lung, output = "ggplot")
  expect_error(ggplot2::ggplot_build(p + ggplot2::geom_hline(yintercept = 0.5)), NA)
  expect_error(ggplot2::ggplot_build(p + ggplot2::annotate("text", x = 500, y = 0.5, label = "x")), NA)
  # RMST/CIF-style ribbon overlay (the downstream P1.1 use case)
  rib <- data.frame(time = c(0, 300))
  expect_error(
    ggplot2::ggplot_build(
      p + ggplot2::geom_ribbon(ggplot2::aes(x = time, ymin = 0, ymax = 0.5),
                               data = rib, inherit.aes = FALSE, alpha = 0.2)),
    NA)
  # facet by the strata variable (external-covariate faceting stays ggsurvplot_facet's job)
  expect_error(ggplot2::ggplot_build(p + ggplot2::facet_wrap(~strata)), NA)
  tf <- tempfile(fileext = ".png"); on.exit(unlink(tf), add = TRUE)
  expect_error(ggplot2::ggsave(tf, p, width = 6, height = 4), NA)
})

test_that("pval / conf.int / surv.median.line layers are preserved on the bare curve", {
  p <- ggsurvplot(fit, data = lung, pval = TRUE, conf.int = TRUE,
                  surv.median.line = "hv", output = "ggplot")
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomText" %in% geoms)     # p-value annotation
  expect_true("GeomConfint" %in% geoms)  # confidence band
  expect_true(any(grepl("Segment|Vline|Hline", geoms)))  # median line
})

test_that("a requested table/panel is dropped with a warning; plain call is quiet", {
  expect_warning(ggsurvplot(fit, data = lung, risk.table = TRUE, output = "ggplot"),
                 "cannot be embedded")
  expect_warning(ggsurvplot(fit, data = lung, cumevents = TRUE, output = "ggplot"),
                 "cannot be embedded")
  expect_warning(ggsurvplot(fit, data = lung, ncensor.plot = TRUE, output = "ggplot"),
                 "cannot be embedded")
  expect_no_warning(ggsurvplot(fit, data = lung, output = "ggplot"))
})

test_that("output = 'classic' (and the default) is unchanged", {
  d  <- ggsurvplot(fit, data = lung)
  cl <- ggsurvplot(fit, data = lung, output = "classic")
  expect_s3_class(d, "ggsurvplot")
  expect_s3_class(cl, "ggsurvplot")
  # same curve build as the default
  expect_equal(ggplot2::ggplot_build(cl$plot)$data,
               ggplot2::ggplot_build(d$plot)$data)
  # output = "ggplot" returns exactly the default's $plot curve
  g <- ggsurvplot(fit, data = lung, output = "ggplot")
  expect_equal(ggplot2::ggplot_build(g)$data,
               ggplot2::ggplot_build(d$plot)$data)
})

test_that("an invalid output value is rejected", {
  expect_error(ggsurvplot(fit, data = lung, output = "foo"))
})

test_that("output = 'ggplot' works for the facet path (bare faceted ggplot)", {
  pf <- suppressWarnings(ggsurvplot(fit, data = lung, facet.by = "sex", output = "ggplot"))
  expect_s3_class(pf, "ggplot")
  expect_false(inherits(pf, "gtable"))
})

test_that("group.by falls back to the classic object WITH its tables intact", {
  # group.by produces several plots (not one), so output='ggplot' cannot apply; it
  # must fall back to the full classic object -- not a table-stripped one.
  gb <- survfit(Surv(time, status) ~ sex, data = colon)
  expect_warning(
    r <- ggsurvplot(gb, data = colon, group.by = "rx", risk.table = TRUE,
                    output = "ggplot"),
    "not available for"
  )
  expect_s3_class(r, "ggsurvplot_list")
  # the risk table the user asked for survives the fall-back
  expect_false(is.null(r[[1]]$table))
})
