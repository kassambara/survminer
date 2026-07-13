# Publication presets: ggsurvplot(preset = ...) and the theme companions.

library(survival)

.fit2 <- function() survfit(Surv(time, status) ~ sex, data = lung)
.geoms <- function(p) vapply(p$plot$layers, function(l) class(l$geom)[1], character(1))

test_that("preset = 'none' (default) is a strict no-op", {
  fit <- .fit2()
  a <- ggsurvplot(fit, data = lung, conf.int = TRUE, risk.table = TRUE)
  b <- ggsurvplot(fit, data = lung, conf.int = TRUE, risk.table = TRUE, preset = "none")
  # same layers, labels, and built plot data -> byte-identical behaviour
  expect_identical(.geoms(a), .geoms(b))
  expect_identical(a$plot$labels, b$plot$labels)
  expect_equal(ggplot2::ggplot_build(a$plot)$data,
               ggplot2::ggplot_build(b$plot)$data)
  # a plain call with no table/CI is also unchanged by preset = "none"
  c1 <- ggsurvplot(fit, data = lung)
  c2 <- ggsurvplot(fit, data = lung, preset = "none")
  expect_identical(.geoms(c1), .geoms(c2))
  expect_null(c2$table[["missing"]])
})

test_that("preset = 'publication' bundles the evidence-panel defaults", {
  p <- ggsurvplot(.fit2(), data = lung, preset = "publication")
  expect_true("GeomConfint" %in% .geoms(p))      # conf.int
  expect_false(is.null(p$table))                # risk.table
  expect_true(any(grepl("Geom(Segment|Vline|Hline)", .geoms(p))))  # median lines
  # percent y-axis
  built <- ggplot2::ggplot_build(p$plot)
  expect_match(paste(built$layout$panel_params[[1]]$y$get_labels(), collapse = " "), "%")
})

test_that("an explicit user argument overrides the preset default", {
  # conf.int = FALSE beats publication's conf.int = TRUE
  p <- ggsurvplot(.fit2(), data = lung, preset = "publication", conf.int = FALSE)
  expect_false("GeomConfint" %in% .geoms(p))
  # palette override is honoured
  expect_error(ggsurvplot(.fit2(), data = lung, preset = "publication",
                          palette = "npg"), NA)
  # risk.table = FALSE beats classic's risk.table = TRUE
  p2 <- ggsurvplot(.fit2(), data = lung, preset = "classic", risk.table = FALSE)
  expect_null(p2$table)
})

test_that("every preset builds without error or warning", {
  fit <- .fit2()
  for (pr in c("publication", "minimal", "classic", "presentation")) {
    expect_silent(built <- ggplot2::ggplot_build(
      ggsurvplot(fit, data = lung, preset = pr)$plot))
  }
})

test_that("an unknown preset name errors", {
  expect_error(ggsurvplot(.fit2(), data = lung, preset = "nejm"))
  expect_error(ggsurvplot(.fit2(), data = lung, preset = "lancet"))
})

test_that("the median-survival line is dropped when there are >2 strata", {
  d <- lung[!is.na(lung$ph.ecog) & lung$ph.ecog %in% 0:2, ]
  fit3 <- survfit(Surv(time, status) ~ ph.ecog, data = d)
  defs <- survminer:::.resolve_preset("publication", fit3)
  expect_identical(defs$surv.median.line, "none")
  # two strata keeps it
  defs2 <- survminer:::.resolve_preset("publication", .fit2())
  expect_identical(defs2$surv.median.line, "hv")
})

test_that("a preset reaches the data-frame (surv_summary) input path", {
  fit <- .fit2()
  ss <- surv_summary(fit, data = lung)
  # publication sets risk.table = TRUE; it must also apply to the df path
  p <- ggsurvplot(ss, data = lung, preset = "publication")
  expect_false(is.null(p$table))
  # preset = "none" leaves the df path unchanged (no table by default)
  expect_null(ggsurvplot(ss, data = lung, preset = "none")$table)
})

test_that("presets behave sensibly on a single-arm (null) model", {
  fit1 <- survfit(Surv(time, status) ~ 1, data = lung)
  # minimal keeps conf.int = FALSE even on the null-strata path (no band)
  pm <- ggsurvplot(fit1, data = lung, preset = "minimal")
  expect_false("GeomConfint" %in% .geoms(pm))
  # p-value is dropped when there is nothing to compare -> no null-model warning
  expect_warning(invisible(ggplot2::ggplot_build(
    ggsurvplot(fit1, data = lung, preset = "publication")$plot)), NA)
  defs <- survminer:::.resolve_preset("publication", fit1)
  expect_false(defs$pval)
})

test_that("the classic preset distinguishes curves by line type (greyscale-safe)", {
  expect_identical(survminer:::.resolve_preset("classic", .fit2())$linetype, "strata")
})

test_that("theme companions return usable ggplot2 themes", {
  for (th in list(theme_surv_classic(), theme_surv_minimal(),
                  theme_surv_bold())) {
    expect_s3_class(th, "theme")
  }
  # drop-in as ggtheme
  expect_error(ggplot2::ggplot_build(
    ggsurvplot(.fit2(), data = lung, ggtheme = theme_surv_bold())$plot), NA)
})

test_that("a preset ggtheme is inherited by the risk table (tables.theme)", {
  # publication injects ggtheme; the table should not fall back to the default
  p <- ggsurvplot(.fit2(), data = lung, preset = "publication")
  expect_s3_class(p$table, "ggplot")
  # an explicit tables.theme still wins
  expect_error(ggsurvplot(.fit2(), data = lung, preset = "publication",
                          tables.theme = theme_cleantable()), NA)
})
