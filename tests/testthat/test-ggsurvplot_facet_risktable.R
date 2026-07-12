# Faceted risk tables and per-panel p-values in the strip labels
# (ggsurvplot_facet risk.table = TRUE and pval.in.label = TRUE).

library(survival)

# Collect every text-grob label drawn in a gtable (recursively).
.facet_grob_labels <- function(g) {
  gf <- grid::grid.force(g)
  out <- character()
  walk <- function(gr) {
    if (inherits(gr, "text") && !is.null(gr$label))
      out[[length(out) + 1L]] <<- paste(as.character(gr$label), collapse = " ")
    if (!is.null(gr$children)) for (ch in gr$children) walk(ch)
  }
  walk(gf)
  trimws(out)
}

test_that("risk.table = FALSE returns the plain faceted ggplot (no regression)", {
  fit <- survfit(Surv(time, status) ~ sex, data = colon)
  p <- ggsurvplot_facet(fit, colon, facet.by = "rx")
  expect_s3_class(p, "ggplot")
  expect_false(inherits(p, "gtable"))
  expect_false(inherits(p, "ggsurvplot_facet"))
})

test_that("risk.table = TRUE returns an aligned gtable of class ggsurvplot_facet", {
  # Runs on CRAN: exercises the whole risk-table pipeline (core re-run, relabel,
  # facet, gtable stacking) and its print method with version-robust assertions.
  fit <- survfit(Surv(time, status) ~ sex, data = colon)
  g <- suppressWarnings(suppressMessages(
    ggsurvplot_facet(fit, colon, facet.by = "rx", risk.table = TRUE)
  ))
  expect_s3_class(g, "ggsurvplot_facet")
  expect_s3_class(g, "gtable")
  # It draws without error through its print method.
  pdf(NULL); on.exit(dev.off())
  expect_error(print(g), NA)
})

test_that("a faceted risk table is refused (with a warning) for two faceting variables", {
  fit <- survfit(Surv(time, status) ~ sex, data = colon)
  expect_warning(
    p <- ggsurvplot_facet(fit, colon, facet.by = c("rx", "adhere"), risk.table = TRUE),
    "single .*facet.by. variable"
  )
  # Falls back to the plain faceted ggplot, never a mislabelled table.
  expect_s3_class(p, "ggplot")
  expect_false(inherits(p, "gtable"))
})

test_that("a faceted risk table tolerates an unused grouping factor level", {
  skip_on_cran()
  d <- colon
  d$g <- factor(ifelse(d$sex == 0, "A", "B"), levels = c("A", "B", "C"))  # C: 0 rows
  fit <- survfit(Surv(time, status) ~ g, data = d)
  expect_error(
    g <- suppressWarnings(suppressMessages(
      ggsurvplot_facet(fit, d, facet.by = "rx", risk.table = TRUE)
    )),
    NA
  )
  expect_s3_class(g, "ggsurvplot_facet")
})

test_that("tables.y.text = FALSE warns and keeps the labels for a faceted risk table", {
  fit <- survfit(Surv(time, status) ~ sex, data = colon)
  expect_warning(
    g <- ggsurvplot_facet(fit, colon, facet.by = "rx", risk.table = TRUE,
                          tables.y.text = FALSE),
    "tables.y.text"
  )
  expect_s3_class(g, "ggsurvplot_facet")
  # Labels are kept (not hidden) so the rows stay identifiable, and the neutral
  # colour override merges cleanly (no element-class crash).
  pdf(NULL); on.exit(dev.off())
  expect_error(print(g), NA)
})

test_that("a literal `color` does not leak the raw combined strata into the table", {
  skip_on_cran()
  # The y axis is relabelled from the grouping column, not the `color` argument, so a
  # literal colour still yields clean group labels (not "sex=0, rx=Obs"). The label
  # text is verified in-review; here we assert the pipeline builds and does not leak
  # the raw combined strata string.
  fit <- survfit(Surv(time, status) ~ sex, data = colon)
  g <- suppressWarnings(suppressMessages(
    ggsurvplot_facet(fit, colon, facet.by = "rx", color = "red", risk.table = TRUE)
  ))
  expect_s3_class(g, "ggsurvplot_facet")
  labs <- .facet_grob_labels(g)
  expect_false(any(grepl("sex=0, rx=", labs)))
  expect_false(any(grepl("\\.strata\\.", labs)))
})

test_that("faceted risk-table y labels are the clean within-panel group, not the raw strata", {
  skip_on_cran()
  fit <- survfit(Surv(time, status) ~ sex, data = colon)
  g <- suppressWarnings(suppressMessages(
    ggsurvplot_facet(fit, colon, facet.by = "rx", risk.table = TRUE)
  ))
  labs <- .facet_grob_labels(g)
  # The combined strata string is cleaned away; only the group value remains.
  expect_true("0" %in% labs)
  expect_true("1" %in% labs)
  expect_false(any(grepl("^sex=0, rx=", labs)))
  expect_false(any(grepl("\\.strata\\.", labs)))
  # (The per-group n.risk counts and their alignment to these labels are checked
  # device-independently in the "no swap" test below via ggplot_build.)
})

test_that("faceted risk-table labels map to the correct group / count (no swap)", {
  skip_on_cran()
  # Rebuild the exact object the function builds (refit + core table + relabel)
  # and check that the label shown at each y position names the group whose
  # n.risk sits there -- the invariant that broke when the y scale was replaced
  # without preserving ggsurvtable's rev() ordering.
  data <- colon
  rf <- surv_fit(Surv(time, status) ~ sex + rx, data = data)
  core <- suppressWarnings(suppressMessages(
    ggsurvplot_core(rf, data = data, color = "sex", risk.table = TRUE)
  ))
  tab <- core$table
  lv  <- as.character(levels(tab$data$strata))
  grp <- as.character(tab$data$sex[match(lv, as.character(tab$data$strata))])
  tab2 <- suppressMessages(
    tab + ggplot2::scale_y_discrete(breaks = lv, labels = rev(grp))
  )
  tp <- survminer:::.facet(tab2, "rx", scales = "free_y")
  b  <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(tp)))
  lay <- b$layout$layout
  obs.panel <- as.integer(lay$PANEL[lay$rx == "Obs"])
  y.labels <- b$layout$panel_params[[obs.panel]]$y$get_labels()  # bottom -> top
  # sex=0 (n.risk 298) is the top row, sex=1 (332) the bottom row.
  expect_identical(y.labels[length(y.labels)], "0")
  expect_identical(y.labels[1], "1")
})

test_that("special characters in a factor level survive the risk-table relabelling", {
  skip_on_cran()
  d <- colon
  d$grp <- factor(ifelse(d$sex == 0, "a=1, b", "c;d"))
  fit <- survfit(Surv(time, status) ~ grp, data = d)
  g <- suppressWarnings(suppressMessages(
    ggsurvplot_facet(fit, d, facet.by = "rx", risk.table = TRUE)
  ))
  labs <- .facet_grob_labels(g)
  expect_true("a=1, b" %in% labs)
  expect_true("c;d" %in% labs)
})

test_that("pval.in.label puts the p-value in the strip label and not on the panel", {
  fit <- survfit(Surv(time, status) ~ sex, data = colon)
  p <- ggsurvplot_facet(fit, colon, facet.by = "rx",
                        pval = TRUE, pval.in.label = TRUE)
  expect_s3_class(p, "ggplot")
  # No on-panel p-value layer was added (it lives in the strip instead).
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomText" %in% geoms)
  # The strip labels carry the per-panel p-value text.
  b <- ggplot2::ggplot_build(p)
  labelled <- b$plot$facet$params$labeller(list(rx = levels(factor(colon$rx))))
  expect_true(any(grepl("p =", unlist(labelled))))
})

test_that("pval.in.label composes with p.adjust.method (adjusted label)", {
  fit <- survfit(Surv(time, status) ~ sex, data = colon)
  p <- ggsurvplot_facet(fit, colon, facet.by = "rx", pval = TRUE,
                        pval.in.label = TRUE, p.adjust.method = "BH")
  b <- ggplot2::ggplot_build(p)
  labelled <- b$plot$facet$params$labeller(list(rx = levels(factor(colon$rx))))
  expect_true(any(grepl("adj\\.p =", unlist(labelled))))
})

test_that("pval.in.label warns and falls back for two faceting variables", {
  fit <- survfit(Surv(time, status) ~ sex, data = colon)
  expect_warning(
    p <- ggsurvplot_facet(fit, colon, facet.by = c("rx", "adhere"),
                          pval = TRUE, pval.in.label = TRUE),
    "single faceting variable"
  )
  # Falls back to drawing the p-value on the panels: still a ggplot with a text layer.
  expect_s3_class(p, "ggplot")
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomText" %in% geoms)
})

test_that("pval.in.label without pval warns and leaves the labels unchanged", {
  fit <- survfit(Surv(time, status) ~ sex, data = colon)
  expect_warning(
    p <- ggsurvplot_facet(fit, colon, facet.by = "rx", pval.in.label = TRUE),
    "requires `pval = TRUE`"
  )
  expect_s3_class(p, "ggplot")
})
