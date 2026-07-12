context("ggsurvplot_facet custom labeller")

# Regression tests for #667: ggsurvplot_facet() gains a `labeller` argument,
# forwarded to ggplot2 facet_wrap()/facet_grid(), so the panel strip labels can
# be formatted (e.g. ggplot2::label_both, ggplot2::as_labeller(...)). Default
# NULL keeps the current labels (byte-identical). A supplied labeller takes
# precedence over short.panel.labs and composes with panel.labs.
library(survival)

# Collect the text drawn in the facet strip grobs of a rendered plot.
strip_text <- function(p) {
  g <- ggplot2::ggplotGrob(p)
  idx <- which(grepl("strip", g$layout$name))
  labs <- character(0)
  collect <- function(gr) {
    if (inherits(gr, "text")) labs <<- c(labs, gr$label)
    if (!is.null(gr$children)) lapply(gr$children, collect)
    if (!is.null(gr$grobs)) lapply(gr$grobs, collect)
    invisible(NULL)
  }
  for (i in idx) collect(g$grobs[[i]])
  labs[!is.na(labs)]
}

drew_ok <- function(p) {
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)
  tryCatch({ suppressWarnings(print(p)); TRUE }, error = function(e) FALSE)
}

fit  <- survfit(Surv(time, status) ~ sex, data = colon)
fit2 <- survfit(Surv(time, status) ~ sex, data = colon)

test_that("no-regression: default labeller is unchanged (label_both) (#667)", {
  p <- ggsurvplot_facet(fit, colon, facet.by = "rx")
  # By default short.panel.labs = FALSE -> label_both; the stored facet labeller
  # must be byte-identical to what it was before the new argument existed.
  expect_true(identical(p$facet$params$labeller, ggplot2::label_both))
  # And the strips still read "rx: <level>".
  st <- strip_text(p)
  expect_true(length(st) > 0)
  expect_true(all(grepl("^rx: ", st)))
})

test_that("no-regression: short.panel.labs = TRUE still uses label_value (#667)", {
  p <- ggsurvplot_facet(fit, colon, facet.by = "rx", short.panel.labs = TRUE)
  expect_true(identical(p$facet$params$labeller, ggplot2::label_value))
})

test_that("a custom as_labeller() renames the strips for facet_wrap (#667)", {
  lab <- ggplot2::as_labeller(c("Obs" = "Observation",
                                "Lev" = "Levamisole",
                                "Lev+5FU" = "Lev + 5-FU"))
  p <- ggsurvplot_facet(fit, colon, facet.by = "rx", labeller = lab)
  expect_true(identical(p$facet$params$labeller, lab))
  st <- strip_text(p)
  expect_true("Observation" %in% st)
  expect_true("Levamisole"  %in% st)
  expect_true("Lev + 5-FU"  %in% st)
  # and it takes precedence over short.panel.labs (still renamed, not "rx: ...")
  expect_false(any(grepl("^rx: ", st)))
})

test_that("labeller overrides short.panel.labs (#667)", {
  # short.panel.labs = FALSE would give label_both ("rx: Obs"); a supplied
  # labeller must win.
  p <- ggsurvplot_facet(fit, colon, facet.by = "rx",
                        short.panel.labs = FALSE,
                        labeller = ggplot2::label_value)
  expect_true(identical(p$facet$params$labeller, ggplot2::label_value))
  st <- strip_text(p)
  expect_false(any(grepl("^rx: ", st)))
  expect_true("Obs" %in% st)
})

test_that("labeller works for two faceting variables (facet_grid) (#667)", {
  p <- ggsurvplot_facet(fit, colon, facet.by = c("rx", "adhere"),
                        labeller = ggplot2::label_value)
  expect_true(identical(p$facet$params$labeller, ggplot2::label_value))
  expect_true(drew_ok(p))
})

test_that("labeller composes with panel.labs (renamed levels reach the labeller) (#667)", {
  # panel.labs renames the underlying data levels; label_both then prefixes with
  # the (renamed) variable label. The labeller sees the renamed values.
  p <- ggsurvplot_facet(fit, colon, facet.by = "rx",
                        panel.labs = list(rx = c("A", "B", "C")),
                        labeller = ggplot2::label_value)
  st <- strip_text(p)
  expect_true(all(c("A", "B", "C") %in% st))
})

test_that("labeller does not break per-panel pval (#667)", {
  p <- suppressWarnings(
    ggsurvplot_facet(fit2, colon, facet.by = "rx", pval = TRUE,
                     labeller = ggplot2::label_value))
  expect_true(drew_ok(p))
})
