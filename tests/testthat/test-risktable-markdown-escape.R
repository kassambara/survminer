context("risk-table labels with markdown/HTML special characters")

# Regression test for #532: the risk-table y-axis tick labels are rendered with
# ggtext::element_markdown() (coloured per strata), so a legend label containing
# "<", ">" or "&" (e.g. "> 1 Risk factor") was parsed as an HTML tag and errored
# with "gridtext has encountered a tag ... <blockquote>". The labels are now
# HTML-escaped before markdown rendering so they display literally.
library(survival)

drew_ok <- function(p) {
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)
  # suppress device font warnings (e.g. the pdf device substituting a Unicode
  # glyph like U+2264); we are only asserting that drawing does not ERROR.
  tryCatch({ suppressWarnings(grid::grid.draw(p)); TRUE }, error = function(e) FALSE)
}

test_that("'>' in legend.labs with risk.table renders (was gridtext blockquote error) (#532)", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  p <- suppressWarnings(ggsurvplot(
    fit, data = lung,
    legend.labs = c("≤ 1 Risk factor", "> 1 Risk factor"),
    risk.table = TRUE))
  expect_true(drew_ok(p))
  ylabs <- ggplot2::ggplot_build(p$table)$layout$panel_params[[1]]$y$get_labels()
  if (survminer:::.ggplot2_ge_4()) {
    # ggplot2 >= 4.0: labels use element_text() (coloured per strata via a colour
    # vector), so the ">" is shown literally with no HTML escaping (#557).
    expect_true(any(grepl("> 1 Risk factor", ylabs, fixed = TRUE)))
  } else {
    # ggplot2 < 4.0: labels use element_markdown(), so ">" is HTML-escaped and
    # gridtext decodes "&gt;" back to a literal ">" at render time (#532).
    expect_true(any(grepl("&gt;", ylabs, fixed = TRUE)))
    expect_false(any(grepl(">", ylabs, fixed = TRUE)))
  }
})

test_that("'>=' / '<' / '&' in labels with risk.table render (#532)", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  for (labs in list(c(">= median", "< median"), c("A & B", "C <d>"))) {
    p <- suppressWarnings(ggsurvplot(fit, data = lung, legend.labs = labs,
                                     risk.table = TRUE))
    expect_true(drew_ok(p))
  }
})

test_that(".escape_markdown escapes &, <, > (and only those), ampersand first", {
  expect_equal(survminer:::.escape_markdown("> 1"), "&gt; 1")
  expect_equal(survminer:::.escape_markdown("a < b & c > d"),
               "a &lt; b &amp; c &gt; d")
  # no double-escaping of the ampersands we introduce
  expect_equal(survminer:::.escape_markdown("&"), "&amp;")
  # ordinary text is untouched (no-op)
  expect_equal(survminer:::.escape_markdown("sex=1"), "sex=1")
})

test_that("no-regression: plain risk.table with y.text.col = FALSE is not escaped", {
  # When the y labels use plain element_text (not markdown) escaping must NOT be
  # applied, otherwise a "&gt;" would show literally. This path renders fine and
  # keeps the raw label.
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  p <- suppressWarnings(ggsurvplot(
    fit, data = lung, legend.labs = c("<= 1", "> 1"),
    risk.table = TRUE, risk.table.y.text.col = FALSE))
  expect_true(drew_ok(p))
})
