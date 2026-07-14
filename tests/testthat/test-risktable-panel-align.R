# Risk-table / curve x-axis alignment: the survival panel and the table panel must
# share an identical left column layout so their x-axes line up. Guards against a
# future ggplot2 layout change silently reintroducing the misalignment class
# (#102/#302/#348/#649/#676).

skip_if_not_installed("survival")

# left width (mm) before the first panel column of a gtable
.left_mm <- function(gr) {
  pc <- min(gr$layout$l[grepl("panel", gr$layout$name)])
  if (pc <= 1) return(0)
  sum(as.numeric(grid::convertWidth(gr$widths[seq_len(pc - 1)], "mm")))
}

test_that(".align_panel_widths equalizes the common left columns and is guarded", {
  library(survival)
  grDevices::pdf(NULL)                 # deterministic device for convertWidth
  on.exit(grDevices::dev.off())
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  p <- ggsurvplot(fit, data = lung, risk.table = TRUE,
                  legend.labs = c("A very long stratum label one",
                                  "A very long stratum label two"))
  cg <- ggplot2::ggplotGrob(p$plot)
  tg <- ggplot2::ggplotGrob(p$table)
  aligned <- survminer:::.align_panel_widths(list(cg, tg))
  ncmin <- min(length(cg$widths), length(tg$widths))
  w1 <- as.numeric(grid::convertWidth(aligned[[1]]$widths[seq_len(ncmin)], "mm"))
  w2 <- as.numeric(grid::convertWidth(aligned[[2]]$widths[seq_len(ncmin)], "mm"))
  expect_equal(w1, w2, tolerance = 1e-6)

  # guards: NULL entries dropped; fewer than two grobs returned unchanged
  expect_length(survminer:::.align_panel_widths(list(cg, NULL)), 1L)
  expect_length(survminer:::.align_panel_widths(list()), 0L)
})

test_that("built curve and risk table share the same panel left offset", {
  library(survival)
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  grDevices::pdf(NULL)            # a device for convertWidth
  on.exit(grDevices::dev.off())

  cases <- list(
    default   = list(risk.table = TRUE),
    longlabel = list(risk.table = TRUE,
                     legend.labs = c("Male treatment arm (control)",
                                     "Female treatment arm (experimental)"),
                     xlim = c(0, 800)),
    xscale    = list(risk.table = TRUE, xscale = 30.44, break.time.by = 182.5),
    threetab  = list(risk.table = TRUE, cumevents = TRUE, cumcensor = TRUE)
  )
  for (nm in names(cases)) {
    p <- do.call(ggsurvplot, c(list(fit, data = lung), cases[[nm]]))
    built <- survminer:::.build_ggsurvplot(p)
    kids <- built$grobs[vapply(built$grobs,
                               function(g) inherits(g, "gtable") &&
                                 any(grepl("panel", g$layout$name)), logical(1))]
    lefts <- vapply(kids, .left_mm, numeric(1))
    expect_gt(length(lefts), 1L)                       # curve + at least one table
    expect_lt(max(lefts) - min(lefts), 0.01)           # panels share the left edge
  }
})
