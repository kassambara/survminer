# ggcompetingrisks(risk.table = TRUE): number-at-risk table under CIF curves (#838).

skip_if_not_installed("tidycmprsk")
skip_if_not_installed("survival")

.tfit <- function() {
  data("trial", package = "tidycmprsk", envir = environment())
  tidycmprsk::cuminc(survival::Surv(ttdeath, death_cr) ~ trt, data = trial)
}

test_that("risk.table = FALSE returns the unchanged bare ggplot", {
  p <- ggcompetingrisks(.tfit())
  expect_s3_class(p, "ggplot")
  expect_null(p$table)
  expect_false(inherits(p, "ggcompetingrisks"))
})

test_that("risk.table = TRUE returns an aligned compound object", {
  p <- ggcompetingrisks(.tfit(), risk.table = TRUE)
  expect_s3_class(p, "ggcompetingrisks")
  expect_s3_class(p, "ggsurvplot")           # reuses the aligned-render path
  expect_s3_class(p$plot, "ggplot")
  expect_s3_class(p$table, "ggplot")
  # cr.data / grays.test attrs survive on the compound and its $plot
  expect_false(is.null(attr(p, "cr.data")))
  expect_equal(attr(p, "cr.data"), attr(p$plot, "cr.data"))
  expect_false(is.null(attr(p, "grays.test")))
  # it draws without error (and ggsave works via grid.draw method)
  grDevices::pdf(NULL); on.exit(grDevices::dev.off())
  expect_error(print(p), NA)
})

test_that("the at-risk counts match the Kaplan-Meier convention (summary.survfit)", {
  fit <- .tfit()
  p <- ggcompetingrisks(fit, risk.table = TRUE, break.time.by = 6)
  td <- p$table$data                          # ggsurvtable's survsummary frame
  # independent ground truth: event-free survfit, summary at the same breaks
  d <- fit$data
  d$ev <- as.integer(as.character(d$death_cr) %in% names(fit$failcode))
  sf <- survival::survfit(survival::Surv(ttdeath, ev) ~ trt, data = d)
  ref <- summary(sf, times = c(0, 6, 12, 18, 24), extend = TRUE)
  refdf <- data.frame(strata = as.character(ref$strata), time = ref$time,
                      n.risk = ref$n.risk)
  tddf <- data.frame(strata = as.character(td$strata), time = td$time,
                     n.risk = td$n.risk)
  m <- merge(refdf, tddf, by = c("strata", "time"))
  expect_equal(m$n.risk.x, m$n.risk.y)
  # beyond the last observed time the count is 0, not a carried-forward positive
  expect_true(all(m$n.risk.y[m$time > max(fit$data$ttdeath)] == 0))
})

test_that("the table shares the curve's break times (single source)", {
  p <- ggcompetingrisks(.tfit(), risk.table = TRUE, break.time.by = 6)
  curve.brk <- ggplot2::ggplot_build(p$plot)$layout$panel_params[[1]]$x$breaks
  curve.brk <- curve.brk[!is.na(curve.brk)]
  table.times <- sort(unique(p$table$data$time))
  expect_true(all(c(0, 6, 12, 18, 24) %in% table.times))
  expect_setequal(intersect(curve.brk, c(0, 6, 12, 18, 24)), c(0, 6, 12, 18, 24))
})

test_that("risk.table degrades honestly for non-tidycmprsk input", {
  skip_if_not_installed("cmprsk")
  set.seed(2)
  ss <- rexp(100); gg <- factor(sample(1:2, 100, replace = TRUE))
  cc <- factor(sample(0:2, 100, replace = TRUE), 0:2, c("none", "death", "prog"))
  cf <- cmprsk::cuminc(ss, cc, gg)
  expect_message(p <- ggcompetingrisks(cf, risk.table = TRUE),
                 "only for a tidycmprsk")
  expect_null(p$table)
  expect_false(inherits(p, "ggcompetingrisks"))
})

test_that("explicit multiple_panels = TRUE with risk.table informs and switches", {
  expect_message(ggcompetingrisks(.tfit(), risk.table = TRUE, multiple_panels = TRUE),
                 "single-panel layout")
})

test_that("a character risk.table selects the table content", {
  p <- ggcompetingrisks(.tfit(), risk.table = "percentage")
  expect_s3_class(p, "ggcompetingrisks")
  expect_false(is.null(p$table))
})
