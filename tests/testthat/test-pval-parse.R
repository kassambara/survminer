context("pval.parse renders plotmath p-values (#605, #679)")

# #605/#679: a custom p-value string was always drawn literally, so italic /
# superscript p-values (e.g. "italic(P)==1.4~x~10^-6") could not be shown.
# The new pval.parse argument passes parse = TRUE to the p-value annotate()
# calls. Default FALSE keeps the literal behaviour (byte-identical).
library(survival)

fit <- survfit(Surv(time, status) ~ sex, data = lung)

pval_layer_parse <- function(p) {
  # find the annotate("text") layer carrying the p-value label and read its parse flag
  for (ly in p$plot$layers) {
    if (inherits(ly$geom, "GeomText") && !is.null(ly$aes_params$label) &&
        grepl("P|p =|p <", ly$aes_params$label))
      return(isTRUE(ly$geom_params$parse))
  }
  NA
}

test_that("no-regression: default pval is drawn literally, unchanged (#605)", {
  p_no <- ggsurvplot(fit, data = lung, pval = TRUE)
  p_ex <- ggsurvplot(fit, data = lung, pval = TRUE, pval.parse = FALSE)
  # default text label unchanged and not parsed
  expect_false(isTRUE(pval_layer_parse(p_no)))
  expect_equal(ggplot2::ggplot_build(p_no$plot)$data,
               ggplot2::ggplot_build(p_ex$plot)$data)
})

test_that("pval.parse = TRUE marks the p-value layer for plotmath parsing (#605, #679)", {
  p <- ggsurvplot(fit, data = lung, pval = "italic(P)==1.4~x~10^-6",
                  pval.parse = TRUE)
  expect_true(isTRUE(pval_layer_parse(p)))
  # and it renders (plotmath is valid) without error
  expect_error(ggplot2::ggplotGrob(p$plot), NA)
})

test_that("a custom plotmath string without parse still renders (no error) (#679)", {
  p <- ggsurvplot(fit, data = lung, pval = "p = 0.001")
  expect_error(ggplot2::ggplotGrob(p$plot), NA)
})

test_that("pval.parse never parses the (generated) method label -> no crash (#605)", {
  # The method name is generated text (e.g. "modified Peto-Peto", "Log-rank,
  # tft"), NOT plotmath; parsing it would crash. pval.parse must only affect
  # the p-value text. Use pval = TRUE (auto) with pval.method = TRUE so a real,
  # non-plotmath method label ("modified Peto-Peto") is actually present -- a
  # custom character pval forces method = "" and would make this test vacuous.
  # This combo crashed under the prior code (method parsed as plotmath).
  find_method_layer <- function(p) {
    for (ly in p$plot$layers)
      if (inherits(ly$geom, "GeomText") && !is.null(ly$aes_params$label) &&
          grepl("Peto|Log-rank|rank", ly$aes_params$label, ignore.case = TRUE))
        return(ly)
    NULL
  }
  p <- ggsurvplot(fit, data = lung, pval = TRUE, pval.parse = TRUE,
                  pval.method = TRUE, log.rank.weights = "S2")
  expect_error(ggplot2::ggplotGrob(p$plot), NA)
  ml <- find_method_layer(p)
  expect_false(is.null(ml))                       # the method label is really there
  expect_match(ml$aes_params$label, "Peto")       # "modified Peto-Peto"
  expect_false(isTRUE(ml$geom_params$parse))      # and it is NOT parsed

  # weighted test + trend appends ", tft" -> also non-plotmath; must still render
  p2 <- ggsurvplot(fit, data = lung, pval = TRUE, pval.parse = TRUE,
                   pval.method = TRUE, log.rank.weights = "n", test.for.trend = TRUE)
  expect_error(ggplot2::ggplotGrob(p2$plot), NA)
})
