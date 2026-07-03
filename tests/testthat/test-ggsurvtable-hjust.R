context("ggsurvtable hjust")

# Regression test for #629: ggsurvtable() gains an hjust argument so the table
# text can be justified (e.g. left-aligned). Default is 0.5 (centered),
# reproducing the previous output.
library(survival)

table_hjust <- function(p) {
  b  <- ggplot2::ggplot_build(p)
  ly <- which(vapply(b$plot$layers,
                     function(l) inherits(l$geom, "GeomText"), logical(1)))
  b$plot$layers[[ly[1]]]$aes_params$hjust
}

test_that("hjust is applied to the table text (#629)", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  p0  <- ggsurvtable(fit, data = lung, survtable = "risk.table")
  pL  <- ggsurvtable(fit, data = lung, survtable = "risk.table", hjust = 0)
  expect_equal(table_hjust(p0), 0.5)   # default centered (unchanged)
  expect_equal(table_hjust(pL), 0)     # left-aligned
  expect_error(ggplot2::ggplotGrob(pL), NA)
})

test_that("no-regression: default table renders unchanged (#629)", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  expect_error(ggsurvtable(fit, data = lung, survtable = "cumevents"), NA)
})
