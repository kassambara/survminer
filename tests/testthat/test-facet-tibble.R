context("ggsurvplot_facet with a tibble")

# Regression test for #591: ggsurvplot_facet() with panel.labs errored with
# "cannot xtfrm data frames" when the data was a tibble, because the panel-label
# code did as.factor(data[, var]) and tibble[, var] returns a one-column tibble
# (not a vector). The data is now coerced to a plain data.frame at entry.
library(survival)

test_that("ggsurvplot_facet() + panel.labs works with a tibble (#591)", {
  d   <- tibble::as_tibble(colon)
  fit <- survfit(Surv(time, status) ~ rx, data = d)
  expect_error(
    ggsurvplot_facet(fit, data = d, facet.by = "sex",
                     panel.labs = list(sex = c("Male", "Female"))),
    NA
  )
})

test_that("no-regression: data.frame input with panel.labs still works (#591)", {
  fit <- survfit(Surv(time, status) ~ rx, data = colon)
  expect_error(
    ggsurvplot_facet(fit, data = colon, facet.by = "sex",
                     panel.labs = list(sex = c("Male", "Female"))),
    NA
  )
  # plain facet (no panel.labs) unaffected
  expect_error(ggsurvplot_facet(fit, data = colon, facet.by = "sex"), NA)
})
