context("extract_ggplot_colors")

# Regression test for #397 / #519 / #595 / #691: a palette containing duplicate
# colours (or a default palette that ggplot2 rendered with a repeated hue) made
# .extract_ggplot_colors() call unique() on the colour column, returning fewer
# colours than groups. Naming that shorter vector with the group levels then
# failed with "'names' attribute [n] must be the same length as the vector [m]",
# which surfaced whenever risk.table / ncensor.plot were requested.
test_that(".extract_ggplot_colors keeps one colour per group (#397, #519, #595, #691)", {
  library(survival)
  set.seed(1)
  d <- lung
  d$grp <- factor(sample(c("a", "b", "c", "d"), nrow(d), replace = TRUE))
  fit <- survfit(Surv(time, status) ~ grp, data = d)

  # a palette with a duplicated colour (groups a and b share "red")
  pal <- c("red", "red", "blue", "green")
  p <- ggsurvplot(fit, data = d, palette = pal)$plot
  cols <- .extract_ggplot_colors(p, grp.levels = c("a", "b", "c", "d"))
  expect_length(cols, 4)
  expect_equal(unname(cols), pal)

  # no-regression: a palette of distinct colours still yields one colour per group
  p2 <- ggsurvplot(fit, data = d)$plot
  cols2 <- .extract_ggplot_colors(p2, grp.levels = c("a", "b", "c", "d"))
  expect_length(cols2, 4)

  # end-to-end: duplicate palette + risk.table / ncensor.plot no longer error
  expect_error(ggsurvplot(fit, data = d, palette = pal, risk.table = TRUE), NA)
  expect_error(ggsurvplot(fit, data = d, palette = pal, ncensor.plot = TRUE), NA)
})
