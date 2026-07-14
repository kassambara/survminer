context("ggsurvplot legend.labs plotmath (#350)")

library(survival)
fit <- survfit(Surv(time, status) ~ sex, data = lung)

test_that(".is_plotmath distinguishes plotmath from plain labels", {
  expect_false(survminer:::.is_plotmath(NULL))
  expect_false(survminer:::.is_plotmath(c("Male", "Female")))
  expect_false(survminer:::.is_plotmath(factor(c("a", "b"))))
  expect_false(survminer:::.is_plotmath(1:2))
  expect_true(survminer:::.is_plotmath(c(expression(x^2), expression(y))))
  expect_true(survminer:::.is_plotmath(expression(x^2)))
  expect_true(survminer:::.is_plotmath(list(quote(x^2), "plain")))
})

test_that(".plotmath_to_char gives distinct character labels", {
  ch <- survminer:::.plotmath_to_char(c(expression(x^2), expression(alpha[i])))
  expect_type(ch, "character")
  expect_length(ch, 2)
  expect_true(all(nzchar(ch)))
  expect_equal(length(unique(ch)), 2)
})

test_that("character legend.labs does NOT trigger the plotmath path (no regression)", {
  p <- ggsurvplot(fit, data = lung, legend.labs = c("Male", "Female"))
  expect_null(attr(p$plot, "parameters")$legend.labs.math)
  # default (no legend.labs) is likewise untouched
  p0 <- ggsurvplot(fit, data = lung)
  expect_null(attr(p0$plot, "parameters")$legend.labs.math)
})

test_that("expression legend.labs renders as plotmath in the legend", {
  labs <- c(expression(x^2 ~ y^2), expression(alpha[i] >= beta))
  p <- ggsurvplot(fit, data = lung, palette = "jco", legend.labs = labs)
  # the math is stored on the plot parameters
  expect_false(is.null(attr(p$plot, "parameters")$legend.labs.math))
  # the colour scale carries expression labels (renders as math)
  expect_true(is.expression(p$plot$scales$get_scales("colour")$labels))
})

test_that("expression legend.labs preserves the palette exactly", {
  labs <- c(expression(x^2), expression(y))
  base <- ggsurvplot(fit, data = lung, palette = "jco",
                     legend.labs = c("A", "B"))
  math <- ggsurvplot(fit, data = lung, palette = "jco", legend.labs = labs)
  cols_base <- survminer:::.extract_ggplot_colors(base$plot, grp.levels = c("A", "B"))
  cols_math <- survminer:::.extract_ggplot_colors(math$plot, grp.levels = c("A", "B"))
  expect_equal(unname(cols_base), unname(cols_math))
})

test_that("expression legend.labs still enforces the length check", {
  expect_error(
    ggsurvplot(fit, data = lung, legend.labs = c(expression(only_one))),
    "length of legend.labs"
  )
})

test_that("expression legend.labs also renders math on the risk table", {
  labs <- c(expression(x^2), expression(beta[i]))
  p <- ggsurvplot(fit, data = lung, risk.table = TRUE, legend.labs = labs)
  expect_false(is.null(p$table))
  expect_true(is.expression(p$table$scales$get_scales("y")$labels))
})

test_that("a list() of expressions renders math on both legend and table (#350)", {
  # `list(...)` must be flattened; as.expression(list_of_expr) would nest and
  # render nothing on the table.
  labs <- list(expression(gamma[0]), expression(delta^2))
  p <- ggsurvplot(fit, data = lung, risk.table = TRUE, legend.labs = labs)
  ycols <- p$table$scales$get_scales("y")$labels
  expect_true(is.expression(ycols))
  expect_length(ycols, 2)
  # not nested: each element is a language object, not an expression-in-expression
  expect_false(is.expression(ycols[[1]]))
})

test_that("plotmath does not override a hidden table y-axis (risk.table.y.text = FALSE)", {
  labs <- c(expression(x^2), expression(y))
  p <- ggsurvplot(fit, data = lung, risk.table = TRUE,
                  risk.table.y.text = FALSE, legend.labs = labs)
  # the hidden-label dash must be preserved, not re-exposed as the expressions
  ylabs <- p$table$scales$get_scales("y")$labels
  expect_false(is.expression(ylabs))
})
