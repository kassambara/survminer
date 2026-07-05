context("customize_labels")

suppressPackageStartupMessages({
  library(ggplot2)
  library(survival)
})

test_that("customize_labels() sets fonts on a single ggplot", {
  skip_if_not_installed("ggtext")
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  q <- customize_labels(
    p,
    font.title = c(16, "bold", "darkblue"),
    font.x = c(14, "bold.italic", "red"),
    font.xtickslab = c(12, "plain", "darkgreen")
  )
  expect_true(ggplot2::is_ggplot(q))
  # markdown elements are used, so styling survives on markdown-capable slots
  expect_true(inherits(q$theme[["plot.title"]], "element_markdown"))
  expect_equal(q$theme[["plot.title"]]$size, 16)
  expect_equal(q$theme[["axis.title.x"]]$colour, "red")
  expect_equal(q$theme[["axis.text.x"]]$size, 12)
  # untouched slots stay NULL
  expect_null(q$theme[["plot.caption"]])
})

test_that("customize_labels() handles the ggsurvplot list and preserves markdown", {
  skip_if_not_installed("ggtext")
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  ggsurv <- ggsurvplot(fit, data = lung, risk.table = TRUE)
  out <- customize_labels(
    ggsurv,
    font.title = c(16, "bold", "darkblue"),
    font.y = c(14, "bold.italic", "darkred")
  )
  # structure preserved
  expect_s3_class(out, "ggsurvplot")
  expect_true(ggplot2::is_ggplot(out$plot))
  expect_true(ggplot2::is_ggplot(out$table))
  # font applied on both curve and table titles
  expect_equal(out$plot$theme[["plot.title"]]$size, 16)
  expect_equal(out$table$theme[["plot.title"]]$size, 16)
  # both curve and table render without error
  expect_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(out$plot)), NA)
  expect_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(out$table)), NA)
})

test_that("customize_labels() leaves NULL fonts unchanged and errors on bad input", {
  skip_if_not_installed("ggtext")
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  q <- customize_labels(p) # all fonts NULL -> no theme change
  expect_null(q$theme[["plot.title"]])
  expect_null(q$theme[["axis.title.x"]])
  expect_error(customize_labels(42), "Can't handle an object of class")
})
