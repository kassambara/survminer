context("ggforest ggtheme argument (#530)")

# #530: ggforest() returns as_ggplot(gtable), so a theme added to the returned
# object is silently ignored. The new `ggtheme` argument applies a theme to the
# internal plot before it is rasterised. Default NULL is unchanged.
library(survival)

fit <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data = lung)

# largest text-grob fontsize anywhere in the drawn plot
max_text_size <- function(g) {
  gt <- ggplot2::ggplotGrob(g)
  sizes <- numeric(0)
  walk <- function(x) {
    if (inherits(x, "text") && !is.null(x$gp) && !is.null(x$gp$fontsize))
      sizes <<- c(sizes, x$gp$fontsize)
    if (!is.null(x$children)) for (nm in names(x$children)) walk(x$children[[nm]])
    if (!is.null(x$grobs))    for (k in seq_along(x$grobs)) walk(x$grobs[[k]])
  }
  walk(gt)
  max(sizes, na.rm = TRUE)
}

test_that("no-regression: default (ggtheme = NULL) is byte-identical (#530)", {
  expect_equal(ggplot2::ggplot_build(ggforest(fit, data = lung))$data,
               ggplot2::ggplot_build(ggforest(fit, data = lung, ggtheme = NULL))$data)
})

test_that("ggtheme reaches the internal plot text (#530)", {
  base   <- max_text_size(ggforest(fit, data = lung))
  themed <- max_text_size(ggforest(fit, data = lung,
                          ggtheme = ggplot2::theme(text = ggplot2::element_text(size = 40))))
  expect_gt(themed, base)                 # the big text theme actually took effect
})

test_that("ggtheme builds without error and NULL default renders (#530)", {
  expect_error(ggplot2::ggplotGrob(
    ggforest(fit, data = lung,
             ggtheme = ggplot2::theme(plot.background =
               ggplot2::element_rect(fill = "lightyellow")))), NA)
  expect_error(ggplot2::ggplotGrob(ggforest(fit, data = lung)), NA)
})
