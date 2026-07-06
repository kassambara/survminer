context("ggforest var.labels custom variable names (#405)")

# #405: ggforest() drew the raw model term names as the variable labels with no
# way to rename them. A named `var.labels` vector now remaps them; NULL (default)
# is unchanged, unmatched terms keep their original name.
# ggforest() returns as_ggplot(gtable), so labels are baked into text grobs;
# collect them by walking the grob tree.
library(survival)

fit <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data = lung)

grob_labels <- function(g) {
  gt <- ggplot2::ggplotGrob(g)
  out <- character(0)
  walk <- function(x) {
    if (!is.null(x$label)) out[[length(out) + 1L]] <<- paste(x$label, collapse = "|")
    if (!is.null(x$children)) for (nm in names(x$children)) walk(x$children[[nm]])
    if (!is.null(x$grobs))    for (k in seq_along(x$grobs)) walk(x$grobs[[k]])
  }
  walk(gt)
  paste(out, collapse = " || ")
}

test_that("no-regression: default (var.labels = NULL) is byte-identical (#405)", {
  expect_equal(ggplot2::ggplot_build(ggforest(fit, data = lung))$data,
               ggplot2::ggplot_build(ggforest(fit, data = lung, var.labels = NULL))$data)
})

test_that("var.labels remaps the drawn variable names (#405)", {
  g <- ggforest(fit, data = lung,
                var.labels = c(age = "Age (years)", sex = "Sex", ph.ecog = "ECOG PS"))
  labs <- grob_labels(g)
  expect_true(grepl("Age (years)", labs, fixed = TRUE))
  expect_true(grepl("ECOG PS", labs, fixed = TRUE))
  # raw names no longer drawn as standalone variable labels
  expect_false(grepl("(^|\\|)age(\\||$)", labs))
  expect_false(grepl("(^|\\|)ph.ecog(\\||$)", labs))
  expect_error(ggplot2::ggplotGrob(g), NA)
})

test_that("unmatched terms keep their original name; partial map works (#405)", {
  g <- ggforest(fit, data = lung, var.labels = c(age = "Age (yrs)"))
  labs <- grob_labels(g)
  expect_true(grepl("Age (yrs)", labs, fixed = TRUE))   # remapped
  expect_true(grepl("sex", labs))                        # untouched
  expect_true(grepl("ph.ecog", labs, fixed = TRUE))      # untouched
})

test_that("var.labels must be a named vector (#405)", {
  expect_error(ggforest(fit, data = lung, var.labels = c("Age", "Sex")),
               "named")
})

test_that("a named factor var.labels is coerced to its labels, not codes (#405)", {
  vl <- factor(c("Age (yrs)", "Sex"))          # named factor -> would inject 1/2
  names(vl) <- c("age", "sex")
  g <- ggforest(fit, data = lung, var.labels = vl)
  labs <- grob_labels(g)
  # labels present (not the integer codes 1/2 a raw factor->character coercion
  # would have injected in their place)
  expect_true(grepl("Age (yrs)", labs, fixed = TRUE))
  expect_true(grepl("Sex", labs, fixed = TRUE))
})

test_that("an NA label leaves that variable name unchanged, not dropped (#405)", {
  g <- ggforest(fit, data = lung, var.labels = c(age = NA, sex = "Sex"))
  labs <- grob_labels(g)
  expect_true(grepl("age", labs))     # age kept (NA label ignored)
  expect_true(grepl("Sex", labs, fixed = TRUE))
})
