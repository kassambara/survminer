# ggforest(): colour the HR points/intervals via `palette`, and per-variable
# reference labels via a named `refLabel`. Defaults are unchanged.

library(survival)

# text labels drawn in a ggforest (grob-forced so annotations are materialised)
.forest_labels <- function(p) {
  gt <- grid::grid.force(ggplot2::ggplotGrob(p))
  labs <- character(0)
  walk <- function(gr) {
    if (inherits(gr, "gTree") && !is.null(gr$children))
      for (nm in names(gr$children)) walk(gr$children[[nm]])
    if (!is.null(gr$label)) labs <<- c(labs, as.character(gr$label))
  }
  walk(gt); unique(labs[nzchar(labs)])
}
# colours of the drawn HR points (geom_point -> a points grob with gp$col)
.forest_point_cols <- function(p) {
  gt <- grid::grid.force(ggplot2::ggplotGrob(p))
  cols <- character(0)
  walk <- function(gr) {
    if (inherits(gr, "gTree") && !is.null(gr$children))
      for (nm in names(gr$children)) walk(gr$children[[nm]])
    if (inherits(gr, "points") && !is.null(gr$gp$col))
      cols <<- c(cols, gr$gp$col)
  }
  walk(gt); unique(cols)
}

.m <- function() {
  d <- within(colon, { sex <- factor(sex, labels = c("female", "male")); rx <- factor(rx) })
  list(fit = coxph(Surv(time, status) ~ sex + rx + age + nodes, data = d), data = d)
}

.norm_col <- function(x) toupper(sub("^#", "#", x))

test_that("default palette (NULL) draws black points; a single colour recolours them", {
  mm <- .m()
  black <- .norm_col(.forest_point_cols(ggforest(mm$fit, data = mm$data)))
  expect_true("#000000" %in% black)              # default: black points
  accent <- .norm_col(.forest_point_cols(
    ggforest(mm$fit, data = mm$data, palette = "#0073C2")))
  expect_true("#0073C2" %in% accent)             # recoloured
  expect_false("#000000" %in% accent)            # no black point remains
})

test_that("a palette colours points by variable (one colour per variable)", {
  mm <- .m()
  cols <- .forest_point_cols(ggforest(mm$fit, data = mm$data, palette = "npg"))
  # 4 variables (sex, rx, age, nodes) -> at least two distinct point colours
  expect_gte(length(unique(cols)), 2L)
  expect_false("#000000" %in% .norm_col(cols))
})

test_that("a named refLabel sets per-variable reference labels; others keep default", {
  mm <- .m()
  labs <- .forest_labels(ggforest(mm$fit, data = mm$data,
                                  refLabel = c(sex = "female (ref)")))
  expect_true("female (ref)" %in% labs)   # named variable relabelled
  expect_true("reference" %in% labs)      # rx (unnamed) keeps the default
})

test_that("a custom scalar refLabel is drawn (and no longer mis-handled)", {
  mm <- .m()
  labs <- .forest_labels(ggforest(mm$fit, data = mm$data, refLabel = "Ref"))
  expect_true("Ref" %in% labs)
  expect_false("reference" %in% labs)
})

test_that("palette and refLabel do not error with ref.display = FALSE / global.stats = FALSE", {
  mm <- .m()
  expect_error(ggforest(mm$fit, data = mm$data, palette = "jco",
                        ref.display = FALSE, global.stats = FALSE), NA)
})
