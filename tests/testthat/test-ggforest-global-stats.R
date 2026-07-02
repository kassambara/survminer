context("ggforest global.stats")

# Regression test for #392: ggforest() always drew the global-statistics caption
# ("# Events; Global p-value; AIC; Concordance Index") at the bottom, with no way
# to omit it (e.g. when arranging several forest plots in a panel). A global.stats
# argument now controls it; the default (TRUE) keeps the caption unchanged.
library(survival)

# ggforest() returns ggpubr::as_ggplot(gtable); the forest itself lives in a
# single GeomCustomAnn layer whose grob is the assembled gtable. Search that grob
# tree for the caption text.
caption_present <- function(g) {
  grob <- g$layers[[1]]$geom_params$grob
  labels <- character(0)
  rec <- function(x) {
    if (!is.null(x$label)) labels <<- c(labels, unlist(x$label))
    if (!is.null(x$grobs)) lapply(x$grobs, rec)
    if (!is.null(x$children)) lapply(x$children, rec)
    invisible(NULL)
  }
  rec(grob)
  any(grepl("# Events", labels))
}

test_that("ggforest() draws the global-statistics caption by default (#392)", {
  cm <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data = lung)
  g <- ggforest(cm, data = lung)
  expect_true(caption_present(g))
  expect_true(caption_present(ggforest(cm, data = lung, global.stats = TRUE)))
})

test_that("ggforest(global.stats = FALSE) omits the global-statistics caption (#392)", {
  cm <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data = lung)
  g <- ggforest(cm, data = lung, global.stats = FALSE)
  expect_false(caption_present(g))
  # the plot still builds without error
  expect_s3_class(g, "ggplot")
})
