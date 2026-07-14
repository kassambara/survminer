# ggforest(columns=): choose which statistic columns show, reflowing the rest.
# The default (all four) is unchanged.

library(survival)

.flabels <- function(p) {
  gt <- grid::grid.force(ggplot2::ggplotGrob(p)); L <- character()
  w <- function(g) {
    if (inherits(g, "gTree") && !is.null(g$children))
      for (n in names(g$children)) w(g$children[[n]])
    if (!is.null(g$label)) L <<- c(L, as.character(g$label))
  }
  w(gt); L[nzchar(L)]
}

.m <- function() coxph(Surv(time, status) ~ age + factor(ph.ecog) + sex, data = lung)

test_that("the default columns argument reproduces the default output", {
  m <- .m()
  expect_setequal(.flabels(ggforest(m, data = lung)),
                  .flabels(ggforest(m, data = lung, columns = c("N", "hr", "ci", "p"))))
})

test_that("dropping a column removes exactly its labels", {
  m <- .m()
  all  <- .flabels(ggforest(m, data = lung))
  noN  <- .flabels(ggforest(m, data = lung, columns = c("hr", "ci", "p")))
  noP  <- .flabels(ggforest(m, data = lung, columns = c("N", "hr", "ci")))
  # N labels look like "(N=138)"
  expect_true(any(grepl("^\\(N=", all)))
  expect_false(any(grepl("^\\(N=", noN)))
  # p/stars labels contain significance stars or "<0.001"
  expect_true(any(grepl("\\*|<0.001", all)))
  expect_false(any(grepl("\\*|<0.001", noP)))
})

test_that("minimal columns keep the HR/CI and the reference indicator", {
  m <- .m()
  labs <- .flabels(ggforest(m, data = lung, columns = c("hr", "ci")))
  expect_false(any(grepl("^\\(N=", labs)))     # no N
  expect_true("reference" %in% labs)           # ref label kept (lives in hr column)
  expect_true(any(grepl("^\\(", labs)))        # CI strings like "(1.02 - 2.23)"
})

test_that("columns is case-insensitive and validated", {
  m <- .m()
  expect_error(ggforest(m, data = lung, columns = c("HR", "P")), NA)  # OK
  expect_error(ggforest(m, data = lung, columns = c("hr", "xyz")),
               "Unknown `columns`")
  expect_error(ggforest(m, data = lung, columns = character(0)), NA)  # legal minimal
  # NULL means the default (all four), not "none"
  expect_setequal(.flabels(ggforest(m, data = lung, columns = NULL)),
                  .flabels(ggforest(m, data = lung)))
})
