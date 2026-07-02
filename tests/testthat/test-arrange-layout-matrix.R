context("arrange_ggsurvplots layout_matrix")

# Regression test for #300: arrange_ggsurvplots() gains a layout_matrix argument
# (forwarded to gridExtra::marrangeGrob) so plots can be ordered by row, as is
# usual in journals. The default (layout_matrix = NULL) is unchanged.
library(survival)

make_splots <- function(n = 6) {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  lapply(seq_len(n), function(i) ggsurvplot(fit, data = lung, title = paste("Plot", i)))
}

# top,left cell position of each arranged plot on the (single) page
cell_pos <- function(al) al[[1]]$layout[, c("t", "l")]

test_that("no-regression: default arrange_ggsurvplots() fills column-wise (#300)", {
  al <- arrange_ggsurvplots(make_splots(6), print = FALSE, ncol = 2, nrow = 3)
  expect_s3_class(al, "arrangelist")
  pos <- cell_pos(al)
  # grobs stored 1..6, placed down columns: rows 1,2,3 then 1,2,3
  expect_equal(pos$t, c(1, 2, 3, 1, 2, 3))
  expect_equal(pos$l, c(1, 1, 1, 2, 2, 2))
})

test_that("layout_matrix orders plots by row (#300)", {
  al <- arrange_ggsurvplots(make_splots(6), print = FALSE, ncol = 2, nrow = 3,
                            layout_matrix = matrix(1:6, nrow = 3, byrow = TRUE))
  expect_s3_class(al, "arrangelist")
  pos <- cell_pos(al)
  # row-wise fill: (1,1),(1,2),(2,1),(2,2),(3,1),(3,2)
  expect_equal(pos$t, c(1, 1, 2, 2, 3, 3))
  expect_equal(pos$l, c(1, 2, 1, 2, 1, 2))
})
