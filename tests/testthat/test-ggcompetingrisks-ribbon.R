context("ggcompetingrisks ribbon grouping")

# Regression test for #490: in a SINGLE panel with conf.int = TRUE, the
# confidence ribbon mapped only fill = event, so its implicit grouping ignored
# `group` and the bands jumped between groups of the same event ("messed up
# plot"). The ribbon is now grouped by interaction(event, group), giving one
# band per event x group.
test_that("single-panel ggcompetingrisks conf.int groups ribbons by event x group (#490)", {
  skip_if_not_installed("cmprsk")
  ov <- survival::ovarian
  ci <- cmprsk::cuminc(ov$futime, ov$fustat, cencode = 2, group = ov$resid.ds)

  p <- ggcompetingrisks(ci, multiple_panels = FALSE, conf.int = TRUE)
  b <- ggplot2::ggplot_build(p)
  ribbon_idx <- which(vapply(p$layers,
                             function(l) inherits(l$geom, "GeomRibbon"),
                             logical(1)))
  rb <- b$data[[ribbon_idx]]

  # 2 events x 2 groups => 4 distinct ribbons (was 2, event-only, which mixed
  # the bands across groups).
  expect_equal(length(unique(rb$group)), 4L)
})
