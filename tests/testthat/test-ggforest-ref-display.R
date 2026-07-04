context("ggforest ref.display argument")

# Regression test for #563: ggforest() gains a `ref.display` argument. When FALSE,
# the reference-level rows of factor variables (baselines, which have no hazard
# ratio) are dropped from both the plot and the table, keeping only the compared
# levels. Default TRUE shows every level (unchanged). The reference rows are
# identified precisely by an NA estimate, so genuine estimates are never dropped.
library(survival)

# Collect all text drawn in a rendered ggforest (labels live in text grobs).
forest_text <- function(p) {
  g <- ggplot2::ggplotGrob(p)
  labs <- character(0)
  collect <- function(gr) {
    if (inherits(gr, "text")) labs <<- c(labs, gr$label)
    if (!is.null(gr$children)) lapply(gr$children, collect)
    if (!is.null(gr$grobs)) lapply(gr$grobs, collect)
    invisible(NULL)
  }
  collect(g)
  labs[!is.na(labs)]
}

fit <- coxph(Surv(time, status) ~ sex + factor(ph.ecog) + age, data = lung)

test_that("no-regression: ref.display = TRUE (default) shows reference rows (#563)", {
  p <- ggforest(fit, data = lung)
  txt <- forest_text(p)
  # the factor(ph.ecog) baseline (level 0) is shown as a 'reference' row
  expect_true(any(grepl("reference", txt)))
})

test_that("ref.display = FALSE drops the reference rows (#563)", {
  p <- ggforest(fit, data = lung, ref.display = FALSE)
  txt <- forest_text(p)
  expect_false(any(grepl("reference", txt)))
  # the compared factor levels are still present
  expect_true(any(grepl("2.47", txt)))   # ph.ecog level 2 hazard ratio
  # and the plot still draws
  expect_error(ggplot2::ggplotGrob(p), NA)
})

test_that("ref.display = FALSE does not drop continuous-only estimates (#563)", {
  # a model with no factor (no reference rows): output should be the same with
  # ref.display TRUE or FALSE (nothing to drop).
  fit_num <- coxph(Surv(time, status) ~ age + wt.loss, data = lung)
  p_on  <- ggforest(fit_num, data = lung)
  p_off <- ggforest(fit_num, data = lung, ref.display = FALSE)
  expect_setequal(forest_text(p_on), forest_text(p_off))
})

test_that("ref.display = FALSE also drops aliased/non-estimable (NA-estimate) terms (#563)", {
  # Documented behavior: the drop targets rows with no hazard ratio (NA estimate),
  # which is a superset of factor baselines -- it also includes aliased/collinear
  # terms that coxph sets to NA. These are already shown as "reference" in the
  # default plot (they have no plottable HR), so dropping them is self-consistent.
  d <- lung
  d$age2 <- 2 * d$age                       # perfectly collinear -> coxph aliases it
  fit_alias <- suppressWarnings(coxph(Surv(time, status) ~ age + age2, data = d))
  # sanity: age2 is aliased to an NA estimate
  expect_true(is.na(broom::tidy(fit_alias)$estimate[broom::tidy(fit_alias)$term == "age2"]))
  # default shows it (as a "reference"-labelled row); ref.display = FALSE drops it
  expect_true(any(grepl("reference", forest_text(suppressWarnings(ggforest(fit_alias, data = d))))))
  expect_false(any(grepl("reference",
                         forest_text(suppressWarnings(ggforest(fit_alias, data = d, ref.display = FALSE))))))
})

test_that("no-regression: ref.display = FALSE keeps a non-reference row whose CI is wide (#563)", {
  # ph.ecog level 3 (N=1) has a very wide but finite interval -- a real estimate,
  # not a reference -- so it must survive the drop.
  p <- ggforest(fit, data = lung, ref.display = FALSE)
  txt <- forest_text(p)
  expect_true(any(grepl("7.06", txt)))   # ph.ecog level 3 hazard ratio
})
