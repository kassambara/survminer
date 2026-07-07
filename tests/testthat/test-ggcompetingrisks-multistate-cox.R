context("ggcompetingrisks() clear error for multistate-Cox newdata (#625)")

# #625: survfit(coxph_multistate, newdata = <2+ rows>) returns a 3-D pstate array
# (time x covariate-profile x state) with more columns than states, so
# ggcompetingrisks()'s column renaming produced NA names and the user hit the
# opaque "Names repair functions can't return `NA` values" error. It now fails
# early with an actionable message when the column count doesn't match the
# states. The 2-D survfitms path AND a single-profile prediction (one column per
# state) are unchanged and still render.
library(survival)

draw_ok <- function(p) {
  tmp <- tempfile(fileext = ".pdf"); grDevices::pdf(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)
  print(p); invisible(TRUE)
}

# Build the Therneau competing-risks setup on mgus2 (etime/event are derived,
# not raw columns).
mg <- mgus2
mg$etime <- with(mg, ifelse(pstat == 1, ptime, futime))
mg$event <- factor(with(mg, ifelse(pstat == 1, 1, 2 * death)),
                   0:2, labels = c("censor", "pcm", "death"))

test_that("multi-profile multistate-Cox prediction gives an actionable error, not the vctrs one (#625)", {
  cfit  <- coxph(Surv(etime, event) ~ age + sex + mspike, mg, id = id)
  csurv <- survfit(cfit, newdata = expand.grid(sex = c("F", "M"), age = 60, mspike = 1.2))
  expect_gt(ncol(as.data.frame(csurv$pstate)), length(csurv$states))   # >states columns
  expect_error(ggcompetingrisks(csurv), "several covariate")
  # and not the opaque internal message it used to surface
  msg <- tryCatch(ggcompetingrisks(csurv), error = function(e) conditionMessage(e))
  expect_false(grepl("Names repair", msg))
})

test_that("no-regression: a single-profile multistate-Cox prediction still renders (#625)", {
  cfit   <- coxph(Surv(etime, event) ~ age + sex + mspike, mg, id = id)
  csurv1 <- survfit(cfit, newdata = data.frame(sex = "F", age = 60, mspike = 1.2))
  # 3-D array but the profile axis has extent 1, so as.data.frame() yields
  # exactly one column per state -- the column count matches, guard stays quiet.
  expect_equal(ncol(as.data.frame(csurv1$pstate)), length(csurv1$states))
  expect_error(draw_ok(ggcompetingrisks(csurv1)), NA)
})

test_that("no-regression: 2-D survfitms competing-risks plots still render (#625)", {
  cif1 <- survfit(Surv(etime, event) ~ 1,   data = mg)
  cif2 <- survfit(Surv(etime, event) ~ sex, data = mg)
  expect_equal(length(dim(cif1$pstate)), 2)
  expect_equal(length(dim(cif2$pstate)), 2)
  expect_error(draw_ok(ggcompetingrisks(cif1)), NA)
  expect_error(draw_ok(ggcompetingrisks(cif2)), NA)
})
