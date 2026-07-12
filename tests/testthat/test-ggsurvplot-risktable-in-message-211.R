context("in-plot risk table y.text message (#211)")

# With risk.table.pos = "in", the risk table is drawn over the survival panel's
# own y-axis, so its strata y-axis labels are blanked (the rows are coloured by
# strata instead). risk.table.y.text = TRUE therefore has no effect there. Rather
# than silently ignoring an explicit request, ggsurvplot() now says so -- but only
# when the user EXPLICITLY passes both risk.table.pos = "in" and
# risk.table.y.text = TRUE, so a default in-plot table (y.text = TRUE by default)
# is not spammed.
library(survival)
fit <- survfit(Surv(time, status) ~ sex, data = lung)

# TRUE if the expression emits the #211 message (robust to unrelated messages)
emits_211 <- function(expr) {
  msgs <- character(0)
  withCallingHandlers(force(expr),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  any(grepl("no effect with", msgs, fixed = TRUE))
}

test_that("explicit pos='in' + y.text=TRUE emits the message (#211)", {
  expect_true(emits_211(
    ggsurvplot(fit, data = lung, risk.table = TRUE, risk.table.pos = "in",
               risk.table.y.text = TRUE)))
})

test_that("the message also fires for a string-valued risk.table type in-plot (#211)", {
  # risk.table = "abs_pct" (a type string) draws an in-plot table too, so it has
  # the same silent-ignore of risk.table.y.text.
  expect_true(emits_211(
    ggsurvplot(fit, data = lung, risk.table = "abs_pct", risk.table.pos = "in",
               risk.table.y.text = TRUE)))
})

test_that("the message does NOT fire for default / other configurations (#211)", {
  # default in-plot table (y.text = TRUE by default, but not explicitly passed)
  expect_false(emits_211(
    ggsurvplot(fit, data = lung, risk.table = TRUE, risk.table.pos = "in")))
  # explicit y.text = FALSE
  expect_false(emits_211(
    ggsurvplot(fit, data = lung, risk.table = TRUE, risk.table.pos = "in",
               risk.table.y.text = FALSE)))
  # out-of-plot table (labels do show there)
  expect_false(emits_211(
    ggsurvplot(fit, data = lung, risk.table = TRUE, risk.table.pos = "out",
               risk.table.y.text = TRUE)))
  # y.text.col must not partial-match risk.table.y.text
  expect_false(emits_211(
    ggsurvplot(fit, data = lung, risk.table = TRUE, risk.table.pos = "in",
               risk.table.y.text.col = TRUE)))
})
