# surv_adtte(): CDISC ADTTE censoring-flip guard.

.adtte <- function() {
  data.frame(
    USUBJID = sprintf("S%03d", 1:8),
    PARAMCD = rep(c("OS", "PFS"), each = 4),
    AVAL    = c(120, 300, 450, 600, 90, 150, 200, 500),
    CNSR    = c(0, 1, 0, 2, 0, 0, 1, 0),
    TRT01P  = rep(c("A", "B"), 4),
    stringsAsFactors = FALSE
  )
}

test_that("event is derived as CNSR == 0, not 1 - CNSR", {
  d <- .adtte()
  os <- suppressMessages(surv_adtte(d, "OS"))
  # OS rows have CNSR 0,1,0,2 -> event 1,0,1,0 (CNSR = 2 is censored, not -1)
  expect_equal(os$event, c(1L, 0L, 1L, 0L))
  expect_equal(nrow(os), 4L)
  expect_equal(attr(os, "adtte"), list(events = 2L, censored = 2L, n = 4L))
})

test_that("CNSR is read by code for factor / character / double columns", {
  d <- .adtte()
  ref <- suppressMessages(surv_adtte(d, "OS"))$event
  for (coerce in list(as.factor, as.character, as.double)) {
    d2 <- .adtte(); d2$CNSR <- coerce(d2$CNSR)
    expect_equal(suppressMessages(surv_adtte(d2, "OS"))$event, ref)
  }
})

test_that("logical columns and SAS/blank missing are handled", {
  # a logical event.ref (TRUE = event) is cross-checked, not rejected
  d <- .adtte(); d$EVFL <- c(TRUE, FALSE, TRUE, FALSE)   # agrees with OS events
  expect_silent(suppressMessages(surv_adtte(d, "OS", event.ref = "EVFL")))
  # a character AVAL with a SAS "." missing is dropped, not an error
  d2 <- .adtte(); d2$AVAL <- as.character(d2$AVAL); d2$AVAL[1] <- "."
  expect_warning(os <- surv_adtte(d2, "OS"), "missing")
  expect_equal(nrow(os), 3L)
})

test_that("a multi-parameter dataset requires paramcd", {
  d <- .adtte()
  expect_error(surv_adtte(d), "several parameters")
  expect_error(surv_adtte(d, "XYZ"), "No rows")
  # a single-parameter dataset needs no paramcd
  expect_silent(suppressMessages(surv_adtte(d[d$PARAMCD == "OS", ])))
})

test_that("invalid CNSR is rejected", {
  d <- .adtte()
  d1 <- d; d1$CNSR[1] <- NA
  expect_error(surv_adtte(d1, "OS"), "missing")
  d2 <- d; d2$CNSR[1] <- -1
  expect_error(surv_adtte(d2, "OS"), "non-negative")
  d3 <- d; d3$CNSR[1] <- 1.5
  expect_error(surv_adtte(d3, "OS"), "non-negative")
  d4 <- d; d4$CNSR <- as.character(d4$CNSR); d4$CNSR[1] <- "yes"
  expect_error(surv_adtte(d4, "OS"), "non-numeric")
})

test_that("an existing event column is not clobbered silently", {
  d <- .adtte(); d$event <- 99
  expect_error(surv_adtte(d, "OS"), "already exists")
  os <- suppressMessages(surv_adtte(d, "OS", overwrite = TRUE))
  expect_equal(os$event, c(1L, 0L, 1L, 0L))
})

test_that("event.ref disagreement is flagged", {
  d <- .adtte(); d$EVFL <- 1   # claims every record is an event
  expect_warning(surv_adtte(d, "OS", event.ref = "EVFL"), "disagrees")
})

test_that("duplicate subjects and missing/negative AVAL are handled", {
  d <- .adtte(); d$USUBJID[2] <- d$USUBJID[1]
  expect_warning(surv_adtte(d, "OS"), "duplicate")

  d2 <- .adtte(); d2$AVAL[1] <- NA
  expect_warning(os <- surv_adtte(d2, "OS"), "missing")
  expect_equal(nrow(os), 3L)

  d3 <- .adtte(); d3$AVAL[1] <- -5
  expect_error(surv_adtte(d3, "OS"), "negative")
})

test_that("missing required columns error clearly", {
  d <- .adtte()
  expect_error(surv_adtte(d[setdiff(names(d), "CNSR")], "OS"), "CNSR")
  expect_error(surv_adtte(d[setdiff(names(d), "AVAL")], "OS"), "AVAL")
  expect_error(surv_adtte(1:5), "data frame")
})

test_that("the bundled adtte dataset is well-formed and separates clearly", {
  data("adtte", package = "survminer")
  expect_true(all(c("USUBJID", "PARAMCD", "AVAL", "CNSR", "TRT01P") %in% names(adtte)))
  expect_setequal(unique(adtte$PARAMCD), c("OS", "PFS"))
  expect_true(all(adtte$CNSR %in% 0:2))            # 0 event, 1-2 censoring reasons
  # a clear, significant treatment effect for both parameters
  for (pc in c("OS", "PFS")) {
    os <- suppressMessages(surv_adtte(adtte, pc))
    sd <- survival::survdiff(survival::Surv(AVAL, event) ~ TRT01P, data = os)
    p <- stats::pchisq(sd$chisq, length(sd$n) - 1, lower.tail = FALSE)
    expect_lt(p, 0.001)
  }
})

test_that("the result feeds survfit correctly", {
  d <- .adtte()
  os <- suppressMessages(surv_adtte(d, "OS"))
  fit <- survival::survfit(survival::Surv(AVAL, event) ~ TRT01P, data = os)
  # 2 events among the 4 OS records
  expect_equal(sum(fit$n.event), 2)
})
