context("data auto-extraction message for non-global-environment fits")

# Regression test for #521: ggsurvplot(fit) called WITHOUT an explicit `data=`
# re-derives the data via eval(fit$call$data) in .get_data(). When the survfit
# was created in a NON-global environment (e.g. inside a function or a {targets}
# pipeline) the referenced object is out of scope at plot time, and eval() threw
# a cryptic "object '<name>' not found". A survfit stores no reference to its
# creation environment, so the data cannot be recovered here; the cryptic error
# is now converted into an actionable message (telling the user to pass `data=`
# explicitly) that also preserves the original error as context. Working inputs
# (data supplied, or fit fitted in the global scope) are byte-identical.
library(survival)

test_that("no-regression: explicit data is returned unchanged (#521)", {
  fit <- survfit(Surv(time, status) ~ 1, data = lung)
  expect_identical(survminer:::.get_data(fit, data = lung), lung)
})

test_that("no-regression: global-scope fit still auto-extracts its data (#521)", {
  # eval(fit$call$data) succeeds -> same data, same code path as before.
  lungG <<- lung
  fitG  <<- survfit(Surv(time, status) ~ sex, data = lungG)
  on.exit(rm(list = c("lungG", "fitG"), envir = globalenv()), add = TRUE)
  expect_identical(suppressWarnings(survminer:::.get_data(fitG)), lungG)
})

test_that("non-global-env fit without data gives an actionable message (#521)", {
  envir <- new.env(parent = globalenv())
  evalq({
    dat521 <- lung
    survival <- survfit(Surv(time, status) ~ 1, data = dat521)
  }, envir = envir)
  fit <- get("survival", envir = envir)
  rm(envir)  # ensure the `dat521` object is truly out of scope

  msg <- tryCatch(suppressWarnings(survminer:::.get_data(fit)),
                  error = function(e) conditionMessage(e))
  # actionable guidance to pass data explicitly ...
  expect_match(msg, "provide it explicitly", fixed = TRUE)
  expect_match(msg, "data = mydata", fixed = TRUE)
  # ... and the original error is preserved as context (not swallowed).
  expect_match(msg, "Original error", fixed = TRUE)

  # And the same holds end-to-end through ggsurvplot() (which calls .get_data
  # with complain = FALSE, so previously the user saw only the cryptic error).
  msg2 <- tryCatch(ggsurvplot(fit), error = function(e) conditionMessage(e))
  expect_match(msg2, "could not be extracted automatically", fixed = TRUE)
})

test_that("non-global-env fit works when data is supplied explicitly (#521)", {
  envir <- new.env(parent = globalenv())
  evalq({
    dat521 <- lung
    survival <- survfit(Surv(time, status) ~ 1, data = dat521)
  }, envir = envir)
  fit <- get("survival", envir = envir)
  p <- ggsurvplot(fit, data = lung)
  expect_s3_class(p, "ggsurvplot")
})
