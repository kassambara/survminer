context("ggcoxfunctional non-continuous terms")

# Regression test for #357: ggcoxfunctional() failed with a cryptic
# "'x' and 'y' lengths differ" error when the Cox formula contained a factor or
# character covariate (its model-matrix column is renamed, e.g. sex -> sex2, so
# it no longer matches the term label). Non-continuous terms are now dropped
# with a warning; continuous-only formulas are unaffected.
library(survival)

# complete-case numeric data to avoid the separate missing-data issue (#248)
cc <- na.omit(lung[, c("time", "status", "age", "ph.karno", "wt.loss", "sex")])
cc$sex <- factor(cc$sex, labels = c("male", "female"))

test_that("ggcoxfunctional() skips a factor covariate with a warning (#357)", {
  fit <- coxph(Surv(time, status) ~ age + sex + ph.karno, data = cc)
  expect_warning(p <- ggcoxfunctional(fit, data = cc), "non-continuous")
  expect_identical(names(p), c("age", "ph.karno"))   # sex dropped
  expect_s3_class(p, "ggcoxfunctional")
})

test_that("ggcoxfunctional() also skips character terms and strata() (#357)", {
  cc2 <- cc
  cc2$grp <- ifelse(cc2$age > 60, "old", "young")
  fit_chr <- coxph(Surv(time, status) ~ age + grp, data = cc2)
  expect_warning(p <- ggcoxfunctional(fit_chr, data = cc2), "non-continuous")
  expect_identical(names(p), "age")

  fit_str <- coxph(Surv(time, status) ~ age + strata(sex), data = cc)
  expect_warning(p2 <- ggcoxfunctional(fit_str, data = cc), "non-continuous")
  expect_identical(names(p2), "age")
})

test_that("no-regression: a continuous-only formula is unaffected (#357)", {
  fit <- coxph(Surv(time, status) ~ age + ph.karno, data = cc)
  expect_silent(p <- ggcoxfunctional(fit, data = cc))
  expect_identical(names(p), c("age", "ph.karno"))
})

test_that("ggcoxfunctional() errors clearly when no continuous covariate remains (#357)", {
  fit <- coxph(Surv(time, status) ~ sex, data = cc)
  expect_error(suppressWarnings(ggcoxfunctional(fit, data = cc)),
               "No continuous covariate")
})
