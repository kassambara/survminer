# Fleming-Harrington weighted log-rank: weighted_logrank() + surv_pvalue(method="FH").

library(survival)

test_that("weighted_logrank() reproduces the Fleming-Harrington reference values", {
  # lung ~ sex, validated against survival::survdiff and the FH literature.
  w <- weighted_logrank(Surv(time, status) ~ sex, data = lung,
                        rho = c(0, 1, 0), gamma = c(0, 0, 1))
  expect_identical(w$weight, c("FH(0, 0)", "FH(1, 0)", "FH(0, 1)"))
  expect_equal(w$statistic, c(10.33, 12.71, 3.46), tolerance = 1e-2)
  expect_equal(w$p.value, c(0.0013, 0.00036, 0.0629), tolerance = 1e-3)
  expect_true(all(w$df == 1))
})

test_that("FH(0,0) equals the ordinary log-rank test (survdiff)", {
  sd <- survdiff(Surv(time, status) ~ sex, data = lung)
  w0 <- weighted_logrank(Surv(time, status) ~ sex, data = lung, rho = 0, gamma = 0)
  expect_equal(w0$statistic, unname(sd$chisq), tolerance = 1e-6)
})

test_that("FH(1,0) equals survdiff(rho = 1)", {
  sd1 <- survdiff(Surv(time, status) ~ sex, data = lung, rho = 1)
  w10 <- weighted_logrank(Surv(time, status) ~ sex, data = lung, rho = 1, gamma = 0)
  expect_equal(w10$statistic, unname(sd1$chisq), tolerance = 1e-6)
})

test_that("weighted_logrank() recycles rho/gamma and validates input", {
  w <- weighted_logrank(Surv(time, status) ~ sex, data = lung,
                        rho = c(0, 1), gamma = 0)          # gamma recycled
  expect_equal(nrow(w), 2L)
  expect_equal(w$gamma, c(0, 0))
  expect_error(weighted_logrank(Surv(time, status) ~ sex, data = lung, rho = "a"),
               "numeric")
})

test_that("surv_pvalue(method='FH', rho, gamma) returns statistic and df", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  p <- surv_pvalue(fit, data = lung, method = "FH", rho = 1, gamma = 0)
  expect_true(all(c("statistic", "df") %in% names(p)))
  expect_equal(p$statistic, 12.71, tolerance = 1e-2)
  expect_equal(p$df, 1)
  expect_match(p$method, "Fleming-Harrington")
})

test_that("surv_pvalue log-rank now carries statistic and df (no regression)", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  p <- surv_pvalue(fit, data = lung)               # default survdiff log-rank
  expect_equal(p$statistic, 10.33, tolerance = 1e-2)
  expect_equal(p$df, 1)
  expect_equal(p$pval, 0.0013, tolerance = 1e-3)   # p-value unchanged
})

test_that("bare method='FH' stays FH(1,1) (backward compatible)", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  p <- surv_pvalue(fit, data = lung, method = "FH")     # no rho/gamma
  expect_equal(p$statistic, 7.66, tolerance = 1e-2)     # FH(1,1)
})

test_that("a three-group FH test has df = 2", {
  d <- lung[!is.na(lung$ph.ecog) & lung$ph.ecog %in% 0:2, ]
  w <- weighted_logrank(Surv(time, status) ~ ph.ecog, data = d, rho = 0, gamma = 1)
  expect_equal(w$df, 2)
  fit <- survfit(Surv(time, status) ~ ph.ecog, data = d)
  expect_equal(surv_pvalue(fit, data = d, method = "FH", rho = 0, gamma = 1)$df, 2)
})

test_that("ggsurvplot forwards rho/gamma to the plotted p-value", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  # pull the p-value text annotation off the built plot
  plotted_p <- function(p) {
    labs <- unlist(lapply(p$plot$layers, function(l) l$aes_params$label))
    labs <- labs[grepl("^p ", labs)]
    labs[1]
  }
  early <- ggsurvplot(fit, data = lung, pval = TRUE, log.rank.weights = "FH",
                      rho = 1, gamma = 0)          # FH(1,0): p = 4e-04
  late  <- ggsurvplot(fit, data = lung, pval = TRUE, log.rank.weights = "FH",
                      rho = 0, gamma = 1)          # FH(0,1): p = 0.063
  bare  <- ggsurvplot(fit, data = lung, pval = TRUE, log.rank.weights = "FH")  # FH(1,1)
  expect_match(plotted_p(early), "4e-04")
  expect_match(plotted_p(late), "0.06")
  # the three must differ (rho/gamma are not ignored)
  expect_false(identical(plotted_p(early), plotted_p(late)))
  expect_false(identical(plotted_p(early), plotted_p(bare)))
})

test_that("rho/gamma are ignored (with a warning) for a non-FH method", {
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  expect_warning(p <- surv_pvalue(fit, data = lung, method = "1", rho = 1, gamma = 0),
                 "only used with")
  expect_equal(p$statistic, 10.33, tolerance = 1e-2)   # stays log-rank
})

test_that("negative Fleming-Harrington parameters are refused", {
  expect_error(weighted_logrank(Surv(time, status) ~ sex, data = lung, gamma = -1),
               ">= 0")
  fit <- survfit(Surv(time, status) ~ sex, data = lung)
  expect_error(surv_pvalue(fit, data = lung, method = "FH", rho = -1),
               ">= 0")
})
