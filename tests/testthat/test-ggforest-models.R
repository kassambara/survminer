# ggforest_models(): compare one term's HR across several Cox models.

skip_if_not_installed("survival")

.mm <- function() {
  library(survival)
  list(crude    = coxph(Surv(time, status) ~ age, data = lung),
       adjusted = coxph(Surv(time, status) ~ age + sex + ph.ecog, data = lung))
}

test_that("the HR / CI / p per model match a direct coxph extraction (and broom)", {
  library(survival)
  models <- .mm()
  p <- ggforest_models(models, term = "age")
  expect_s3_class(p, "ggplot")

  tab <- survminer:::.models_forest_table(models, "age", 0.95, NULL)
  for (i in seq_along(models)) {
    m <- models[[i]]
    b <- coef(m)["age"]; se <- sqrt(vcov(m)["age", "age"]); z <- qnorm(0.975)
    expect_equal(tab$hr[i],    unname(exp(b)),            tolerance = 1e-8)
    expect_equal(tab$lower[i], unname(exp(b - z * se)),   tolerance = 1e-8)
    expect_equal(tab$upper[i], unname(exp(b + z * se)),   tolerance = 1e-8)
    expect_equal(tab$p[i],     unname(2 * pnorm(-abs(b / se))), tolerance = 1e-8)
    # independent cross-check against broom
    if (requireNamespace("broom", quietly = TRUE)) {
      bt <- broom::tidy(m, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95)
      br <- bt[bt$term == "age", ]
      expect_equal(tab$hr[i],    br$estimate,  tolerance = 1e-6)
      expect_equal(tab$lower[i], br$conf.low,  tolerance = 1e-4)
      expect_equal(tab$upper[i], br$conf.high, tolerance = 1e-4)
    }
  }
})

test_that("conf.int controls the interval width", {
  models <- .mm()
  t95 <- survminer:::.models_forest_table(models, "age", 0.95, NULL)
  t90 <- survminer:::.models_forest_table(models, "age", 0.90, NULL)
  expect_true(all(t90$upper < t95$upper))   # 90% CI narrower
  expect_true(all(t90$lower > t95$lower))
})

test_that("a term matching several coefficients errors with the names", {
  library(survival)
  m <- coxph(Surv(time, status) ~ rx, data = colon)
  expect_error(ggforest_models(list(a = m), term = "rx"),
               "matches several coefficients")
})

test_that("a model missing the term is dropped with a warning", {
  library(survival)
  models <- list(has = coxph(Surv(time, status) ~ age + sex, data = lung),
                 not = coxph(Surv(time, status) ~ sex, data = lung))
  expect_warning(p <- ggforest_models(models, term = "age"), "does not contain")
  expect_s3_class(p, "ggplot")
  # if every model lacks the term -> stop
  expect_error(
    suppressWarnings(ggforest_models(list(a = models$not), term = "age")),
    "nothing to plot")
})

test_that("non-coxph elements error, naming the offending index", {
  models <- .mm()
  expect_error(ggforest_models(c(models, list(lm(age ~ 1, data = survival::lung))),
                               term = "age"),
               "must be a coxph model")
})

test_that("a very wide interval is clamped with a warning (arrow), value kept", {
  library(survival)
  set.seed(1); d <- lung; d$rare <- rbinom(nrow(d), 1, 0.05)
  models <- list(m1 = coxph(Surv(time, status) ~ age + rare, data = d),
                 m2 = coxph(Surv(time, status) ~ rare, data = d[d$age > 65, ]))
  expect_warning(ggforest_models(models, term = "rare"),
                 "extends beyond the plotted range")
})

test_that("unnamed / duplicate model labels are made unique", {
  library(survival)
  m <- coxph(Surv(time, status) ~ age, data = lung)
  tab <- survminer:::.models_forest_table(list(m, m), "age", 0.95, NULL)
  expect_equal(tab$model, c("Model 1", "Model 2"))
  tab2 <- survminer:::.models_forest_table(
    stats::setNames(list(m, m), c("A", "A")), "age", 0.95, NULL)
  expect_equal(anyDuplicated(tab2$model), 0L)
})

test_that("a differing reference level across models is flagged", {
  library(survival)
  d <- lung; d$sexf <- factor(d$sex, labels = c("male", "female"))
  d2 <- d; d2$sexf <- relevel(d2$sexf, "female")
  ma <- coxph(Surv(time, status) ~ sexf, data = d)    # ref male
  mb <- coxph(Surv(time, status) ~ sexf, data = d2)   # ref female
  expect_warning(ggforest_models(list(A = ma, B = mb), term = "sexf"),
                 "different coefficients")
})

test_that("a partially named models list keeps the provided names", {
  library(survival)
  m1 <- coxph(Surv(time, status) ~ age, data = lung)
  m2 <- coxph(Surv(time, status) ~ age + sex, data = lung)
  tab <- survminer:::.models_forest_table(
    stats::setNames(list(m1, m2), c("Named", "")), "age", 0.95, NULL)
  expect_equal(tab$model, c("Named", "Model 2"))
})

test_that("the CI-column header tracks conf.int (no hardcoded 95%)", {
  library(survival)
  m <- coxph(Surv(time, status) ~ age, data = lung)
  labs <- function(p) {
    gt <- grid::grid.force(ggplot2::ggplotGrob(p)); L <- character()
    w <- function(g) { if (inherits(g, "gTree") && !is.null(g$children))
      for (n in names(g$children)) w(g$children[[n]])
      if (!is.null(g$label)) L <<- c(L, as.character(g$label)) }
    w(gt); L
  }
  expect_true(any(grepl("HR (80% CI)", labs(
    ggforest_models(list(A = m, B = m), term = "age", conf.int = 0.8)), fixed = TRUE)))
})
