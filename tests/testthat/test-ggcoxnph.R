# Non-proportional-hazards diagnostic panel, ggcoxnph().

library(survival)

.lung2 <- function() {
  d <- lung
  d$sex <- factor(d$sex, labels = c("Male", "Female"))
  d
}
.lung3 <- function() {
  d <- .lung2()
  d <- d[!is.na(d$ph.ecog) & d$ph.ecog %in% 0:2, ]
  d$ph.ecog <- factor(d$ph.ecog)
  d
}

test_that("a two-level covariate yields all four panels with the right class", {
  d <- .lung2()
  fit <- coxph(Surv(time, status) ~ sex + age, data = d)
  p <- ggcoxnph(fit, variable = "sex")
  expect_s3_class(p, "ggcoxnph")
  expect_setequal(names(p), c("cloglog", "schoenfeld", "hr", "rmst"))
  expect_true(all(vapply(p, ggplot2::is.ggplot, logical(1))))
})

test_that("a single-term model uses that covariate by default", {
  d <- .lung2()
  fit <- coxph(Surv(time, status) ~ sex, data = d)
  expect_silent(suppressMessages(p <- ggcoxnph(fit)))
  expect_equal(attr(p, "variable"), "sex")
})

test_that("a multi-term model requires `variable`", {
  d <- .lung2()
  fit <- coxph(Surv(time, status) ~ sex + age, data = d)
  expect_error(ggcoxnph(fit), "set `variable`")
})

test_that("a numeric covariate skips the group panels unless split", {
  d <- .lung2()
  fit <- coxph(Surv(time, status) ~ age + sex, data = d)
  suppressMessages(p1 <- ggcoxnph(fit, variable = "age"))
  expect_setequal(names(p1), c("schoenfeld", "hr"))
  suppressMessages(p2 <- ggcoxnph(fit, variable = "age", numeric.split = TRUE))
  expect_setequal(names(p2), c("cloglog", "schoenfeld", "hr", "rmst"))
})

test_that("a >2-level factor drops the single-coefficient panels", {
  d <- .lung3()
  fit <- coxph(Surv(time, status) ~ ph.ecog + age, data = d)
  suppressMessages(p <- ggcoxnph(fit, variable = "ph.ecog"))
  expect_setequal(names(p), c("cloglog", "rmst"))
  expect_false(any(c("schoenfeld", "hr") %in% names(p)))
})

test_that("the HR panel is exactly exp() of the Schoenfeld beta(t) smooth", {
  d <- .lung2()
  fit <- coxph(Surv(time, status) ~ sex + age, data = d)
  p <- ggcoxnph(fit, variable = "sex")
  b <- attr(p, "data")$beta_t
  # the HR trend line is exp(beta) on the same time grid (y is on a log10 scale,
  # so the layer's transformed y back-transforms to exp(beta))
  gl <- which(vapply(p$hr$layers, function(l) inherits(l$geom, "GeomLine"),
                     logical(1)))[1]
  hr.data <- ggplot2::layer_data(p$hr, gl)
  expect_equal(sort(round(exp(b$beta), 6)), sort(round(10^hr.data$y, 6)))
})

test_that("the reference lines use the Cox coefficient exactly", {
  d <- .lung2()
  fit <- coxph(Surv(time, status) ~ sex + age, data = d)
  p <- ggcoxnph(fit, variable = "sex")
  # a factor's coefficient is named "sexFemale", not the term name "sex"
  expect_equal(attr(p, "data")$beta_hat, unname(coef(fit)["sexFemale"]))
  expect_false(is.na(attr(p, "data")$beta_hat))
  # the reference line is actually drawn in both panels
  hl <- function(pl) sum(vapply(pl$layers,
        function(l) inherits(l$geom, "GeomHline"), logical(1)))
  expect_gte(hl(p$schoenfeld), 1L)   # beta-hat line
  expect_gte(hl(p$hr), 2L)           # HR=1 and exp(beta-hat)
  # the Grambsch-Therneau p is the one from cox.zph for this term
  expect_equal(attr(p, "data")$gt_pval,
               unname(cox.zph(fit)$table["sex", "p"]), tolerance = 1e-8)
})

test_that("the RMST-vs-tau curve matches an independent RMST at the same tau", {
  d <- .lung2()
  fit <- coxph(Surv(time, status) ~ sex + age, data = d)
  p <- ggcoxnph(fit, variable = "sex")
  rt <- attr(p, "data")$rmst_tau
  # reference: exact-KM RMST for the Female arm at the largest tau on the grid
  df <- data.frame(time = d$time, status = d$status, sex = d$sex)
  df <- df[complete.cases(df), ]
  tau <- max(rt$groups$tau)
  female <- df[df$sex == "Female", ]
  f <- survfit(Surv(time, status) ~ 1, data = female)
  tt <- f$time; S <- f$surv
  keep <- tt <= tau
  gt <- c(0, tt[keep], tau); gS <- c(1, S[keep], S[keep][sum(keep)])
  ref <- sum(gS[-length(gS)] * diff(gt))
  got <- rt$groups$rmst[rt$groups$.g == "Female" & rt$groups$tau == tau]
  expect_equal(got, ref, tolerance = 1e-6)
})

test_that("panels= subsets the drawn panels", {
  d <- .lung2()
  fit <- coxph(Surv(time, status) ~ sex + age, data = d)
  p <- ggcoxnph(fit, variable = "sex", panels = c("schoenfeld", "hr"))
  expect_setequal(names(p), c("schoenfeld", "hr"))
})

test_that("a non-coxph input errors", {
  expect_error(ggcoxnph(lm(1 ~ 1)), "must be a coxph")
})
