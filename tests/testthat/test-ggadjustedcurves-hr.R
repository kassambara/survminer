# Adjusted hazard-ratio annotation on ggadjustedcurves() (show.hr).

library(survival)

.lung2 <- function() {
  d <- lung
  d$sex <- factor(d$sex, labels = c("Male", "Female"))
  d
}
.geoms <- function(p) vapply(p$layers, function(l) class(l$geom)[1], character(1))

test_that("the adjusted HR label matches exp(coef) and its confidence interval", {
  d <- .lung2()
  fit <- coxph(Surv(time, status) ~ sex + age, data = d)
  lab <- survminer:::.adjusted_hr_label(fit, "sex")
  # reference: the model's own adjusted HR for sexFemale
  hr <- unname(exp(coef(fit)["sexFemale"]))
  ci <- exp(confint(fit)["sexFemale", ])
  expect_match(lab, sprintf("%.2f", hr))
  expect_match(lab, sprintf("%.2f", ci[1]))
  expect_match(lab, sprintf("%.2f", ci[2]))
  expect_match(lab, "Female vs Male")
})

test_that("a factor with >2 levels gives one HR line per non-reference level", {
  d <- lung[!is.na(lung$ph.ecog) & lung$ph.ecog %in% 0:2, ]
  d$ph.ecog <- factor(d$ph.ecog)
  fit <- coxph(Surv(time, status) ~ ph.ecog + age, data = d)
  lab <- survminer:::.adjusted_hr_label(fit, "ph.ecog")
  expect_length(lab, 2L)
  expect_match(lab[1], "1 vs 0")
  expect_match(lab[2], "2 vs 0")
  # numbers equal the model coefficients
  hr <- unname(exp(coef(fit))[c("ph.ecog1", "ph.ecog2")])
  expect_match(lab[1], sprintf("%.2f", hr[1]))
  expect_match(lab[2], sprintf("%.2f", hr[2]))
})

test_that("a numeric covariate is labelled without a reference level", {
  d <- .lung2()
  fit <- coxph(Surv(time, status) ~ age + sex, data = d)
  lab <- survminer:::.adjusted_hr_label(fit, "age")
  expect_length(lab, 1L)
  expect_match(lab, "Adjusted HR \\(age, per unit\\)")
  expect_match(lab, sprintf("%.2f", unname(exp(coef(fit)["age"]))))
})

test_that("no HR is drawn for a strata() term or a missing variable (with a warning)", {
  d <- .lung2()
  fit <- coxph(Surv(time, status) ~ age + strata(sex), data = d)
  expect_warning(r <- survminer:::.adjusted_hr_label(fit, "sex"), "strata")
  expect_null(r)
  expect_warning(r2 <- survminer:::.adjusted_hr_label(fit, NULL), "needs a grouping")
  expect_null(r2)
})

test_that("show.hr = FALSE (default) is unchanged; TRUE annotates a subtitle", {
  d <- .lung2()
  fit <- coxph(Surv(time, status) ~ sex + age, data = d)
  p0 <- ggadjustedcurves(fit, data = d, method = "average", variable = "sex")
  expect_false("GeomText" %in% .geoms(p0))
  expect_null(p0$labels$subtitle)
  # default: the HR is a subtitle (outside the panel, collision/clip-proof)
  p1 <- ggadjustedcurves(fit, data = d, method = "average", variable = "sex",
                         show.hr = TRUE)
  expect_match(p1$labels$subtitle, "Adjusted HR")
  expect_false("GeomLabel" %in% .geoms(p1))
  # hr.coord: an in-panel text annotation (boxed label) at the chosen point
  p2 <- ggadjustedcurves(fit, data = d, method = "average", variable = "sex",
                         show.hr = TRUE, hr.coord = c(100, 0.9))
  expect_true("GeomLabel" %in% .geoms(p2))
  txt <- p2$layers[[which(.geoms(p2) == "GeomLabel")]]
  expect_equal(txt$data$x, 100)
  expect_equal(txt$data$y, 0.9)
})

test_that("no HR is drawn (with a warning) for a non-treatment-contrast factor", {
  d <- lung[!is.na(lung$ph.ecog) & lung$ph.ecog %in% 0:2, ]
  d$ph.ecog <- ordered(factor(d$ph.ecog))   # polynomial contrasts (.L, .Q)
  fit <- coxph(Surv(time, status) ~ ph.ecog + age, data = d)
  expect_warning(r <- survminer:::.adjusted_hr_label(fit, "ph.ecog"),
                 "treatment contrasts")
  expect_null(r)
})

test_that("a long factor-level name wraps instead of running off one line", {
  d <- lung
  d$sex <- factor(d$sex, labels = c("Male patients cohort", "Female patients cohort"))
  fit <- coxph(Surv(time, status) ~ sex + age, data = d)
  lab <- survminer:::.adjusted_hr_label(fit, "sex")
  # the full contrast is present once the wrap newlines are flattened to spaces
  flat <- gsub("[\n]+", " ", lab)
  expect_match(flat, "Female patients cohort vs Male patients cohort")
  # every physical line stays within the wrap width (no run-off single line)
  expect_true(all(nchar(strsplit(lab, "\n", fixed = TRUE)[[1]]) <= 34))
})

test_that("no HR is drawn (with a warning) when the variable is in an interaction", {
  d <- .lung2()
  fit <- coxph(Surv(time, status) ~ sex * age, data = d)
  expect_warning(r <- survminer:::.adjusted_hr_label(fit, "sex"), "interaction")
  expect_null(r)
})

test_that("show.hr works together with a risk table", {
  d <- .lung2()
  fit <- coxph(Surv(time, status) ~ sex + age, data = d)
  p <- ggadjustedcurves(fit, data = d, method = "average", variable = "sex",
                        show.hr = TRUE, risk.table = TRUE)
  expect_s3_class(p, "ggsurvplot")
  expect_match(p$plot$labels$subtitle, "Adjusted HR")   # HR subtitle on the curve panel
})
