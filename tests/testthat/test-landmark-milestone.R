# gglandmark() + ggmilestone(): landmark re-origining and milestone survival.

skip_if_not_installed("survival")

.fit2 <- function() {
  survival::survfit(survival::Surv(time, status) ~ sex, data = survival::lung)
}

# ---- gglandmark --------------------------------------------------------------

test_that("gglandmark re-origins the clock and the curve starts at S = 1", {
  fit <- .fit2()
  p <- suppressMessages(gglandmark(fit, data = survival::lung, landmark.time = 200))
  info <- attr(p$plot, "landmark")
  expect_equal(info$landmark.time, 200)
  # every re-origined stratum must start at S = 1 at time 0
  s0 <- summary(info$fit, times = 0)
  expect_true(all(abs(s0$surv - 1) < 1e-8))
  # x-axis is re-origined (first curve time is 0, not the landmark)
  expect_equal(min(p$plot$data$time), 0)
})

test_that("gglandmark keeps subjects at risk at L (time >= L) and re-origins", {
  fit <- .fit2()
  L <- 200
  p <- suppressMessages(gglandmark(fit, data = survival::lung, landmark.time = L))
  info <- attr(p$plot, "landmark")
  # base R landmark cohort: event-free & under observation at L
  d <- survival::lung
  expect_equal(info$n.at.risk, sum(d$time >= L))
  # re-origined follow-up = original - L
  expect_equal(max(p$plot$data$time), max(d$time) - L)
})

test_that("gglandmark excludes an event falling exactly on the landmark", {
  d <- survival::lung
  d$time[1:3] <- 200; d$status[1:3] <- 2      # three events exactly at L = 200
  fit <- survival::survfit(survival::Surv(time, status) ~ sex, data = d)
  p <- suppressMessages(gglandmark(fit, data = d, landmark.time = 200))
  info <- attr(p$plot, "landmark")
  expect_equal(info$n.dropped.event, 3L)
  s0 <- summary(info$fit, times = 0)
  expect_true(all(abs(s0$surv - 1) < 1e-8))   # still starts at 1 despite the ties
})

test_that("gglandmark validates its inputs", {
  fit <- .fit2()
  expect_error(gglandmark("x", data = survival::lung, landmark.time = 1),
               "survfit")
  expect_error(gglandmark(fit, data = survival::lung),
               "landmark.time")
  expect_error(gglandmark(fit, data = survival::lung, landmark.time = -1),
               "non-negative")
  expect_error(gglandmark(fit, data = survival::lung, landmark.time = 1e6),
               "beyond the last observed time")
})

test_that("gglandmark respects a user-supplied subtitle", {
  fit <- .fit2()
  p <- suppressMessages(
    gglandmark(fit, data = survival::lung, landmark.time = 200,
               subtitle = "my subtitle"))
  expect_equal(p$plot$labels$subtitle, "my subtitle")
})

# ---- ggmilestone -------------------------------------------------------------

test_that("ggmilestone tabulates per-arm S(t) and the between-arm difference", {
  fit <- .fit2()
  p <- ggmilestone(fit, data = survival::lung, milestone.times = c(365, 730))
  tab <- attr(p$plot, "milestone.table")
  # per-arm S(t) matches summary.survfit
  s <- summary(fit, times = 365)
  arm1 <- tab$surv[tab$group == levels(factor(tab$group))[1] & tab$time == 365]
  expect_equal(round(arm1, 6), round(s$surv[1], 6))
  # difference row = S_A - S_B with additive Greenwood variance
  dr <- tab[grepl(" - ", tab$group) & tab$time == 365, ]
  expect_equal(nrow(dr), 1L)
  a <- tab[tab$group == "1" & tab$time == 365, ]
  b <- tab[tab$group == "2" & tab$time == 365, ]
  expect_equal(dr$surv, a$surv - b$surv)
  expect_equal(dr$se, sqrt(a$se^2 + b$se^2))
  expect_true(dr$p.value > 0 && dr$p.value < 1)
})

test_that("ggmilestone returns NA (with a warning) for a time beyond follow-up", {
  fit <- .fit2()
  expect_warning(p <- ggmilestone(fit, data = survival::lung,
                                  milestone.times = c(365, 1e6)),
                 "beyond the observed follow-up")
  tab <- attr(p$plot, "milestone.table")
  expect_true(all(is.na(tab$surv[tab$time == 1e6 & !grepl(" - ", tab$group)])))
})

test_that("ggmilestone gives no difference for one arm", {
  f1 <- survival::survfit(survival::Surv(time, status) ~ 1, data = survival::lung)
  p <- ggmilestone(f1, data = survival::lung, milestone.times = 365)
  tab <- attr(p$plot, "milestone.table")
  expect_false("p.value" %in% names(tab))
  expect_equal(nrow(tab), 1L)
})

test_that("ggmilestone needs ref.group for 3+ arms and validates it", {
  d <- survival::lung
  d$g <- factor(pmin(d$ph.ecog, 2)); d <- d[!is.na(d$g), ]
  f3 <- survival::survfit(survival::Surv(time, status) ~ g, data = d)
  # no ref.group -> per-arm only (no difference rows)
  p <- ggmilestone(f3, data = d, milestone.times = 365)
  expect_false(any(grepl(" - ", attr(p$plot, "milestone.table")$group)))
  # ref.group -> difference vs the reference
  pr <- ggmilestone(f3, data = d, milestone.times = 365, ref.group = "0")
  expect_true(any(grepl(" - 0$", attr(pr$plot, "milestone.table")$group)))
  expect_error(ggmilestone(f3, data = d, milestone.times = 365, ref.group = "z"),
               "ref.group")
})

test_that("ggmilestone validates milestone.times and conf.level", {
  fit <- .fit2()
  expect_error(ggmilestone(fit, data = survival::lung, milestone.times = -5),
               "positive")
  expect_error(ggmilestone(fit, data = survival::lung, milestone.times = c(1, NA)),
               "positive")
  expect_error(ggmilestone(fit, data = survival::lung, milestone.times = 365,
                           conf.level = 2), "conf.level")
})

test_that("ggmilestone names the arms in the caption and honours legend.labs", {
  fit <- .fit2()
  p <- ggmilestone(fit, data = survival::lung, milestone.times = 365,
                   legend.labs = c("Male", "Female"))
  cap <- p$plot$labels$caption
  expect_true(grepl("Male", cap) && grepl("Female", cap))   # plot labels, not "sex=1"
  # single milestone goes to the caption, not a clipping-prone subtitle
  expect_null(p$plot$labels$subtitle)
})

test_that("ggmilestone caption is robust to an arm label containing ' - '", {
  d <- survival::lung
  d$arm <- factor(ifelse(seq_len(nrow(d)) <= 100, "A - B", "C"))
  fit <- survival::survfit(survival::Surv(time, status) ~ arm, data = d)
  p <- ggmilestone(fit, data = d, milestone.times = c(200, 400))
  # the label collides with the " - " difference separator; the caption must not
  # mangle it into NA (regression: string-splitting the label).
  expect_false(grepl("NA", p$plot$labels$caption))
  expect_true(grepl("A - B", p$plot$labels$caption))
})

test_that("ggmilestone respects a user-supplied caption", {
  fit <- .fit2()
  p <- ggmilestone(fit, data = survival::lung, milestone.times = c(365, 730),
                   caption = "MY CAPTION")
  expect_equal(p$plot$labels$caption, "MY CAPTION")
})

test_that("ggmilestone gives no NaN p-value for a milestone before any event", {
  fit <- .fit2()
  p <- ggmilestone(fit, data = survival::lung, milestone.times = 1)
  tab <- attr(p$plot, "milestone.table")
  expect_false(any(is.nan(tab$p.value)))          # 0/0 must be guarded to NA
  expect_false(grepl("NaN", p$plot$labels$caption))
})

test_that("ggmilestone reports 0 beyond follow-up when the curve has reached 0", {
  d <- data.frame(time = c(1, 2, 3, 5, 10, 15),
                  status = c(1, 1, 1, 1, 1, 0),
                  g = rep(c("allevent", "censlast"), each = 3))
  fit <- survival::survfit(survival::Surv(time, status) ~ g, data = d)
  p <- suppressWarnings(ggmilestone(fit, data = d, milestone.times = 20))
  tab <- attr(p$plot, "milestone.table")
  # allevent's curve reaches 0 -> S(20) = 0 (estimable); censlast -> NA
  expect_equal(tab$surv[tab$group == "allevent" & tab$time == 20], 0)
  expect_true(is.na(tab$surv[tab$group == "censlast" & tab$time == 20]))
})

test_that("ggmilestone rejects an empty-string arm label", {
  d <- survival::lung
  d$arm <- factor(ifelse(seq_len(nrow(d)) <= 100, "", "X"))
  fit <- survival::survfit(survival::Surv(time, status) ~ arm, data = d)
  expect_error(ggmilestone(fit, data = d, milestone.times = 100), "empty string")
})

test_that("gglandmark re-origined curve equals an independent manual survfit", {
  # cross-check: our re-origining must reproduce a from-scratch survfit on the
  # manually subset + re-origined data (independent of the gglandmark code path).
  d <- survival::lung; L <- 200
  fit <- .fit2()
  flm <- attr(suppressMessages(
    gglandmark(fit, data = d, landmark.time = L))$plot, "landmark")$fit
  man <- d[d$time >= L & !(d$time == L & d$status == 2), ]
  man$nt <- man$time - L
  ind <- survival::survfit(survival::Surv(nt, status == 2) ~ sex, data = man)
  ours <- summary(flm, times = c(0, 100, 300))
  ref  <- summary(ind, times = c(0, 100, 300))
  expect_equal(ours$surv, ref$surv)
})

test_that("ggmilestone difference equals the independent Greenwood formula", {
  fit <- .fit2()
  tab <- attr(ggmilestone(fit, data = survival::lung,
                          milestone.times = 365)$plot, "milestone.table")
  a <- tab[tab$group == "1" & tab$time == 365, ]
  b <- tab[tab$group == "2" & tab$time == 365, ]
  dr <- tab[grepl(" - ", tab$group), ]
  # independent recomputation from survival::summary.survfit
  s <- summary(fit, times = 365)
  d.ref  <- s$surv[1] - s$surv[2]
  se.ref <- sqrt(s$std.err[1]^2 + s$std.err[2]^2)
  p.ref  <- 2 * stats::pnorm(-abs(d.ref / se.ref))
  expect_equal(dr$surv, d.ref)
  expect_equal(dr$se, se.ref)
  expect_equal(dr$p.value, p.ref)
})

test_that("both functions return a printable ggsurvplot", {
  fit <- .fit2()
  p1 <- suppressMessages(gglandmark(fit, data = survival::lung, landmark.time = 200))
  p2 <- ggmilestone(fit, data = survival::lung, milestone.times = 365)
  expect_s3_class(p1, "ggsurvplot")
  expect_s3_class(p2, "ggsurvplot")
  expect_s3_class(p1$plot, "ggplot")
  expect_s3_class(p2$plot, "ggplot")
})
