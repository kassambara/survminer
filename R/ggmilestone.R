#' @include utilities.R gglandmark.R
NULL
#' Milestone (Fixed-Time) Survival
#'
#' @description
#' Annotates Kaplan-Meier curves with \strong{milestone survival}: the survival
#' probability \eqn{S(t)} at one or more clinically meaningful fixed times (e.g. 1-
#' and 2-year survival) with its confidence interval, and -- for a two-arm
#' comparison -- the \strong{between-arm difference} in milestone survival with its
#' confidence interval and p-value. Milestone survival is a standard trial endpoint
#' when a single hazard ratio is not enough (e.g. under non-proportional hazards).
#'
#' @details
#' For each arm and milestone the estimate is the Kaplan-Meier \eqn{S(t)} with its
#' Greenwood standard error and confidence interval (\code{summary.survfit}; the
#' interval uses the same \code{conf.type} as \code{fit}). The between-arm difference
#' is on the risk-difference (identity) scale, \eqn{S_A(t) - S_B(t)}, with
#' \eqn{se = \sqrt{se_A^2 + se_B^2}} (arms independent), a normal confidence interval
#' and Wald p-value -- the quantity clinical reports quote. Note (Klein et al., 2007)
#' that this naive identity-scale inference can have inflated type-I error and poor
#' coverage when \eqn{S} is close to 0 or 1 or the sample is small; interpret such
#' cases with care.
#'
#' A milestone time beyond an arm's follow-up is \strong{not} estimable unless every
#' subject in that arm has already had the event (the curve has reached \eqn{S = 0}):
#' otherwise that arm's \eqn{S(t)} is returned as \code{NA} with a warning, rather
#' than silently dropped or carried forward as \code{summary.survfit} would.
#'
#' The between-arm difference is defined only for two arms; for three or more arms,
#' set \code{ref.group} to report each other arm's difference against a reference
#' (otherwise only the per-arm milestone table is produced). For a single arm only
#' the per-arm milestone survival is shown.
#'
#' @param fit a survfit object (\code{survival::survfit()} or survminer's
#'   \code{\link{surv_fit}()}).
#' @param data the data frame used to fit the curves (see \code{\link{ggsurvplot}}).
#' @param milestone.times numeric vector of the fixed times at which to read
#'   survival (in the data's time units). Required.
#' @param conf.int logical; draw the confidence-interval band of the Kaplan-Meier
#'   curves. Default \code{FALSE} (matching \code{\link{ggsurvplot}()}). The milestone
#'   confidence level is set by \code{conf.level}.
#' @param conf.level confidence level for the milestone survival intervals and the
#'   between-arm difference. Default 0.95.
#' @param ref.group for three or more arms, the reference arm the milestone
#'   differences are taken against (an arm label). Default \code{NULL} reports only
#'   the per-arm milestone survival. Ignored for one or two arms.
#' @param label.milestones logical or \code{NULL}; draw the per-arm \eqn{S(t)}
#'   value labels on the curves. Default \code{NULL} labels them only when the plot
#'   stays legible (arms x milestones \eqn{\le} 4) and otherwise omits them (the
#'   values remain in the caption and the attached table). \code{TRUE}/\code{FALSE}
#'   force it on/off.
#' @param ... other arguments passed to \code{\link{ggsurvplot}()} (e.g.
#'   \code{palette}, \code{legend.labs}, \code{risk.table}). A \code{caption}
#'   supplied here is respected; otherwise the milestone summary is placed there.
#'
#' @return a \code{ggsurvplot} object: the Kaplan-Meier curves with a dashed line at
#'   each milestone, the per-arm \eqn{S(t)} marked, and the milestone survival (and
#'   between-arm difference) reported in the caption. The full milestone table --
#'   per-arm \eqn{S(t)}, CI, and any between-arm differences with CI and p-value --
#'   is attached as \code{attr(x$plot, "milestone.table")}.
#'
#' @references
#' Klein JP, Logan B, Harhoff M, Andersen PK (2007). Analyzing survival curves at a
#' fixed point in time. \emph{Statistics in Medicine} 26(24):4505-4519.
#'
#' @seealso \code{\link{gglandmark}}, \code{\link{ggrmst}}, \code{\link{ggsurvplot}}.
#'
#' @examples
#' library(survival)
#' fit <- survfit(Surv(time, status) ~ sex, data = lung)
#'
#' # 1- and 2-year (365 / 730 day) milestone survival with the between-arm difference
#' ggmilestone(fit, data = lung, milestone.times = c(365, 730))
#'
#' # recover the full milestone table
#' p <- ggmilestone(fit, data = lung, milestone.times = c(365, 730))
#' attr(p$plot, "milestone.table")
#'
#' @export
ggmilestone <- function(fit, data = NULL, milestone.times, conf.int = FALSE,
                        conf.level = 0.95, ref.group = NULL,
                        label.milestones = NULL, ...) {

  if (!.is_survfit(fit))
    stop("`fit` must be a survfit object.", call. = FALSE)
  if (missing(milestone.times) || !is.numeric(milestone.times) ||
      !length(milestone.times) || anyNA(milestone.times) || any(milestone.times <= 0))
    stop("`milestone.times` must be a vector of positive numbers.", call. = FALSE)
  if (!is.numeric(conf.level) || length(conf.level) != 1L ||
      conf.level <= 0 || conf.level >= 1)
    stop("`conf.level` must be a single number in (0, 1).", call. = FALSE)
  milestone.times <- sort(unique(milestone.times))

  ext <- .km_reorigin_extract(fit, data)
  glevels <- levels(ext$group)
  if (any(!nzchar(glevels)))
    stop("An arm/group label is an empty string; give each group a non-empty ",
         "label before calling ggmilestone().", call. = FALSE)
  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  ct <- if (!is.null(fit$conf.type)) fit$conf.type else "log"

  # ---- per-arm milestone S(t) --------------------------------------------------
  # summary() at t within follow-up; a time beyond follow-up is estimable only when
  # the arm's curve has reached S = 0 (everyone has had the event) -- otherwise NA.
  per <- lapply(glevels, function(g) {
    i  <- ext$group == g
    ti <- ext$time[i]; si <- ext$status[i]
    fa <- survival::survfit(survival::Surv(ti, si) ~ 1,
                            conf.type = ct, conf.int = conf.level)
    tmax <- max(ti, na.rm = TRUE)
    s.end <- fa$surv[length(fa$surv)]
    do.call(rbind, lapply(milestone.times, function(t) {
      na.row <- data.frame(group = g, time = t, surv = NA_real_, se = NA_real_,
                           lower = NA_real_, upper = NA_real_,
                           row.names = NULL, stringsAsFactors = FALSE)
      if (t <= tmax) {
        sm <- summary(fa, times = t, extend = FALSE)
        return(data.frame(group = g, time = t, surv = sm$surv, se = sm$std.err,
                          lower = sm$lower, upper = sm$upper,
                          row.names = NULL, stringsAsFactors = FALSE))
      }
      if (isTRUE(s.end == 0)) {   # curve has reached 0: S(t) = 0 for all later t
        na.row$surv <- 0; na.row$se <- 0; na.row$lower <- 0; na.row$upper <- 0
        return(na.row)
      }
      na.row                       # beyond follow-up and not estimable
    }))
  })
  names(per) <- glevels
  tab <- do.call(rbind, per)

  n.beyond <- sum(is.na(tab$surv))
  if (n.beyond) {
    b <- tab[is.na(tab$surv), ]
    warning(n.beyond, " arm x milestone combination(s) lie beyond the observed ",
            "follow-up and are not estimable (returned as NA): ",
            paste(sprintf("%s @ %g", b$group, b$time), collapse = "; "), ".",
            call. = FALSE)
  }

  # ---- between-arm differences (k = 2 both; k >= 3 vs ref.group) ---------------
  diffs <- NULL
  if (length(glevels) == 2L) {
    diffs <- list(c(glevels[1], glevels[2]))
  } else if (length(glevels) >= 3L && !is.null(ref.group)) {
    if (!ref.group %in% glevels)
      stop("`ref.group` must be one of the arm labels: ",
           paste(glevels, collapse = ", "), call. = FALSE)
    diffs <- lapply(setdiff(glevels, ref.group), function(g) c(g, ref.group))
  }

  # difference statistics for a pair at milestone index k (structured, never parsed
  # back out of a label -- an arm name may itself contain " - ").
  diff_stats <- function(pr, k) {
    a <- per[[pr[1]]]; b <- per[[pr[2]]]
    d  <- a$surv[k] - b$surv[k]
    sd <- sqrt(a$se[k]^2 + b$se[k]^2)
    p  <- if (isTRUE(sd > 0)) 2 * stats::pnorm(-abs(d / sd)) else NA_real_
    list(d = d, sd = sd, lower = d - z * sd, upper = d + z * sd, p = p)
  }

  diff.tab <- NULL
  if (!is.null(diffs)) {
    diff.tab <- do.call(rbind, lapply(diffs, function(pr) {
      do.call(rbind, lapply(seq_along(milestone.times), function(k) {
        s <- diff_stats(pr, k)
        data.frame(group = paste(pr[1], "-", pr[2]), time = milestone.times[k],
                   surv = s$d, se = s$sd, lower = s$lower, upper = s$upper,
                   p.value = s$p, row.names = NULL, stringsAsFactors = FALSE)
      }))
    }))
    tab$p.value <- NA_real_
  }
  milestone.table <- if (is.null(diff.tab)) tab else rbind(tab, diff.tab)

  # ---- base Kaplan-Meier plot --------------------------------------------------
  dots <- list(...)
  p  <- ggsurvplot(fit, data = ext$data, conf.int = conf.int, ...)
  gg <- if (inherits(p, "ggsurvplot")) p$plot else p

  # positional remap of arm labels to the plot's strata levels (ggsurvplot may
  # relabel via legend.labs / palette), mirroring ggrmst().
  plot.strata <- levels(gg$data$strata)
  remap <- if (!is.null(plot.strata) && length(plot.strata) == length(glevels))
    stats::setNames(plot.strata, glevels) else stats::setNames(glevels, glevels)
  lab <- function(g) remap[[g]]

  pts <- tab[!is.na(tab$surv), , drop = FALSE]
  pts$strata <- factor(remap[pts$group],
                       levels = if (!is.null(plot.strata)) plot.strata else glevels)

  # vertical dashed lines at each milestone (drawn before the points).
  gg <- gg + ggplot2::geom_vline(xintercept = milestone.times,
                                 linetype = "dashed", colour = "grey50",
                                 linewidth = 0.4)
  time <- surv <- strata <- NULL
  if (nrow(pts)) {
    gg <- gg + ggplot2::geom_point(
      data = pts, ggplot2::aes(x = time, y = surv, colour = strata),
      inherit.aes = FALSE, size = 2.2, show.legend = FALSE)
    show.lab <- if (is.null(label.milestones))
      nrow(pts) <= 4L else isTRUE(label.milestones)
    if (show.lab) {
      pts$mlab <- paste0(round(100 * pts$surv), "%")
      gg <- gg + ggplot2::geom_label(
        data = pts, ggplot2::aes(x = time, y = surv, label = mlab, colour = strata),
        inherit.aes = FALSE, show.legend = FALSE, size = 3.7, hjust = -0.15,
        linewidth = 0, fill = grDevices::adjustcolor("white", 0.7))
    }
  }

  # ---- caption (arm-named, one compact line per milestone) ---------------------
  pc <- function(x) if (is.na(x)) "NA" else paste0(round(100 * x), "%")
  pcs <- function(x) if (is.na(x)) "NA" else sprintf("%+d%%", round(100 * x))
  cl <- round(100 * conf.level)
  if (!is.null(diffs)) {
    lines <- unlist(lapply(diffs, function(pr) {
      a <- per[[pr[1]]]; b <- per[[pr[2]]]
      vapply(seq_along(milestone.times), function(k) {
        t <- milestone.times[k]
        if (is.na(a$surv[k]) || is.na(b$surv[k]))
          return(sprintf("t=%g: %s vs %s not estimable (beyond follow-up)",
                         t, lab(pr[1]), lab(pr[2])))
        s <- diff_stats(pr, k)
        sprintf("t=%g: %s %s vs %s %s  (diff %s, %d%% CI %s to %s%s)",
                t, lab(pr[1]), pc(a$surv[k]), lab(pr[2]), pc(b$surv[k]),
                pcs(s$d), cl, pcs(s$lower), pcs(s$upper),
                if (is.na(s$p)) "" else paste0(", p=",
                  format.pval(s$p, digits = 2, eps = 1e-4)))
      }, character(1))
    }))
  } else {
    lines <- vapply(seq_len(nrow(tab)), function(r) {
      if (is.na(tab$surv[r]))
        sprintf("%s @ t=%g: not estimable (beyond follow-up)",
                lab(tab$group[r]), tab$time[r])
      else
        sprintf("%s @ t=%g: %s (%d%% CI %s to %s)",
                lab(tab$group[r]), tab$time[r], pc(tab$surv[r]), cl,
                pc(tab$lower[r]), pc(tab$upper[r]))
    }, character(1))
  }
  if (!"caption" %in% names(dots))
    gg <- gg + ggplot2::labs(caption = paste(lines, collapse = "\n"))

  gg <- gg + ggplot2::theme(
    plot.subtitle = ggplot2::element_text(size = 11, colour = "grey30"),
    plot.caption  = ggplot2::element_text(size = 10, colour = "grey30", hjust = 0))

  attr(gg, "milestone.table") <- milestone.table
  if (inherits(p, "ggsurvplot")) { p$plot <- gg; p } else gg
}
