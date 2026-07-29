#' @include utilities.R
NULL
#' Restricted Mean Survival Time (RMST)
#'
#' @description
#' Restricted mean survival time (RMST) is the area under the Kaplan-Meier curve
#' up to a truncation time \code{tau} -- the average event-free time over
#' \code{[0, tau]}, in the original time units. It is increasingly reported
#' alongside the hazard ratio because it stays interpretable under
#' non-proportional hazards (ICH E9(R1)).
#'
#' \code{ggrmst_difference()} returns a tidy table of the per-group RMST (with
#' standard error and confidence interval) and, for a two-group comparison, the
#' difference in RMST with its confidence interval and p-value.
#'
#' \code{ggrmst()} draws the Kaplan-Meier curves with the RMST region shaded: for
#' two groups the region \emph{between} the curves up to \code{tau} is highlighted
#' and the RMST difference annotated (that region's signed area is the difference;
#' when the curves cross, the shaded area is the total separation and the reported
#' difference -- always the exact signed value -- can be smaller); otherwise the
#' area under each curve is shaded. A vertical line marks \code{tau}.
#'
#' @details
#' The estimate is computed internally from the Kaplan-Meier curve (no external
#' dependency): \eqn{RMST(\tau) = \int_0^\tau S(u)\,du} by exact integration of the
#' KM step function (held flat from the last observed time to \code{tau}), with the
#' analytic variance \eqn{\sum_{t_i \le \tau} A_i^2\, d_i / (Y_i (Y_i - d_i))} where
#' \eqn{A_i = \int_{t_i}^{\tau} S(u)\,du}, \eqn{d_i} events and \eqn{Y_i} at risk.
#' The difference in RMST uses the sum of the group variances and a normal
#' confidence interval / z-test. Results match \code{survRM2::rmst2()} (Uno et al.);
#' see Royston & Parmar (2013) for the methodology.
#'
#' By default \code{tau} is the largest time at which every group's Kaplan-Meier
#' estimate is still defined: a group whose last observation is an event places no
#' upper limit (its curve is defined to be 0 afterwards), while a group whose last
#' observation is censored limits \code{tau} to that time. A \code{tau} beyond that
#' admissible range is an error.
#'
#' Groups are taken from the fit's own strata, so the labels and their order match
#' \code{names(fit$strata)} and the curves drawn by \code{\link{ggsurvplot}()}, and a
#' transformed right-hand side such as \code{~ (age > 60)} is honoured as written.
#' (An ungrouped \code{~ 1} fit has no strata and is reported as a single
#' \code{"All"} group. The grouping is rebuilt from \code{data}, so a fit created
#' with \code{survfit(..., subset =)} should be passed the same subset of rows.)
#' A weighted \code{survfit()} is refused: the confidence-interval convention for
#' weighted data is ambiguous, so reporting an interval for it would overstate what
#' the estimate supports.
#'
#' @param fit a survfit object (\code{survival::survfit()} or survminer's
#'   \code{\link{surv_fit}()}).
#' @param data the data frame used to fit the survival curves. If not supplied it
#'   is extracted from \code{fit}.
#' @param tau numeric truncation time. Default \code{NULL} uses the largest time at
#'   which all groups' curves are defined (see Details). An out-of-range value is an
#'   error.
#' @param conf.level the confidence level for the RMST intervals, a single number in
#'   (0, 1). Default 0.95. (\code{ggrmst_difference()} returns a table, so this is
#'   its only confidence argument.)
#' @param conf.int logical; draw the confidence band around the survival curves,
#'   as in \code{\link{ggmilestone}()} and \code{\link{gglandmark}()}. Default
#'   \code{FALSE}. \code{ggrmst()} only. The band is drawn at the confidence level
#'   \code{fit} was built with; \code{conf.level} sets the level of the RMST
#'   interval, not of the band.
#' @param ref.group for three or more groups, the reference group the RMST
#'   differences are taken against, given in the fit's own label form (e.g.
#'   \code{"sex=1"}; the error message lists the valid values). Default \code{NULL} reports only
#'   the per-group RMST (no differences). Ignored for one or two groups.
#' @param palette,ggtheme,... passed to \code{\link{ggsurvplot}()} for the underlying
#'   curves.
#' @return \code{ggrmst_difference()} returns a data.frame with one row per group
#'   (\code{strata}, \code{rmst}, \code{se}, \code{lower}, \code{upper}, \code{tau})
#'   and, when a two-group (or \code{ref.group}) difference is defined, additional
#'   \code{"... - ..."} rows carrying \code{rmst} (the difference), \code{se},
#'   \code{lower}, \code{upper} and \code{p.value}. The \code{strata} column holds
#'   the fit's own labels, as \code{\link{surv_median}()} and
#'   \code{\link{surv_summary}()} do; a difference row is labelled with its contrast
#'   and is the only kind of row with a non-\code{NA} \code{p.value}.
#'   \code{ggrmst()} returns a ggplot.
#' @references
#' Royston P, Parmar MKB (2013). Restricted mean survival time: an alternative to
#' the hazard ratio for the design and analysis of randomized trials with a
#' time-to-event outcome. \emph{BMC Medical Research Methodology} 13:152.
#' @examples
#' library(survival)
#' fit <- survfit(Surv(time, status) ~ sex, data = lung)
#' ggrmst_difference(fit, data = lung)
#' ggrmst(fit, data = lung, palette = "jco")
#'
#' @describeIn ggrmst_difference a tidy table of per-group RMST and the difference.
#' @export
ggrmst_difference <- function(fit, data = NULL, tau = NULL, conf.level = 0.95,
                              ref.group = NULL) {
  if (!is.numeric(conf.level) || length(conf.level) != 1L || is.na(conf.level) ||
      conf.level <= 0 || conf.level >= 1)
    stop("`conf.level` must be a single number in (0, 1).", call. = FALSE)
  ext <- .rmst_extract(fit, data, fn = "ggrmst_difference")
  tau <- .rmst_resolve_tau(ext$time, ext$status, ext$group, tau)
  alpha <- 1 - conf.level
  glevels <- levels(ext$group)

  per <- lapply(glevels, function(g) {
    i <- ext$group == g
    .rmst_one_arm(ext$time[i], ext$status[i], tau, alpha)
  })
  names(per) <- glevels
  z <- stats::qnorm(1 - alpha / 2)

  out <- data.frame(
    strata = glevels,
    rmst  = vapply(per, function(x) x[["rmst"]], numeric(1)),
    se    = vapply(per, function(x) x[["se"]],  numeric(1)),
    lower = vapply(per, function(x) x[["lower"]], numeric(1)),
    upper = vapply(per, function(x) x[["upper"]], numeric(1)),
    tau   = tau, row.names = NULL, stringsAsFactors = FALSE)

  # Which pairwise differences to report: both groups for k = 2; vs ref.group for
  # k >= 3 when a reference is named; none otherwise (a difference is pairwise, so
  # inventing a reference silently would be dishonest).
  diffs <- NULL
  if (length(glevels) == 2L) {
    diffs <- list(c(glevels[2], glevels[1]))
  } else if (length(glevels) >= 3L && !is.null(ref.group)) {
    if (!ref.group %in% glevels)
      stop("`ref.group` must be one of the strata labels: ",
           paste(glevels, collapse = ", "), call. = FALSE)
    others <- setdiff(glevels, ref.group)
    diffs <- lapply(others, function(g) c(g, ref.group))
  }

  if (!is.null(diffs)) {
    drows <- lapply(diffs, function(pr) {
      a <- per[[pr[1]]]; b <- per[[pr[2]]]
      d  <- a[["rmst"]] - b[["rmst"]]
      sd <- sqrt(a[["var"]] + b[["var"]])
      data.frame(strata = paste(pr[1], "-", pr[2]),
                 rmst = d, se = sd, lower = d - z * sd, upper = d + z * sd,
                 tau = tau, p.value = 2 * stats::pnorm(-abs(d / sd)),
                 row.names = NULL, stringsAsFactors = FALSE)
    })
    out$p.value <- NA_real_
    out <- rbind(out, do.call(rbind, drows))
  }
  out
}


#' @describeIn ggrmst_difference the shaded Kaplan-Meier RMST plot.
#' @export
ggrmst <- function(fit, data = NULL, tau = NULL, conf.level = 0.95,
                   palette = NULL, ggtheme = theme_survminer(), ...,
                   conf.int = FALSE) {
  # conf.int is the band flag, as everywhere else in survminer. Catch the stale
  # numeric spelling rather than let ggsurvplot() read 0.9 as "TRUE" while the
  # RMST interval silently stays at conf.level.
  if (!is.logical(conf.int) || length(conf.int) != 1L || is.na(conf.int))
    stop("`conf.int` must be TRUE or FALSE (it draws the confidence band around ",
         "the curves). Use `conf.level` to set the confidence level of the RMST ",
         "interval.", call. = FALSE)
  ext <- .rmst_extract(fit, data)
  tau <- .rmst_resolve_tau(ext$time, ext$status, ext$group, tau)
  glevels <- levels(ext$group)
  tab <- ggrmst_difference(fit, data = ext$data, tau = tau, conf.level = conf.level)

  # Bare, composable survival-curve ggplot (output = "ggplot"). No table -- RMST is a single plot.
  p <- ggsurvplot(fit, data = ext$data, palette = palette, ggtheme = ggtheme,
                  output = "ggplot", conf.int = conf.int, ...)

  # The plot's own strata levels, in the same order as glevels (ggsurvplot may
  # rename them via legend.labs). Used for every label we draw, so the annotation
  # never names an arm the legend does not show.
  plot.strata <- levels(p$data$strata)
  remap <- if (length(plot.strata) == length(glevels))
    stats::setNames(plot.strata, glevels) else stats::setNames(glevels, glevels)

  time <- surv <- NULL
  if (length(glevels) == 2L) {
    # Shade the area BETWEEN the two step curves up to tau: its area is the RMST
    # difference. Built from the step vertices so it follows the KM exactly.
    band <- .rmst_between_band(ext, tau)
    p <- p +
      ggplot2::geom_ribbon(data = band,
                           ggplot2::aes(x = time, ymin = ymin, ymax = ymax),
                           inherit.aes = FALSE, fill = "grey50", alpha = 0.3)
    # The difference rows are the ones appended after the per-group block; select
    # them by position, never by matching " - " in the label, since a group label
    # may itself contain " - ".
    dr <- tab[nrow(tab), , drop = FALSE]
    contrast <- paste(remap[[glevels[2]]], "-", remap[[glevels[1]]])
    lab <- sprintf("Delta RMST (%s) = %.1f  (%.0f%% CI %.1f to %.1f)\np = %s",
                   contrast, dr$rmst, conf.level * 100, dr$lower, dr$upper,
                   format.pval(dr$p.value, digits = 2, eps = 1e-4))
    p <- p + ggplot2::labs(subtitle = lab)
  } else {
    # One or 3+ groups: shade the area under each curve up to tau (faceted for 3+).
    # Relabel the area's group to the plot's own strata levels (same order) so a
    # facet by strata matches the curves.
    areas <- .rmst_under_band(ext, tau)
    if (length(plot.strata) == length(glevels))
      areas$strata <- factor(remap[areas$strata], levels = plot.strata)
    p <- p +
      ggplot2::geom_ribbon(data = areas,
                           ggplot2::aes(x = time, ymin = 0, ymax = surv,
                                        fill = strata, group = strata),
                           inherit.aes = FALSE, alpha = 0.2, show.legend = FALSE)
    if (length(glevels) >= 3L)
      p <- p + ggplot2::facet_wrap(~strata)
  }

  # tau reference line, always visible and labelled.
  p +
    ggplot2::geom_vline(xintercept = tau, linetype = "dashed", colour = "grey30") +
    ggplot2::annotate("text", x = tau, y = 1, label = paste0("tau = ", round(tau, 1)),
                      hjust = 1.05, vjust = 1, size = 3.2, colour = "grey30")
}


# ---- internal estimator (validated to machine precision vs survRM2) ----------

# Per-arm RMST and variance to tau from raw (time, status), flat-forwarding the KM
# from the last observed time to tau.
.rmst_one_arm <- function(time, status, tau, alpha = 0.05) {
  f <- survival::survfit(survival::Surv(time, status) ~ 1)
  tt <- f$time; Y <- f$n.risk; d <- f$n.event; S <- f$surv
  keep <- tt <= tau
  tt <- tt[keep]; Y <- Y[keep]; d <- d[keep]; S <- S[keep]
  gt <- c(0, tt, tau)
  # Step value on each left segment; flat to tau. When no observation falls at or
  # before tau, S is empty and S[length(S)] is numeric(0) -- which would collapse gS
  # to length 1 and make the sum below 0 instead of tau. The curve is identically 1
  # on [0, tau] in that case, so the flat-forward value is 1.
  gS <- c(1, S, if (length(S)) S[length(S)] else 1)
  w  <- diff(gt)
  Sv <- gS[-length(gS)]
  rmst <- sum(Sv * w)
  seg  <- Sv * w
  A.start <- rev(cumsum(rev(seg)))    # area from each grid start to tau
  A.i <- A.start[match(tt, gt)]       # A_i = area from event time t_i to tau
  var <- sum(ifelse(d > 0 & Y > d, A.i^2 * d / (Y * (Y - d)), 0))
  se <- sqrt(var)
  z <- stats::qnorm(1 - alpha / 2)
  c(rmst = rmst, se = se, var = var, lower = rmst - z * se, upper = rmst + z * se)
}

# Default / validated tau: the largest time at which all groups' KM is defined.
.rmst_resolve_tau <- function(time, status, group, tau = NULL) {
  by <- split(seq_along(time), group)
  # per-group admissible upper bound: Inf if the last observation is an event, else
  # the last observation time (the curve is undefined beyond a final censoring).
  bnds <- vapply(by, function(i) {
    # A group's last observation limits tau only if it is (partly) censored: the
    # curve is defined beyond max(t) only when every observation there is an event
    # (KM reaches 0). This mirrors survRM2's min(status at max) rule, incl. the case
    # where the max time carries both an event and a censoring (-> censored-ending).
    tt <- time[i]; st <- status[i]; m <- max(tt)
    if (all(st[tt == m] == 1)) Inf else m
  }, numeric(1))
  tau.max <- if (all(is.infinite(bnds))) max(time) else min(bnds[is.finite(bnds)])
  if (is.null(tau)) return(tau.max)
  if (!is.numeric(tau) || length(tau) != 1L || is.na(tau) || tau <= 0)
    stop("`tau` must be a single positive number.", call. = FALSE)
  if (tau > tau.max)
    stop("`tau` (", tau, ") is beyond the range where every group's survival curve ",
         "is defined; it must be <= ", round(tau.max, 4), ".", call. = FALSE)
  tau
}

# Extract raw time, status and the grouping (combined strata) from fit + data.
.rmst_extract <- function(fit, data, fn = "ggrmst") {
  if (!.is_survfit(fit))
    stop("`fit` must be a survfit object.", call. = FALSE)
  .stop_if_weighted(fit, fn)
  data <- as.data.frame(.get_data(fit, data))
  f <- stats::formula(fit)
  resp <- eval(f[[2]], envir = data)
  # Right-censored data only. A competing-risks / multi-state Surv is `type =
  # "mright"` (also 2 columns, so an ncol check alone lets it through) and would give
  # a meaningless RMST from the collapsed status codes; left-censored likewise.
  if (!survival::is.Surv(resp) || ncol(resp) != 2L ||
      !identical(attr(resp, "type"), "right"))
    stop(fn, "() supports right-censored `Surv(time, status)` data only.",
         call. = FALSE)
  time <- as.numeric(resp[, 1]); status <- as.numeric(resp[, 2])
  # Grouping straight from the fit's own strata convention: the levels reproduce
  # names(fit$strata) in survfit's order, so the label remap onto the plotted
  # curves is correct by construction and a transformed right-hand side such as
  # `~ (age > 60)` groups into its two strata rather than one per distinct value.
  group <- .strata_group_from_formula(f, data)
  # survfit() stores no strata when the formula collapses to a single group (e.g.
  # a factor with one used level); the package calls that "All" everywhere else.
  # Rename the LEVELS only -- replacing the vector would overwrite the NAs that
  # reproduce survfit()'s na.omit, and the estimates would then be computed on
  # rows the fit excluded.
  if (is.null(fit$strata) && nlevels(group) > 0L)
    levels(group) <- rep("All", nlevels(group))
  ok <- stats::complete.cases(time, status) & !is.na(group)
  list(time = time[ok], status = status[ok], group = droplevels(group[ok]),
       data = data)
}

# Step vertices of the KM band under a curve (ymin=0, ymax=S) up to tau, per group.
.rmst_under_band <- function(ext, tau) {
  do.call(rbind, lapply(levels(ext$group), function(g) {
    i <- ext$group == g
    f <- survival::survfit(survival::Surv(ext$time[i], ext$status[i]) ~ 1)
    st <- .km_steps(f$time, f$surv, tau)
    data.frame(time = st$time, surv = st$surv, strata = g,
               row.names = NULL, stringsAsFactors = FALSE)
  }))
}

# Ribbon between two groups' step curves up to tau (ymin = lower curve, ymax = upper).
# Built from explicit step vertices so it follows both KM curves exactly (two rows
# per interval at the interval's constant value; the shared x at each change point
# gives the vertical drop).
.rmst_between_band <- function(ext, tau) {
  gl <- levels(ext$group)
  km <- lapply(gl[1:2], function(g) {
    i <- ext$group == g
    f <- survival::survfit(survival::Surv(ext$time[i], ext$status[i]) ~ 1)
    keep <- f$time <= tau
    list(t = c(0, f$time[keep]), s = c(1, f$surv[keep]))  # value on [t_j, t_{j+1}) = s[j]
  })
  cps <- sort(unique(c(km[[1]]$t, km[[2]]$t, tau)))
  cps <- cps[cps <= tau]
  val <- function(k, x) k$s[findInterval(x, k$t)]   # right-continuous value at x
  do.call(rbind, lapply(seq_len(length(cps) - 1L), function(i) {
    xl <- cps[i]; xr <- cps[i + 1L]
    v1 <- val(km[[1]], xl); v2 <- val(km[[2]], xl)
    data.frame(time = c(xl, xr), ymin = pmin(v1, v2), ymax = pmax(v1, v2),
               row.names = NULL)
  }))
}

# Build left-continuous step (time, surv) vertices from 0 to tau (duplicated x at
# each drop so geom_ribbon/geom_step render the true step function).
.km_steps <- function(t, s, tau) {
  keep <- t <= tau
  t <- c(0, t[keep], tau); s <- c(1, s[keep], s[keep][sum(keep)])
  if (sum(keep) == 0L) s <- c(1, 1)
  # duplicate vertices to make horizontal-then-vertical steps
  n <- length(t)
  tx <- rep(t, each = 2)[-1]
  sx <- rep(s, each = 2)[-(2 * n)]
  list(time = tx, surv = sx)
}
