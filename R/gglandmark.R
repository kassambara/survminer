#' @include utilities.R
NULL
#' Landmark Analysis of Survival Curves
#'
#' @description
#' Draws Kaplan-Meier curves \strong{re-origined at a landmark time} \code{L}: the
#' analysis is restricted to subjects still event-free and under observation at
#' \code{L}, the time axis is reset so that \code{L} becomes time 0, and the curves
#' are re-estimated from the landmark onward.
#'
#' Landmark analysis is the standard remedy for \strong{immortal-time (guarantee-time)
#' bias} that arises when groups are defined by a post-baseline status (e.g. tumour
#' response, transplantation): comparing such groups from study baseline credits the
#' responders with the event-free time they were guaranteed to accrue \emph{before}
#' they could be classified. Conditioning on being event-free at a fixed landmark
#' removes that bias (Anderson, Cain & Gelber, 1983; Dafni, 2011).
#'
#' @details
#' The landmark cohort is every subject with \code{time >= landmark.time} (still at
#' risk at \code{L}). Each retained subject's clock is reset to
#' \code{time - landmark.time} with its event status unchanged, and the
#' Kaplan-Meier curves are re-fitted on this re-origined data via
#' \code{\link{surv_fit}()} and drawn with \code{\link{ggsurvplot}()}. Because the
#' cohort is event-free at \code{L}, every curve correctly starts at \eqn{S = 1}; a
#' subject whose \emph{event} falls exactly on \code{L} has, by definition, already
#' had the event by the landmark and is excluded (reported in a message and the
#' subtitle) so the re-origined curve is not pulled below 1 at time 0.
#'
#' Any log-rank p-value or table you request through \code{...} (e.g.
#' \code{pval = TRUE}, \code{risk.table = TRUE}) is computed on the re-origined
#' landmark cohort, as it must be for a valid landmark comparison.
#'
#' Caveats. Landmark analysis discards subjects with an event or censoring before
#' \code{L} (a loss of power) and is sensitive to the -- necessarily arbitrary --
#' choice of \code{L}; reporting a sensitivity analysis over several landmarks is
#' good practice (Dafni, 2011). The immortal-time-bias correction applies when the
#' grouping variable reflects a status attained up to \code{L}; for a fixed baseline
#' covariate there is no immortal time to remove.
#'
#' Groups are taken from the fit's own strata, so the labels and their order match
#' \code{names(fit$strata)}, and a transformed right-hand side such as
#' \code{~ (age > 60)} is honoured as written (an ungrouped \code{~ 1} fit has no
#' strata and is treated as a single group). Note that the landmark curves are a
#' \emph{refit} on the landmark cohort, so a data-dependent grouping term such as
#' \code{cut(age, 3)} recomputes its breaks on the survivors; bin such a variable
#' before fitting if the strata must match the original figure exactly.
#' A weighted \code{survfit()} is
#' refused, since the landmark cohort would have to be re-fitted and the
#' confidence-interval convention for weighted data is ambiguous.
#'
#' @param fit a survfit object (\code{survival::survfit()} or survminer's
#'   \code{\link{surv_fit}()}).
#' @param data the data frame used to fit the curves. Strongly recommended: it is
#'   required to rebuild the landmark cohort and can only be recovered automatically
#'   for a fit created in the global environment (see \code{\link{ggsurvplot}}).
#' @param landmark.time the landmark time \code{L} (in the data's time units) at
#'   which the clock is reset. Required.
#' @param conf.int logical; draw the confidence-interval band of the re-origined
#'   curves. Default \code{FALSE} (matching \code{\link{ggsurvplot}()}). Because the
#'   curves are re-fitted on the landmark cohort, the band is drawn at
#'   \code{survfit()}'s default 95\% level rather than the level \code{fit} was
#'   built with.
#' @param xlab x-axis label. Default \code{"Time since landmark"} to make the
#'   re-origining explicit.
#' @param ... other arguments passed to \code{\link{ggsurvplot}()} (e.g.
#'   \code{palette}, \code{legend.labs}, \code{pval}, \code{risk.table}). A
#'   \code{subtitle} supplied here is respected; otherwise a subtitle naming the
#'   landmark and cohort size is added.
#'
#' @return a \code{ggsurvplot} object (the re-origined curves, plus any requested
#'   risk table). The landmark details -- \code{landmark.time}, the number at risk,
#'   the number of events dropped at the landmark, and the re-origined \code{fit} --
#'   are attached as \code{attr(x$plot, "landmark")}.
#'
#' @references
#' Anderson JR, Cain KC, Gelber RD (1983). Analysis of survival by tumor response.
#' \emph{Journal of Clinical Oncology} 1(11):710-719.
#'
#' Dafni U (2011). Landmark analysis at the 25-year landmark point.
#' \emph{Circulation: Cardiovascular Quality and Outcomes} 4(3):363-371.
#'
#' @seealso \code{\link{ggmilestone}}, \code{\link{ggsurvplot}}, \code{\link{ggrmst}}.
#'
#' @examples
#' library(survival)
#' fit <- survfit(Surv(time, status) ~ sex, data = lung)
#'
#' # Re-origin the curves at day 200 (only day-200 survivors, clock reset)
#' gglandmark(fit, data = lung, landmark.time = 200)
#'
#' # With a log-rank p-value and risk table on the landmark cohort
#' gglandmark(fit, data = lung, landmark.time = 200,
#'            pval = TRUE, risk.table = TRUE, palette = "jco")
#'
#' @export
gglandmark <- function(fit, data = NULL, landmark.time, conf.int = FALSE,
                       xlab = "Time since landmark", ...) {

  if (!.is_survfit(fit))
    stop("`fit` must be a survfit object.", call. = FALSE)
  if (missing(landmark.time) || !is.numeric(landmark.time) ||
      length(landmark.time) != 1L || is.na(landmark.time) || landmark.time < 0)
    stop("`landmark.time` must be a single non-negative number.", call. = FALSE)
  # conf.int is the band flag here, as in every sibling; reject the numeric
  # spelling rather than let a confidence level read as TRUE.
  if (!is.logical(conf.int) || length(conf.int) != 1L || is.na(conf.int))
    stop("`conf.int` must be TRUE or FALSE (it draws the confidence band around ",
         "the curves), at the confidence level `fit` was built with.", call. = FALSE)
  L <- landmark.time

  ext <- .km_reorigin_extract(fit, data)
  data <- ext$data
  time <- ext$time; status <- ext$status

  if (L >= max(time, na.rm = TRUE))
    stop("`landmark.time` (", L, ") is at or beyond the last observed time (",
         round(max(time, na.rm = TRUE), 4), "); no subjects remain at risk.",
         call. = FALSE)

  # Landmark cohort: event-free and still under observation at L (time >= L). A
  # subject whose EVENT is exactly at L has had the event by the landmark, so it is
  # excluded -- otherwise it enters as a death at re-origined time 0 and drags the
  # curve below S = 1 at the landmark (Dafni 2011; van Houwelingen & Putter 2012).
  atrisk     <- time >= L & !is.na(time)
  event_at_L <- atrisk & time == L & status == 1
  keep       <- atrisk & !event_at_L
  n.drop.ev  <- sum(event_at_L, na.rm = TRUE)
  if (!any(keep))
    stop("No subjects are event-free and at risk at landmark.time = ", L, ".",
         call. = FALSE)

  d.lm <- data[keep, , drop = FALSE]
  # Re-origined time/status go in columns that cannot clash with a user column of
  # the same name (backticked in the formula so any generated name stays valid),
  # mirroring surv_median_followup().
  uniq <- function(base) { nm <- base; while (nm %in% names(d.lm)) nm <- paste0(nm, "_"); nm }
  tcol <- uniq(".landmark_time"); scol <- uniq(".landmark_status")
  d.lm[[tcol]] <- time[keep] - L
  d.lm[[scol]] <- status[keep]

  form <- stats::as.formula(sprintf("survival::Surv(`%s`, `%s`) ~ %s",
                                    tcol, scol, ext$rhs))
  fit.lm <- surv_fit(form, data = d.lm)

  n.risk <- sum(keep)
  dots <- list(...)
  args <- list(fit = fit.lm, data = d.lm, conf.int = conf.int, xlab = xlab)
  if (!"subtitle" %in% names(dots)) {
    sub <- paste0("Landmark at t = ", L, "  (n = ", n.risk,
                  " at risk at the landmark, shown at time 0 below",
                  if (n.drop.ev) paste0("; ", n.drop.ev,
                                        " event(s) at the landmark excluded") else "",
                  ")")
    args$subtitle <- sub
  }
  p <- do.call(ggsurvplot, c(args, dots))
  if (inherits(p, "ggsurvplot"))
    p$plot <- p$plot + ggplot2::theme(
      plot.subtitle = ggplot2::element_text(size = 9.5, colour = "grey30"))

  info <- list(landmark.time = L, n.at.risk = n.risk,
               n.dropped.event = n.drop.ev, fit = fit.lm)
  if (inherits(p, "ggsurvplot")) attr(p$plot, "landmark") <- info
  else attr(p, "landmark") <- info

  message(n.risk, " subject(s) at risk at landmark.time = ", L,
          if (n.drop.ev) paste0("; ", n.drop.ev,
                                " with an event exactly at the landmark excluded")
          else "", ".")
  p
}


# ---- shared internal: raw (time, status, grouping) from a survfit + data -------
# Mirrors ggrmst's extractor: right-censored Surv only, grouping from the formula
# RHS, missing rows dropped. Reused by gglandmark() and ggmilestone().
.km_reorigin_extract <- function(fit, data, fn = "gglandmark") {
  if (!.is_survfit(fit))
    stop("`fit` must be a survfit object.", call. = FALSE)
  .stop_if_weighted(fit, fn)
  data <- as.data.frame(.get_data(fit, data))
  f <- stats::formula(fit)
  resp <- eval(f[[2]], envir = data)
  if (!survival::is.Surv(resp) || ncol(resp) != 2L ||
      !identical(attr(resp, "type"), "right"))
    stop("Only right-censored `Surv(time, status)` data are supported.",
         call. = FALSE)
  time <- as.numeric(resp[, 1]); status <- as.numeric(resp[, 2])
  # The right-hand side verbatim (not all.vars(), and not the term labels): a
  # transformed term such as `~ (age > 60)` must be re-fitted exactly as written.
  # Term labels drop the parentheses, and reformulate() would paste them with `+`,
  # so `~ sexf + (age > 60)` would re-parse as `~ (sexf + age) > 60`.
  # collapse in case deparse returns several lines for a very wide right-hand side
  rhs <- paste(deparse(f[[3]], width.cutoff = 500L), collapse = "")
  # Grouping from the fit's own strata convention -- see .strata_group_from_formula().
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
       data = data[ok, , drop = FALSE], rhs = rhs, formula = f)
}
