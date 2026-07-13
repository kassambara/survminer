#' @include utilities.R
NULL
#' Median Follow-Up Time (Reverse Kaplan-Meier)
#'
#' @description Estimates the median follow-up time using the reverse
#'   Kaplan-Meier method (Schemper & Smith, 1996), also called the "potential"
#'   follow-up time. The roles of the event and censoring indicators are swapped
#'   -- censored subjects are treated as the events -- and the median of the
#'   resulting Kaplan-Meier curve is reported, per group.
#'
#'   This is the follow-up counterpart of \code{\link{surv_median}()}: the
#'   latter returns the median \emph{survival} time, this returns the median
#'   \emph{follow-up} time. Reporting the median follow-up is standard practice
#'   in clinical publications because the naive median of the observed times
#'   under-estimates follow-up (deaths stop a subject's follow-up early).
#'
#' @details The estimate is computed entirely from base \code{survival}: the
#'   curve is refit on the reversed event indicator (each subject's event/censor
#'   status flipped) with the same grouping as \code{fit}, and the median (with
#'   confidence limits) is read from its summary. The status is read from the
#'   fit's \code{Surv} response, so any right-censored coding works (0/1, 1/2,
#'   or logical). The confidence limits are the standard Kaplan-Meier limits at
#'   the fit's own confidence level and interval type; they are \code{NA} when
#'   the reverse curve does not drop below the relevant bound (e.g. a small group
#'   whose upper limit is not reached, or a fit built with
#'   \code{conf.type = "none"}). Only right-censored \code{Surv(time, status)}
#'   data are supported.
#'
#' @param fit a survfit object (\code{survival::survfit()} or survminer's
#'   \code{\link{surv_fit}()}) built from right-censored \code{Surv(time,
#'   status)} data.
#' @param data the data frame used to fit the survival curves. If not supplied
#'   it is extracted from \code{fit}.
#' @param conf.int the confidence level for the limits. Default \code{NULL} uses
#'   the confidence level the \code{fit} was built with (95\% by default). The
#'   fit's confidence-interval type (\code{conf.type}) is honoured as well.
#' @return A data frame with one row per group and the columns: \itemize{ \item
#'   strata: group name (\code{"All"} for an ungrouped fit) \item median: median
#'   follow-up time \item lower: lower confidence limit \item upper: upper
#'   confidence limit }
#' @references Schemper M, Smith TL (1996). A note on quantifying follow-up in
#'   studies of failure time. \emph{Controlled Clinical Trials} 17(4):343-346.
#' @seealso \code{\link{surv_median}()}
#' @examples
#' library(survival)
#'
#' # Grouped fit: median follow-up per sex
#' fit <- surv_fit(Surv(time, status) ~ sex, data = lung)
#' surv_median_followup(fit)
#'
#' # Contrast with the median survival time (a different quantity)
#' surv_median(fit)
#'
#' # Ungrouped fit: overall median follow-up
#' fit.null <- surv_fit(Surv(time, status) ~ 1, data = lung)
#' surv_median_followup(fit.null)
#' @export
surv_median_followup <- function(fit, data = NULL, conf.int = NULL) {

  if (!.is_survfit(fit))
    stop("`fit` must be a survfit object.", call. = FALSE)

  # Extract the data from the fit when not supplied. Quiet by default (complain =
  # FALSE) so a fit-only call matches surv_median()'s silence; .get_data() still
  # errors clearly if the data genuinely cannot be recovered from the fit.
  data <- as.data.frame(.get_data(fit, data, complain = FALSE))
  f <- stats::formula(fit)
  resp <- eval(f[[2]], envir = data)
  # Right-censored data only. A competing-risks / multi-state Surv is
  # `type = "mright"` (also 2 columns) and reversing its collapsed status codes
  # would be meaningless; left-censored likewise.
  if (!survival::is.Surv(resp) || ncol(resp) != 2L ||
      !identical(attr(resp, "type"), "right"))
    stop("surv_median_followup() supports right-censored `Surv(time, status)` data only.",
         call. = FALSE)

  # Confidence limits: honour the original fit's confidence level (unless
  # overridden) and its interval type, so the reported limits are consistent with
  # how the fit was built (e.g. conf.type = "none" -> NA limits).
  cl <- if (!is.null(conf.int)) conf.int
        else if (!is.null(fit$conf.int)) fit$conf.int
        else 0.95
  ct <- if (!is.null(fit$conf.type)) fit$conf.type else "log"

  # Reverse the event indicator (censored -> "event"); resp[, 2] is already the
  # 0/1-normalised status, so this works for any right-censored coding (0/1, 1/2,
  # logical). Refit the Kaplan-Meier on the reversed indicator with the SAME
  # grouping as the original fit, so strata are reproduced exactly. The reversed
  # time/status go in columns with names that cannot clash with a user column
  # (backticked in the formula so any generated name is valid).
  df <- data
  uniq <- function(base) { nm <- base; while (nm %in% names(df)) nm <- paste0(nm, "_"); nm }
  tcol <- uniq(".rkm_time"); rcol <- uniq(".rkm_rev")
  df[[tcol]] <- as.numeric(resp[, 1])
  df[[rcol]] <- 1 - as.numeric(resp[, 2])
  # collapse in case deparse returns a multi-line vector for a very wide RHS
  rhs <- paste(deparse(f[[3]], width.cutoff = 500L), collapse = "")
  revfit <- survival::survfit(
    stats::as.formula(sprintf("survival::Surv(`%s`, `%s`) ~ %s", tcol, rcol, rhs)),
    data = df, conf.int = cl, conf.type = ct)

  .followup_table(revfit)
}


# Tidy the median + confidence limits out of a (reverse-KM) survfit summary into
# a data frame with strata/median/lower/upper -- mirroring surv_median()'s
# handling: an ungrouped fit yields a one-row "All" table, and the confidence
# limits are detected by their "LCL"/"UCL" suffix (robust to a non-default
# confidence level and to a fit stored without limits).
.followup_table <- function(fit) {
  if (!is.null(fit$strata) || is.matrix(fit$surv)) {
    tab <- as.data.frame(summary(fit)$table)
  } else {
    tab <- as.data.frame(t(as.data.frame(summary(fit)$table)))
    rownames(tab) <- "All"
  }
  tab$strata <- rownames(tab)

  lcl <- grep("LCL$", colnames(tab), value = TRUE)
  ucl <- grep("UCL$", colnames(tab), value = TRUE)
  tab$lower <- if (length(lcl) >= 1L) tab[[lcl[1]]] else NA_real_
  tab$upper <- if (length(ucl) >= 1L) tab[[ucl[1]]] else NA_real_

  tab <- tab[, c("strata", "median", "lower", "upper")]
  rownames(tab) <- NULL
  tab
}
