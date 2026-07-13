#' Analysis-Ready Survival Data from a CDISC ADTTE Dataset
#'
#' @description Prepares a CDISC ADaM time-to-event dataset (\code{ADTTE}) for
#'  survival analysis, guarding against the classic censoring-flip error. In
#'  \code{ADTTE} the censoring flag \code{CNSR} is coded \strong{0 = event,
#'  \eqn{\ge 1} = censored} (values 1-4 give the censoring reason) -- the opposite
#'  of \code{\link[survival]{Surv}()}, which wants \code{1 = event}. Passing
#'  \code{CNSR} straight into \code{Surv()} silently inverts every result.
#'  \code{surv_adtte()} derives the correct event indicator
#'  (\code{event = as.integer(CNSR == 0)}), filters to one analysis parameter,
#'  validates the input, and returns the data with an \code{event} column ready for
#'  \code{\link{surv_fit}()} / \code{\link{ggsurvplot}()}.
#'
#'  Note the derivation \code{CNSR == 0} (not \code{1 - CNSR}): the latter is only
#'  correct for binary \code{CNSR} and breaks for the standard multi-valued
#'  censoring-reason codes.
#'
#' @param data an \code{ADTTE} data frame.
#' @param paramcd the analysis parameter to keep, matched against the \code{param}
#'  column (e.g. \code{"OS"}, \code{"PFS"}). If \code{NULL} and several parameters
#'  are present, an error is raised (a time-to-event analysis is per parameter).
#' @param cnsr,aval,param column names of the censoring flag, analysis value (time)
#'  and parameter code. Defaults are the ADaM standard \code{"CNSR"}, \code{"AVAL"},
#'  \code{"PARAMCD"}.
#' @param subject column name of the subject identifier, used only to warn about
#'  duplicate records (\code{ADTTE} has one record per subject per parameter).
#'  Default \code{"USUBJID"}; ignored if absent.
#' @param event.name name of the 0/1 event column to add. Default \code{"event"}.
#' @param event.ref optional name of an existing event-indicator column
#'  (\code{1 = event}) to cross-check the derivation against; a warning is issued
#'  when the derived event disagrees with it (catches an already-flipped or
#'  nonstandard coding). There is no standard \code{ADTTE} event flag, so this is
#'  opt-in.
#' @param overwrite logical. If \code{event.name} already exists in \code{data},
#'  overwrite it (\code{TRUE}) or error (\code{FALSE}, default).
#'
#' @return \code{data}, filtered to \code{paramcd} and with the \code{event} column
#'  added (rows in input order, row names reset). The event and censoring counts are
#'  attached as \code{attr(x, "adtte")} and reported in a message.
#'
#' @seealso \code{\link{surv_fit}}, \code{\link{ggsurvplot}}. The
#'  \code{ggsurvfit::Surv_CNSR()} and \code{visR::Surv_CNSR()} functions build a
#'  \code{Surv} object from \code{CNSR}; \code{surv_adtte()} instead returns a tidy
#'  data frame (filtered and validated), and derives the event with
#'  \code{CNSR == 0} rather than \code{1 - CNSR}.
#'
#' @examples
#' # a small synthetic ADTTE (CNSR: 0 = event, >= 1 = censored)
#' adtte <- data.frame(
#'   USUBJID = sprintf("S%03d", 1:8),
#'   PARAMCD = rep(c("OS", "PFS"), each = 4),
#'   AVAL    = c(120, 300, 450, 600, 90, 150, 200, 500),
#'   CNSR    = c(0, 1, 0, 2, 0, 0, 1, 0),
#'   TRT01P  = rep(c("A", "B"), 4)
#' )
#'
#' os <- surv_adtte(adtte, paramcd = "OS")
#' fit <- surv_fit(survival::Surv(AVAL, event) ~ TRT01P, data = os)
#' ggsurvplot(fit, data = os)
#'
#' @keywords CDISC ADaM ADTTE CNSR censoring survival
#' @export
surv_adtte <- function(data, paramcd = NULL,
                       cnsr = "CNSR", aval = "AVAL", param = "PARAMCD",
                       subject = "USUBJID",
                       event.name = "event", event.ref = NULL,
                       overwrite = FALSE) {

  if (!is.data.frame(data))
    stop("`data` must be a data frame (an ADTTE dataset).", call. = FALSE)
  if (!cnsr %in% names(data))
    stop("Censoring column `", cnsr, "` not found in `data`. Set `cnsr = `.",
         call. = FALSE)
  if (!aval %in% names(data))
    stop("Analysis-value column `", aval, "` not found in `data`. Set `aval = `.",
         call. = FALSE)

  # ---- filter to one analysis parameter ----------------------------------
  if (!is.null(paramcd)) {
    if (!param %in% names(data))
      stop("Parameter column `", param, "` not found, so `paramcd` cannot be ",
           "applied. Set `param = `.", call. = FALSE)
    keep <- as.character(data[[param]]) == paramcd
    if (!any(keep, na.rm = TRUE))
      stop("No rows with ", param, " == \"", paramcd, "\". Available: ",
           paste(unique(as.character(data[[param]])), collapse = ", "), ".",
           call. = FALSE)
    data <- data[keep & !is.na(keep), , drop = FALSE]
  } else if (param %in% names(data)) {
    np <- unique(as.character(data[[param]]))
    np <- np[!is.na(np)]
    if (length(np) > 1L)
      stop("`data` holds several parameters (", paste(np, collapse = ", "),
           "); set `paramcd` to one of them.", call. = FALSE)
  }

  if (event.name %in% names(data) && !isTRUE(overwrite))
    stop("Column `", event.name, "` already exists in `data`; choose another ",
         "`event.name` or set `overwrite = TRUE`.", call. = FALSE)

  # ---- validate CNSR and derive the event indicator ----------------------
  # coerce via character so a factor/labelled CNSR yields its CODE (0/1/2), not
  # a factor level index (as.integer(factor) would silently corrupt the events).
  cnsr.num <- .adtte_numeric(data[[cnsr]], cnsr)
  if (anyNA(cnsr.num))
    stop("`", cnsr, "` has missing values; a missing censoring flag cannot be ",
         "resolved. Remove or fix those rows first.", call. = FALSE)
  if (any(cnsr.num < 0 | cnsr.num != round(cnsr.num)))
    stop("`", cnsr, "` must be non-negative integers (0 = event, 1..k = ",
         "censored); found ", paste(utils::head(sort(unique(cnsr.num)), 5),
         collapse = ", "), ".", call. = FALSE)
  if (length(unique(cnsr.num)) == 1L)
    warning("`", cnsr, "` has a single value: every record is ",
            if (cnsr.num[1] == 0) "an event." else "censored.", call. = FALSE)
  event <- as.integer(cnsr.num == 0)

  # ---- validate AVAL (drop missing times with a loud note) ---------------
  aval.num <- .adtte_numeric(data[[aval]], aval)
  if (any(aval.num < 0, na.rm = TRUE))
    stop("`", aval, "` (AVAL) has negative survival times.", call. = FALSE)
  na.time <- is.na(aval.num)
  if (any(na.time)) {
    warning("Dropped ", sum(na.time), " row(s) with a missing ", aval, " (AVAL).",
            call. = FALSE)
    data <- data[!na.time, , drop = FALSE]
    event <- event[!na.time]
  }

  # ---- optional cross-check against an explicit event indicator ----------
  if (!is.null(event.ref)) {
    if (!event.ref %in% names(data))
      stop("`event.ref` column `", event.ref, "` not found in `data`.",
           call. = FALSE)
    ref <- .adtte_numeric(data[[event.ref]], event.ref)
    bad <- which(!is.na(ref) & ref != event)
    if (length(bad))
      warning(length(bad), " row(s) where the derived event disagrees with `",
              event.ref, "`; check for an already-flipped or nonstandard coding.",
              call. = FALSE)
  }

  # ---- duplicate-subject warning (one record per subject per parameter) --
  if (!is.null(subject) && subject %in% names(data)) {
    dup <- sum(duplicated(data[[subject]]))
    if (dup > 0)
      warning(dup, " duplicate `", subject, "` value(s)",
              if (!is.null(paramcd)) paste0(" for ", param, " = ", paramcd) else "",
              "; ADTTE should have one record per subject per parameter.",
              call. = FALSE)
  }

  data[[event.name]] <- event
  rownames(data) <- NULL
  n.ev <- sum(event); n.cn <- length(event) - n.ev
  message(n.ev, " events, ", n.cn, " censored (of ", length(event), " records)",
          if (!is.null(paramcd)) paste0(" for ", param, " = ", paramcd) else "", ".")
  attr(data, "adtte") <- list(events = n.ev, censored = n.cn, n = length(event))
  data
}

# robust numeric coercion for an ADaM column: strips factor/character/labelled to
# the underlying numeric CODE, treats a logical as 0/1 and SAS/blank missing
# (".", "") as NA, and errors only on genuinely non-numeric values.
.adtte_numeric <- function(x, name) {
  if (is.logical(x)) return(as.numeric(x))          # FALSE -> 0, TRUE -> 1
  chr <- as.character(x)
  chr[chr %in% c(".", "")] <- NA                    # SAS/blank missing -> NA
  num <- suppressWarnings(as.numeric(chr))
  if (any(is.na(num) & !is.na(chr)))
    stop("`", name, "` has non-numeric values that cannot be read as numbers.",
         call. = FALSE)
  num
}
