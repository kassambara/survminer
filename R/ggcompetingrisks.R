#' Cumulative Incidence Curves for Competing Risks
#' @importFrom survival survfit
#' @description This function plots Cumulative Incidence Curves. For \code{cuminc} objects it's a \code{ggplot2} version of \code{plot.cuminc}.
#' For \code{survfitms} objects a different geometry is used, as suggested by \code{@@teigentler}.
#' It also accepts a \code{\link[tidycmprsk]{cuminc}} object from the \pkg{tidycmprsk}
#' package (which carries proper pointwise confidence limits and the data), can
#' annotate Gray's test, and can overlay the naive complement-of-Kaplan-Meier for
#' teaching.
#' @param fit a cumulative-incidence object: a \code{cmprsk::cuminc}, a
#'  \code{\link[tidycmprsk]{cuminc}} (\pkg{tidycmprsk}), or a \code{survfitms} from
#'  \code{\link[survival]{survfit}}.
#' @param gnames a vector with group names. If not supplied then will be extracted from \code{fit} object (\code{cuminc} only).
#' @param gsep a separator that extracts group names and event names from \code{gnames} object (\code{cuminc} only).
#' @param multiple_panels if \code{TRUE} then groups will be plotted in different panels (\code{cuminc} only).
#' @param ggtheme function, \code{ggplot2} theme name. Default value is \link{theme_survminer}.
#'  Allowed values include ggplot2 official themes: see \code{\link[ggplot2]{theme}}.
#' @param ... further arguments passed to the function \code{\link[ggpubr]{ggpar}} for customizing the plot.
#' @param conf.int if \code{TRUE} then additional layer (\code{geom_ribbon}) is added around the point estimate. The ribon is plotted with boundries +- \code{coef}*standard deviation.
#' @param coef see \code{conf.int}, scaling actor for the ribbon. The default value is 1.96.
#' @param pval logical or a character string. If \code{TRUE}, annotate Gray's test
#'  p-value for each cause (comparing the groups) as a subtitle; a character string
#'  is used verbatim as the subtitle. Gray's test is available only for a
#'  \code{cmprsk} / \code{tidycmprsk} \code{cuminc} object with more than one group;
#'  for other inputs \code{pval} is ignored with a message (Gray, 1988).
#' @param add.naive.km logical. If \code{TRUE}, overlay the naive
#'  complement-of-Kaplan-Meier for one cause (a dashed line), which censors the
#'  competing events and therefore \emph{overestimates} the cumulative incidence --
#'  the classic teaching contrast (Putter, Fiocco and Geskus, 2007). Supported for a
#'  \code{\link[tidycmprsk]{cuminc}} object (which carries the data) on the faceted
#'  layout; ignored with a message otherwise. It is therefore not drawn together
#'  with \code{risk.table = TRUE}, which uses the overlaid single-panel layout.
#' @param cause the cause to overlay when \code{add.naive.km = TRUE}. Required when
#'  the model has more than one cause.
#' @param risk.table logical or a character string. If \code{TRUE}, draw a
#'  number-at-risk table beneath the cumulative-incidence curves, locked to the
#'  same time axis. A character string selects the table content, one of
#'  \code{"absolute"} (the default for \code{TRUE}), \code{"percentage"},
#'  \code{"abs_pct"}, \code{"nrisk_cumcensor"} or \code{"nrisk_cumevents"} (as in
#'  \code{\link{ggsurvplot}}). The table is available for a
#'  \code{\link[tidycmprsk]{cuminc}} object (which carries the data), on the
#'  overlaid single-panel layout; it is drawn with \code{multiple_panels = FALSE}
#'  (a message is shown if \code{multiple_panels = TRUE} was passed explicitly).
#'  For a \code{cmprsk::cuminc} or \code{survfitms} input the at-risk counts are
#'  not available in a single overlaid panel, so \code{risk.table} is ignored with
#'  a message. The number at risk is the count still event-free and under
#'  observation (competing events count as leaving the risk set), matching the
#'  Kaplan-Meier at-risk convention of \code{\link{ggsurvplot}}.
#' @param risk.table.height the height of the risk table as a fraction of the whole
#'  figure. Default 0.25.
#' @param risk.table.title the title of the risk table. Default "Number at risk".
#' @param risk.table.fontsize the font size of the risk-table numbers. Default 4.5.
#' @param risk.table.y.text logical. If \code{TRUE} (default) the risk-table rows are
#'  labelled by group; if \code{FALSE} a short dash is shown instead.
#' @param break.time.by numeric value controlling the time-axis breaks shared by the
#'  curves and the risk table. Default \code{NULL} uses automatic breaks.
#' @return Returns a \code{gg} object; with \code{risk.table = TRUE} a compound
#'  \code{ggcompetingrisks} object (a list with \code{$plot} and \code{$table}) that
#'  prints the curves above the aligned table and works with \code{ggsave()}. The
#'  cumulative-incidence data and, where computed, Gray's test table and the
#'  naive-KM data are attached as \code{attr(x, "cr.data")},
#'  \code{attr(x, "grays.test")} and \code{attr(x, "naive.km")}.
#'
#' @references Gray, R. J. (1988). A class of K-sample tests for comparing the
#'  cumulative incidence of a competing risk. \emph{The Annals of Statistics},
#'  16(3), 1141-1154. \doi{10.1214/aos/1176350951}.
#'
#'  Putter, H., Fiocco, M. and Geskus, R. B. (2007). Tutorial in biostatistics:
#'  competing risks and multi-state models. \emph{Statistics in Medicine}, 26(11),
#'  2389-2430. \doi{10.1002/sim.2712}.
#'
#' @author Przemyslaw Biecek, \email{przemyslaw.biecek@@gmail.com}
#'
#' @examples
#' \dontrun{
#' if(require("cmprsk")){
#'
#' set.seed(2)
#' ss <- rexp(100)
#' gg <- factor(sample(1:3,100,replace=TRUE),1:3,c('BRCA','LUNG','OV'))
#' cc <- factor(sample(0:2,100,replace=TRUE),0:2,c('no event', 'death', 'progression'))
#' strt <- sample(1:2,100,replace=TRUE)
#'
#' # handles cuminc objects
#' print(fit <- cmprsk::cuminc(ss,cc,gg,strt))
#' ggcompetingrisks(fit)
#' ggcompetingrisks(fit, multiple_panels = FALSE)
#' ggcompetingrisks(fit, conf.int = TRUE)
#' ggcompetingrisks(fit, multiple_panels = FALSE, conf.int = TRUE)
#'
#' # handles survfitms objects
#' library(survival)
#' df <- data.frame(time = ss, group = gg, status = cc, strt)
#' fit2 <- survfit(Surv(time, status, type="mstate") ~ 1, data=df)
#' ggcompetingrisks(fit2)
#' fit3 <- survfit(Surv(time, status, type="mstate") ~ group, data=df)
#' ggcompetingrisks(fit3)
#'
#' # tidycmprsk input: Gray's test p-value and the naive-KM teaching overlay
#' if (require("tidycmprsk")) {
#'   dd <- data.frame(time = ss, group = gg, status = cc)
#'   tfit <- tidycmprsk::cuminc(Surv(time, status) ~ group, data = dd)
#'   ggcompetingrisks(tfit, pval = TRUE)
#'   ggcompetingrisks(tfit, add.naive.km = TRUE, cause = "death")
#' }
#' }
#'
#'   library(ggsci)
#'   library(cowplot)
#'   ggcompetingrisks(fit3) + theme_cowplot() + scale_fill_jco()
#' }
#' @export

ggcompetingrisks <- function(fit, gnames = NULL, gsep=" ",
                             multiple_panels = TRUE,
                             ggtheme = theme_survminer(),
                             coef = 1.96, conf.int = FALSE,
                             pval = FALSE, add.naive.km = FALSE, cause = NULL,
                             risk.table = FALSE, risk.table.height = 0.25,
                             risk.table.title = "Number at risk",
                             risk.table.fontsize = 4.5, risk.table.y.text = TRUE,
                             break.time.by = NULL, ...) {
  stopifnot(inherits(fit, c("cuminc", "tidycuminc", "survfitms")))

  # A number-at-risk table needs the per-group at-risk counts on an overlaid
  # single panel. Only a tidycmprsk::cuminc carries the data to compute them; its
  # table is drawn with multiple_panels = FALSE. Degrade honestly for the rest.
  want.table <- isTRUE(risk.table) || is.character(risk.table)
  if (want.table && !inherits(fit, "tidycuminc")) {
    message("ggcompetingrisks(): a number-at-risk table is available only for a ",
            "tidycmprsk::cuminc object; `risk.table` ignored.")
    want.table <- FALSE
  }
  if (want.table) {
    if (!missing(multiple_panels) && isTRUE(multiple_panels))
      message("ggcompetingrisks(): the number-at-risk table is drawn on the ",
              "overlaid single-panel layout; using multiple_panels = FALSE.")
    multiple_panels <- FALSE
  }

  gray <- NULL              # Gray's-test table, for the p-value subtitle + attr
  if (inherits(fit, "tidycuminc")) {
    pl   <- ggcompetingrisks.tidycuminc(fit, multiple_panels = multiple_panels,
                                        conf.int = conf.int)
    gray <- fit$cmprsk$Tests
  } else if (inherits(fit, "cuminc")) {
    pl   <- ggcompetingrisks.cuminc(fit = fit, gnames = gnames, gsep = gsep,
                                    multiple_panels = multiple_panels,
                                    coef = coef, conf.int = conf.int)
    gray <- fit$Tests
  } else {
    pl <- ggcompetingrisks.survfitms(fit = fit)
  }

  pl <- pl + ggtheme +
    ylab("Probability of an event") + xlab("Time") +
    ggtitle("Cumulative incidence functions")

  cr.data <- pl$data

  # Gray's-test p-value (subtitle). Gray's test is a between-group test on the
  # cumulative incidence of each cause; it lives only on the cmprsk / tidycmprsk
  # inputs. A single-group fit carries no test, and a survfitms/Aalen-Johansen
  # object has none -- say so rather than fake it (Gray, 1988).
  if (isTRUE(pval) || is.character(pval)) {
    sub <- .cr_gray_subtitle(fit, pval)
    if (is.null(sub))
      message("ggcompetingrisks(): no Gray's test available for this input ",
              "(it needs a multi-group cmprsk / tidycmprsk cuminc object); ",
              "`pval` ignored.")
    else pl <- pl + ggplot2::labs(subtitle = sub)
  }

  # Naive 1-KM overlay (teaching): the complement of a Kaplan-Meier that censors
  # the competing events overestimates the cumulative incidence (Putter, Fiocco
  # and Geskus, 2007). Needs the raw data, so it is supported for a
  # tidycmprsk::cuminc (which carries `data`/`formula`), for a single chosen
  # cause, on the faceted layout.
  if (isTRUE(add.naive.km)) {
    naive <- .cr_naive_km(fit, cause = cause, multiple_panels = multiple_panels)
    if (!is.null(naive)) {
      # the naive curve is dashed and in the same colour as its cause's CIF, so it
      # sits visibly above the solid CIF. Keep `linetype` a fixed constant (not an
      # aesthetic) so the colour legend is not joined by a mismatched key -- the
      # caption states what the dashed line is.
      pl <- pl +
        ggplot2::geom_line(data = naive,
                           ggplot2::aes(x = .data$time, y = .data$est,
                                        colour = .data$event),
                           linetype = "dashed") +
        ggplot2::labs(caption = "dashed: naive 1-KM (overestimates the CIF)")
      attr(pl, "naive.km") <- naive
    }
  }

  attr(pl, "cr.data") <- cr.data
  if (!is.null(gray)) attr(pl, "grays.test") <- gray
  if (!want.table) return(ggpubr::ggpar(pl, ...))

  # Number-at-risk table locked to the CIF x-axis (tidycmprsk, single panel).
  # Apply ggpar WITHOUT `xlim` here (the table path locks the x-axis with a single
  # coord for alignment) and honour a user `xlim` inside .cr_add_risktable, so the
  # curve and table share one limit and no duplicate coord is added.
  dots <- list(...)
  pl <- do.call(ggpubr::ggpar, c(list(pl), dots[setdiff(names(dots), "xlim")]))
  res <- .cr_add_risktable(fit, pl, risk.table = risk.table,
                           risk.table.height = risk.table.height,
                           title = risk.table.title, fontsize = risk.table.fontsize,
                           y.text = risk.table.y.text, break.time.by = break.time.by,
                           xlim = dots[["xlim"]], ggtheme = ggtheme)
  # mirror the CIF attributes onto the compound object and its $plot so
  # attr(x, "cr.data") reads the same as on the bare-plot path
  for (a in c("cr.data", "grays.test", "naive.km")) {
    v <- attr(pl, a)
    if (!is.null(v)) attr(res, a) <- attr(res$plot, a) <- v
  }
  res
}

# Build the event-free number-at-risk table from a tidycmprsk::cuminc fit and stack
# it under the CIF plot, sharing one time axis. The at-risk count is computed with
# the same machinery as ggsurvplot's KM table (summary(survfit, times=)): a subject
# leaves the risk set at its event (of ANY cause) or censoring, so the count is
# #{still event-free and under observation}. `fit$data`/`fit$formula` are carried by
# tidycmprsk (also used by the naive-KM overlay).
.cr_add_risktable <- function(fit, pl, risk.table = TRUE, risk.table.height = 0.25,
                              title = "Number at risk", fontsize = 4.5,
                              y.text = TRUE, break.time.by = NULL, xlim = NULL,
                              ggtheme = theme_survminer()) {
  data  <- fit$data
  tvar  <- all.vars(fit$formula[[2]])[1]
  svar  <- all.vars(fit$formula[[2]])[2]
  gvars <- all.vars(fit$formula[[3]])
  data[[".cr_event"]] <- as.integer(as.character(data[[svar]]) %in% names(fit$failcode))
  form <- stats::reformulate(if (length(gvars)) gvars else "1",
                             response = paste0("survival::Surv(", tvar, ", .cr_event)"))
  sf <- surv_fit(form, data = data)

  # one shared time axis: derive breaks and limits from the SAME source ggsurvtable
  # uses internally (sf$time), so the curve ticks and the table columns coincide
  # exactly (a single source of truth, not two parallel break computations). A
  # user-supplied `xlim` (via `...`) is honoured on both panels.
  if (is.null(xlim)) xlim <- c(0, max(sf$time))
  brk  <- if (is.null(break.time.by)) .get_default_breaks(sf$time)
          else .time_breaks(break.time.by, max(c(sf$time, xlim)))
  pl <- pl + ggplot2::scale_x_continuous(breaks = brk) +
             ggplot2::coord_cartesian(xlim = xlim)

  rt.type <- if (is.character(risk.table)) risk.table else "absolute"
  tbl <- ggsurvtable(sf, data = data, survtable = "risk.table",
                     risk.table.type = rt.type, break.time.by = break.time.by,
                     xlim = xlim, title = title, ylab = "", xlab = "Time",
                     fontsize = fontsize, y.text = y.text, y.text.col = FALSE,
                     ggtheme = ggtheme)
  # the table has no mapped colour aesthetic (y.text.col = FALSE), so drop the
  # inherited "Strata" colour label to avoid a spurious "Ignoring unknown labels"
  # note when it is drawn
  tbl$labels$colour <- NULL

  # assemble a ggsurvplot-shaped compound so the tested aligned-render path
  # (.build_ggsurvplot: shared-width alignment + legend + heights) draws it
  res <- list(plot = pl, table = tbl)
  attr(res, "heights") <- list(plot = 1 - risk.table.height, table = risk.table.height,
                               ncensor.plot = 0, cumevents = 0)
  attr(res, "y.text")     <- list(plot = TRUE,  table = y.text)
  attr(res, "y.text.col") <- list(plot = FALSE, table = FALSE)
  attr(res, "cumcensor")      <- FALSE
  attr(res, "axes.offset")    <- TRUE
  attr(res, "risk.table.pos") <- "out"
  attr(res, "legend.position") <- "top"
  # legend.labs must match the number of colours in the CIF (colour = cause), so
  # .build_ggsurvplot's colour-naming does not error on a length mismatch
  attr(res, "legend.labs") <- names(fit$failcode)
  class(res) <- c("ggcompetingrisks", "ggsurvplot", "ggsurv", "list")
  res
}


ggcompetingrisks.cuminc <- function(fit, gnames = NULL, gsep=" ",
                                    multiple_panels = TRUE, coef = 1.96, conf.int = FALSE) {
  if (!is.null(fit$Tests))
    fit <- fit[names(fit) != "Tests"]
  fit2 <- lapply(fit, `[`, 1:3)
  if (is.null(gnames)) gnames <- names(fit2)
  fit2_list <- lapply(seq_along(gnames), function(ind) {
    df <- as.data.frame(fit2[[ind]])
    df$name <- gnames[ind]
    df
  })
  time <- est <- event <- group <- NULL
  df <- do.call(rbind, fit2_list)
  df$event <- sapply(strsplit(df$name, split=gsep), `[`, 2)
  df$group <- sapply(strsplit(df$name, split=gsep), `[`, 1)
  df$std <- std <- sqrt(df$var)
  pl <- ggplot(df, aes(time, est, color=event))
  if (multiple_panels) {
    pl <- ggplot(df, aes(time, est, color=event)) + facet_wrap(~group)
  } else {
    pl <- ggplot(df, aes(time, est, color=event, linetype=group))
  }
  if (conf.int) {
    # Group the ribbon by event AND group. Mapping only fill=event makes the
    # ribbon's implicit grouping ignore `group`, so in a single panel the bands
    # jump between groups of the same event (#490). interaction(event, group) is
    # a no-op for the faceted case (one group per panel). Clamp the band to
    # [0, 1]: `est +- coef*std` is symmetric on the probability scale and can run
    # below 0 or above 1, which is not a valid incidence probability.
    pl <- pl + geom_ribbon(aes(ymin = pmax(0, est - coef*std),
                               ymax = pmin(1, est + coef*std),
                               fill = event, group = interaction(event, group)),
                           alpha = 0.2, linetype = 0)
  }
  pl +
    geom_line()
}

ggcompetingrisks.survfitms <- function(fit) {
  times <- fit$time
  psta <- as.data.frame(fit$pstate)
  # A multistate Cox model predicted over SEVERAL covariate profiles
  # (survfit(coxph_multistate, newdata = <2+ rows>)) returns a 3-D pstate array
  # (time x profile x state), so as.data.frame() has one column per
  # profile-x-state combination -- more columns than there are states. The
  # colnames() assignment just below would then recycle fit$states into NA
  # names, which surfaces to the user as the opaque "Names repair functions
  # can't return `NA` values" error (#625). Fail early with an actionable
  # message. Inputs where the column count already matches the states -- the
  # ordinary 2-D path AND a single-profile prediction (time x 1 x state, which
  # collapses to one column per state and renders correctly) -- are untouched.
  if (ncol(psta) != length(fit$states))
    stop("ggcompetingrisks() does not support predicted cumulative incidence ",
         "curves from a multistate Cox model evaluated at several covariate ",
         "profiles -- survfit() with a multi-row `newdata` returns one set of ",
         "curves per profile (a 3-D probability array) that this stacked-area ",
         "plot can't lay out. Supply a single covariate profile, use a ",
         "cmprsk::cuminc() object or a survfit(Surv(time, status, ",
         "type = \"mstate\") ~ ...) fit, or draw the several profiles with the ",
         "base plot() method on the survfit object.", call. = FALSE)
  colnames(psta) <- fit$states
  if (is.null(fit$strata)) {
    psta$strata <- "all"
  } else {
    # Keep the model's strata order in the facets instead of letting facet_wrap()
    # sort the strata names alphabetically (a factor preserves names(fit$strata)
    # order). Byte-identical when the model order already is alphabetical (#470).
    psta$strata <- factor(rep(names(fit$strata), fit$strata),
                          levels = names(fit$strata))
  }
  psta$times <- times
  psta <- .rename_empty_colname(
    psta, newname = "."
    )

  event <- value <- strata <- NULL
  pstal <- gather(psta, event, value, -strata, -times)
  ggplot(pstal, aes(times, value, fill=event)) +
    geom_area() + facet_wrap(~strata)

}

# CIF plot from a tidycmprsk::cuminc object. Reads the stored `$tidy` frame (the
# cumulative incidence with proper, transformed pointwise confidence limits) so
# no call into tidycmprsk is needed at run time. Same geometry as the cmprsk
# path: colour = cause, facet by group (or linetype = group in a single panel).
ggcompetingrisks.tidycuminc <- function(fit, multiple_panels = TRUE, conf.int = FALSE) {
  td <- fit$tidy
  if (is.null(td) || !all(c("time", "estimate", "outcome") %in% names(td)))
    stop("This tidycmprsk object is not in the expected form; update survminer ",
         "or tidycmprsk.", call. = FALSE)
  # an ungrouped fit (Surv(...) ~ 1) has no `strata` column -> one "all" group,
  # matching the cmprsk / survfitms single-group behaviour.
  strata <- if ("strata" %in% names(td)) as.character(td$strata) else "all"
  df <- data.frame(time = td$time, est = td$estimate,
                   event = factor(td$outcome, levels = names(fit$failcode)),
                   group = factor(strata, levels = unique(strata)),
                   lo = td$conf.low, hi = td$conf.high)
  df <- df[!is.na(df$est), , drop = FALSE]      # drop non-plottable rows quietly
  # the pointwise CI is undefined at t = 0 (est = 0); set a zero-width band there
  # so the ribbon does not drop rows with a warning.
  df$lo[is.na(df$lo)] <- df$est[is.na(df$lo)]
  df$hi[is.na(df$hi)] <- df$est[is.na(df$hi)]
  time <- est <- event <- group <- NULL
  if (multiple_panels)
    pl <- ggplot(df, aes(time, est, color = event)) + facet_wrap(~group)
  else
    pl <- ggplot(df, aes(time, est, color = event, linetype = group))
  if (conf.int)
    pl <- pl + geom_ribbon(aes(ymin = .data$lo, ymax = .data$hi, fill = event,
                               group = interaction(event, group)),
                           alpha = 0.2, linetype = 0)
  pl + geom_line()
}

# Gray's-test p-values as a one-line subtitle, or NULL when no test applies.
# `pval` may be a custom string (used verbatim). cmprsk `$Tests` rows are named
# by cause; tidycmprsk `$cmprsk$Tests` rows are cause codes, mapped back to the
# cause labels via `$failcode`.
.cr_gray_subtitle <- function(fit, pval) {
  if (is.character(pval)) return(pval)
  if (inherits(fit, "tidycuminc")) {
    gray <- fit$cmprsk$Tests
    if (is.null(gray) || nrow(gray) == 0) return(NULL)
    labs <- names(fit$failcode)
    if (length(labs) != nrow(gray)) labs <- rownames(gray)
  } else if (inherits(fit, "cuminc")) {
    gray <- fit$Tests
    if (is.null(gray) || nrow(gray) == 0) return(NULL)
    labs <- rownames(gray)
  } else return(NULL)
  pv <- format.pval(gray[, "pv"], digits = 2, eps = 1e-4)
  paste0("Gray's test    ", paste0(labs, ": p = ", pv, collapse = "     "))
}

# Naive complement-of-KM (competing events censored) for one cause, per group, as
# a long data frame (time, est, event, group) matching the CIF plot's factors so
# the overlay lines take the cause's colour. tidycmprsk input only (it carries
# the data + formula); returns NULL with a message otherwise.
.cr_naive_km <- function(fit, cause, multiple_panels) {
  if (!inherits(fit, "tidycuminc")) {
    message("ggcompetingrisks(): `add.naive.km` needs a tidycmprsk::cuminc object ",
            "(it carries the data); ignored.")
    return(NULL)
  }
  if (!isTRUE(multiple_panels)) {
    message("ggcompetingrisks(): `add.naive.km` is drawn only on the faceted ",
            "layout (multiple_panels = TRUE); ignored.")
    return(NULL)
  }
  causes <- names(fit$failcode)
  if (is.null(cause)) {
    if (length(causes) == 1L) cause <- causes
    else stop("`add.naive.km = TRUE` overlays a single cause; set `cause` to one ",
              "of: ", paste(causes, collapse = ", "), ".", call. = FALSE)
  }
  if (length(cause) != 1L || !cause %in% causes)
    stop("`cause` must be one of the model's causes: ",
         paste(causes, collapse = ", "), ".", call. = FALSE)

  data <- fit$data
  tvar <- all.vars(fit$formula[[2]])[1]
  svar <- all.vars(fit$formula[[2]])[2]
  gvars <- all.vars(fit$formula[[3]])
  ev <- as.numeric(as.character(data[[svar]]) == cause)
  if (length(gvars) == 0L) {
    # ungrouped fit (Surv(...) ~ 1): a single "all" group, matching the plot
    km <- survival::survfit(survival::Surv(data[[tvar]], ev) ~ 1)
    glevels <- "all"
    gid <- rep("all", length(km$time))
  } else {
    grp <- data[[gvars[1]]]
    km <- survival::survfit(survival::Surv(data[[tvar]], ev) ~ grp)
    glevels <- levels(factor(grp))
    gid <- if (is.null(km$strata)) rep(as.character(grp)[1], length(km$time))
           else rep(sub("^[^=]*=", "", names(km$strata)), km$strata)
  }
  data.frame(time = km$time, est = 1 - km$surv,
             event = factor(cause, levels = names(fit$failcode)),
             group = factor(gid, levels = glevels))
}

.rename_empty_colname <- function(df, newname = "."){
  empty.col <- colnames(df) == ""
  empty.col.exist <- length(empty.col) > 0
  if(empty.col.exist)
    colnames(df)[empty.col] <- newname
  df
}
