#' @include utilities.R
NULL
#' Subgroup Forest Plot of a Treatment Hazard Ratio
#'
#' @description
#' Draws the forest plot clinicians publish for a subgroup analysis: the hazard
#' ratio of a treatment \emph{within} each level of one or more subgroup
#' variables, together with the treatment-by-subgroup interaction test. It
#' complements \code{\link{ggforest}()}, which draws the coefficients of a single
#' fitted model rather than a treatment effect broken down by subgroup.
#'
#' For each subgroup level the Cox model is refit on that subset and the
#' treatment hazard ratio (with confidence interval) is extracted. For each
#' subgroup \emph{variable} a single interaction p-value is reported, from a
#' likelihood-ratio test of the treatment-by-subgroup interaction -- this test,
#' not the per-level hazard ratios, is the evidence for effect modification.
#'
#' @details
#' The per-level estimate is the treatment coefficient of \code{model} refit on
#' the subset of \code{data} in that level (any subgroup variable that also
#' appears in the model, and any adjusting covariate that becomes constant within
#' the subset, is dropped from that fit so it does not error). Levels in which the
#' model cannot be fit -- too few events, no treatment contrast, or
#' non-convergence -- are dropped from the plot with a warning rather than
#' stopping.
#'
#' The interaction p-value for a subgroup variable is the likelihood-ratio test
#' comparing the additive model (\code{model} plus the subgroup main effect)
#' with the interaction model (additionally the treatment-by-subgroup term),
#' fit on the observations complete for both. As with any subgroup analysis the
#' interaction test is the inference; the individual per-level hazard ratios are
#' descriptive and subject to multiplicity (see Wang et al., 2007).
#'
#' The subset fits are made the way \code{model} itself was: its case weights,
#' tie handling and clustering are carried over, so a weighted model reports
#' weighted subgroup estimates and a clustered model cluster-robust intervals.
#' The weights and the clustering are read from the fitted object and matched to
#' the rows of \code{data}; if they cannot be matched -- \code{data} reordered,
#' or not the frame the model was fitted on -- that is an error rather than a
#' quietly unweighted plot.
#'
#' Which interaction test is used follows the variance the fit carries. With the
#' ordinary model-based variance it is the likelihood-ratio test described above.
#' When the fit carries a robust variance -- \code{\link[survival]{coxph}()}
#' attaches one to any clustered, \code{robust = TRUE} or non-integer weighted
#' fit -- differences in the log-partial-likelihood no longer have a chi-square
#' distribution, and \code{\link[survival]{anova.coxph}} declines to compare
#' them for that reason; the interaction is then tested by a Wald chi-square on
#' the treatment-by-subgroup coefficients, taken from that robust variance.
#' Integer (frequency) weights leave the variance model-based, so they keep the
#' likelihood-ratio test.
#'
#' The per-level hazard ratio is estimated by refitting on that level alone, so
#' each level carries its own baseline hazard and, in an adjusted model, its own
#' covariate coefficients. It is the estimate a reader reproduces by running
#' \code{\link[survival]{coxph}()} on that subset by hand. Some tools instead
#' read the within-level effects off a single treatment-by-subgroup interaction
#' model fitted to everyone, which pools the baseline hazard across levels; the
#' two agree closely in large balanced strata and can differ appreciably in small
#' or heterogeneous ones.
#'
#' The plot is composed of three aligned panels (labels, forest, statistics), so
#' the text columns keep a fixed width and do not collide with the forest at any
#' figure size.
#'
#' @param model a \code{coxph} object whose terms include \code{treatment}. It may
#'   be crude (\code{~ treatment}) or adjusted (\code{~ treatment + covariates}).
#' @param data the data frame used to fit \code{model}. If not supplied it is
#'   extracted from \code{model}.
#' @param treatment the name of the treatment variable (a term of \code{model}).
#'   It must yield a single hazard ratio, i.e. a two-level factor or a numeric
#'   variable; a treatment with more than two levels is ambiguous here and is an
#'   error.
#' @param subgroups subgroup variable names in \code{data} (categorical / factor).
#'   May be named, in which case the names are used as the display labels, e.g.
#'   \code{c(Sex = "sex", "Age group" = "age.grp")}. Continuous variables must be
#'   binned first.
#' @param conf.level the confidence level for the intervals, a single number in
#'   (0, 1). Default 0.95.
#' @param show.overall logical; add an overall (all-subjects) treatment hazard
#'   ratio row at the top. Default \code{TRUE}.
#' @param show.pinteraction logical; show the per-variable interaction p-value.
#'   Default \code{TRUE}.
#' @param show.n logical; show a "No. of patients (\%)" column giving the number of
#'   subjects in each subgroup level (and their percentage of the total). Default
#'   \code{TRUE}. This count is the subgroup size; when the model has adjusting
#'   covariates with missing values it can exceed the complete-case sample the
#'   hazard ratio is actually fit on. It counts subjects, so for a weighted model
#'   it does not equal the sum of the weights the hazard ratio is estimated from.
#' @param favours optional length-2 character vector
#'   \code{c("Favours treatment", "Favours control")} drawn under the axis on the
#'   left (HR < 1) and right (HR > 1) sides of the reference line. Default
#'   \code{NULL} omits it.
#' @param point.size.by.precision logical; scale the hazard-ratio box area by the
#'   estimate's precision (inverse variance), the usual forest-plot convention, so
#'   more informative subgroups draw a larger box. Default \code{TRUE}; set
#'   \code{FALSE} for equal-sized points.
#' @param main plot title. Default \code{"Treatment effect by subgroup"}.
#' @param xlab x-axis label. Default \code{NULL} builds
#'   \code{"Hazard ratio (<treatment> vs <reference>, log scale)"}.
#' @param noDigits number of digits for the hazard ratios and p-values. Default 2.
#' @param ggtheme a ggplot2 theme for the forest panel. Default
#'   \code{\link{theme_survminer}()}.
#' @return a ggplot object (the assembled panels).
#' @references
#' Wang R, Lagakos SW, Ware JH, Hunter DJ, Drazen JM (2007). Statistics in
#' medicine -- reporting of subgroup analyses in clinical trials. \emph{New
#' England Journal of Medicine} 357(21):2189-2194.
#' @seealso \code{\link{ggforest}()}
#' @examples
#' library(survival)
#' # Two treatment arms (Lev+5FU vs Obs) with categorical subgroups
#' cc <- colon[colon$etype == 2 & colon$rx %in% c("Obs", "Lev+5FU"), ]
#' cc$rx <- droplevels(cc$rx)
#' cc$sex <- factor(cc$sex, labels = c("Female", "Male"))
#' cc$age.grp <- factor(ifelse(cc$age >= 60, ">=60", "<60"), levels = c("<60", ">=60"))
#' cc$differ <- factor(cc$differ, labels = c("well", "moderate", "poor"))
#'
#' fit <- coxph(Surv(time, status) ~ rx, data = cc)
#' ggforest_subgroup(fit, data = cc, treatment = "rx",
#'                   subgroups = c(Sex = "sex", Age = "age.grp", Differentiation = "differ"))
#' @export
ggforest_subgroup <- function(model, data = NULL, treatment,
                              subgroups, conf.level = 0.95,
                              show.overall = TRUE, show.pinteraction = TRUE,
                              show.n = TRUE, favours = NULL,
                              point.size.by.precision = TRUE,
                              main = "Treatment effect by subgroup",
                              xlab = NULL, noDigits = 2,
                              ggtheme = theme_survminer()) {

  if (!is.numeric(conf.level) || length(conf.level) != 1L || is.na(conf.level) ||
      conf.level <= 0 || conf.level >= 1)
    stop("`conf.level` must be a single number in (0, 1).", call. = FALSE)
  tab <- .subgroup_forest_table(model, data, treatment, subgroups,
                                conf.level, show.overall)
  if (nrow(tab[tab$type != "header", , drop = FALSE]) == 0L)
    stop("No subgroup level could be estimated; nothing to plot.", call. = FALSE)

  # Top-to-bottom display order mapped to descending y (first row at the top).
  tab$y <- rev(seq_len(nrow(tab)))
  est <- tab[tab$type != "header", , drop = FALSE]
  head.y <- max(tab$y) + 1.4                 # column-title row, above the data
  yr <- c(min(tab$y) - 1, head.y + 0.3)
  if (!is.null(favours) && length(favours) == 2L) yr[1] <- yr[1] - 0.6

  ref.lev <- attr(tab, "ref"); trt.lev <- attr(tab, "trt")
  if (is.null(xlab))
    xlab <- sprintf("Hazard ratio (%s, %g%% CI, log scale)",
                    if (!is.na(trt.lev) && !is.na(ref.lev))
                      paste(trt.lev, "vs", ref.lev) else "treatment",
                    conf.level * 100)

  # ---- forest panel (the only one with an x-axis) --------------------------
  rng <- range(c(est$lower, est$upper), na.rm = TRUE)
  breaks <- .subgroup_breaks(rng)
  if (point.size.by.precision && any(is.finite(est$prec) & est$prec > 0)) {
    w <- est$prec; w[!is.finite(w) | w <= 0] <- NA
    est$psize <- 2.5 + 2.8 * sqrt(w / max(w, na.rm = TRUE))
    est$psize[is.na(est$psize)] <- 3.2
  } else est$psize <- 3.2

  hr <- lower <- upper <- y <- psize <- text <- face <- hrci <- plab <- NULL
  forest <- ggplot2::ggplot(est, ggplot2::aes(x = hr, y = y)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey55") +
    ggplot2::geom_segment(ggplot2::aes(x = lower, xend = upper, y = y, yend = y),
                          colour = "grey25") +
    ggplot2::geom_point(ggplot2::aes(size = psize), shape = 15, colour = "grey15") +
    ggplot2::scale_size_identity() +
    ggplot2::scale_x_log10(breaks = breaks, labels = .fmt_break(breaks)) +
    ggplot2::scale_y_continuous(limits = yr, expand = c(0, 0)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = xlab, y = NULL) +
    ggtheme +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank())

  # optional "favours" annotation flanking the reference line
  if (!is.null(favours) && length(favours) == 2L) {
    yf <- min(tab$y) - 1.1
    forest <- forest +
      ggplot2::annotate("text", x = 1 / 1.08, y = yf, label = favours[1],
                        hjust = 1, size = 3, fontface = "italic", colour = "grey35") +
      ggplot2::annotate("text", x = 1.08, y = yf, label = favours[2],
                        hjust = 0, size = 3, fontface = "italic", colour = "grey35")
  }

  # ---- side panels (labels, statistics) -- no axes, shared y --------------
  void <- .subgroup_side_theme(yr)

  lab.df <- data.frame(
    y = tab$y,
    text = ifelse(tab$type == "level", paste0("   ", tab$label), tab$label),
    face = ifelse(tab$type == "header", "bold", "plain"),
    stringsAsFactors = FALSE)
  label.panel <- ggplot2::ggplot(lab.df, ggplot2::aes(x = 0, y = y)) +
    ggplot2::geom_text(ggplot2::aes(label = text, fontface = face),
                       hjust = 0, size = 3.3) +
    ggplot2::annotate("text", x = 0, y = head.y, label = "Subgroup",
                      hjust = 0, fontface = "bold", size = 3.15) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) + void

  # optional "No. of patients (%)" column (per subgroup level, % of the total).
  # Right-aligned with a generous panel width so the count never clips.
  n.panel <- NULL
  if (isTRUE(show.n) && any(!is.na(tab$n))) {
    n.overall <- attr(tab, "n.overall")
    n.df <- tab[!is.na(tab$n), c("y", "n"), drop = FALSE]
    n.df$nlab <- sprintf("%d (%d%%)", n.df$n, round(100 * n.df$n / n.overall))
    nlab <- NULL
    n.panel <- ggplot2::ggplot(n.df, ggplot2::aes(x = 1, y = y)) +
      ggplot2::geom_text(ggplot2::aes(label = nlab), hjust = 1, size = 3.2) +
      ggplot2::annotate("text", x = 1, y = head.y, label = "No. (%)",
                        hjust = 1, fontface = "bold", size = 3.15) +
      ggplot2::scale_x_continuous(limits = c(0, 1),
                                  expand = ggplot2::expansion(mult = c(0.04, 0.02))) + void
  }

  # HR text is right-aligned so it sits adjacent to the p-int column (rather than
  # drifting to a wide right-edge gap as the figure widens).
  est$hrci <- .fmt_hrci(est, noDigits)
  ci.header <- sprintf("HR (%g%% CI)", conf.level * 100)
  hrci.panel <- ggplot2::ggplot(est, ggplot2::aes(x = 1, y = y)) +
    ggplot2::geom_text(ggplot2::aes(label = hrci), hjust = 1, size = 3.3) +
    ggplot2::annotate("text", x = 1, y = head.y, label = ci.header,
                      hjust = 1, fontface = "bold", size = 3.15) +
    ggplot2::scale_x_continuous(limits = c(0, 1),
                                expand = ggplot2::expansion(mult = c(0.04, 0.02))) + void

  # p-int as its own fixed-width column, so its offset from the HR text does not
  # drift with the figure size.
  hdr <- tab[tab$type == "header", , drop = FALSE]
  has.pint <- show.pinteraction && nrow(hdr) > 0 && any(!is.na(hdr$pint))
  panels <- list(label.panel)
  widths <- 0.6 + 0.085 * max(nchar(c(lab.df$text, "Subgroup")))
  if (!is.null(n.panel)) {
    panels <- c(panels, list(n.panel))
    widths <- c(widths, 0.5 + 0.095 * max(nchar(c(n.df$nlab, "No. (%)"))))
  }
  panels <- c(panels, list(forest, hrci.panel))
  widths <- c(widths, 3.4, 0.5 + 0.085 * max(nchar(c(est$hrci, ci.header))))
  if (has.pint) {
    # bare p-value under a "P-int" header (shorter than an inline "p-int x" label,
    # so the rightmost column does not clip at narrow widths), centred in its panel.
    hdr$plab <- ifelse(is.na(hdr$pint), "",
                       format.pval(hdr$pint, digits = noDigits, eps = 1e-3))
    pint.panel <- ggplot2::ggplot(hdr, ggplot2::aes(x = 0.5, y = y)) +
      ggplot2::geom_text(ggplot2::aes(label = plab), hjust = 0.5, size = 3.05,
                         fontface = "italic", colour = "grey30") +
      ggplot2::annotate("text", x = 0.5, y = head.y, label = "P-int",
                        hjust = 0.5, fontface = "bold", size = 3.15, colour = "grey20") +
      ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) + void
    panels <- c(panels, list(pint.panel))
    widths <- c(widths, 0.5 + 0.08 * max(nchar(c(hdr$plab, "P-int"))))
  }

  # ---- assemble: widths scale with the longest text in each column --------
  p <- ggpubr::ggarrange(plotlist = panels, nrow = 1, widths = widths, align = "h")
  if (!is.null(main) && nzchar(main))
    p <- ggpubr::annotate_figure(
      p, top = ggpubr::text_grob(main, face = "bold", size = 13))
  p
}


# ---- internals ---------------------------------------------------------------

# Build the ordered table of rows (overall / header / level) with the within-level
# treatment HR + CI and the per-variable interaction p-value. Attaches the
# treatment/reference labels used in the axis title.
.subgroup_forest_table <- function(model, data, treatment, subgroups,
                                    conf.level = 0.95, show.overall = TRUE) {
  if (!inherits(model, "coxph"))
    stop("`model` must be a coxph object.", call. = FALSE)
  if (missing(treatment) || length(treatment) != 1L || !is.character(treatment))
    stop("`treatment` must be a single variable name.", call. = FALSE)
  if (missing(subgroups) || !is.character(subgroups) || length(subgroups) < 1L)
    stop("`subgroups` must be a character vector of variable names.", call. = FALSE)
  # `subgroups` may be named -- the names are the display labels for the headers,
  # the values are the variable names in the data (e.g. c(Sex = "sex")).
  sg.vars <- unname(subgroups)
  sg.labs <- if (!is.null(names(subgroups))) {
    ifelse(names(subgroups) == "", .cap1(sg.vars), names(subgroups))
  } else .cap1(sg.vars)
  data <- as.data.frame(.get_data(model, data, complain = FALSE))
  miss <- setdiff(c(treatment, sg.vars), colnames(data))
  if (length(miss) > 0)
    stop("Not found in the data: ", paste(miss, collapse = ", "), ".", call. = FALSE)

  alpha <- 1 - conf.level
  tkey  <- .treatment_key(model, treatment, data)   # coef name + labels

  # Every subgroup hazard ratio comes from refitting the model on that subset, so
  # the refit has to be the user's model: dropping the weights or the tie handling
  # reported a different estimate than the one the model carries, with nothing on
  # the plot to say so. The weights ride along as a data column, so subsetting the
  # data subsets them too.
  menv <- environment(stats::formula(model))
  if (is.null(menv)) menv <- parent.frame()
  # Weights and clustering ride along as data columns, so subsetting the data
  # subsets them too. Both are taken from the fitted object where survival stores
  # them, never re-evaluated from the call: re-evaluating would resolve the name
  # wherever this function happens to be standing and can pick up an unrelated
  # object of the same name.
  wcol <- ccol <- NULL
  if (!is.null(model$weights)) {
    w.all <- .fit_vector(model$weights, model, data)
    if (is.null(w.all) || isFALSE(.data_is_fit_data(model, data)))
      stop("ggforest_subgroup() cannot match the case weights of `model` to the rows ",
           "of `data`. Supply the data frame `model` was fitted on.", call. = FALSE)
    wcol <- ".ggf_weights"
    while (wcol %in% names(data)) wcol <- paste0(wcol, "_")
    data[[wcol]] <- w.all
  }
  # cluster() written in the formula is moved to the call by coxph(), so both
  # spellings arrive here the same way.
  if (!is.null(model$call$cluster)) {
    c.all <- .model_call_vector(model$call$cluster, data, menv)
    if (is.null(c.all) || isFALSE(.data_is_fit_data(model, data)))
      stop("ggforest_subgroup() cannot match the clustering of `model` to the rows of ",
           "`data`. Supply the data frame `model` was fitted on.", call. = FALSE)
    ccol <- ".ggf_cluster"
    while (ccol %in% names(data)) ccol <- paste0(ccol, "_")
    data[[ccol]] <- c.all
  }
  fit.args <- list()
  if (!is.null(model$call$ties))
    fit.args$ties <- tryCatch(eval(model$call$ties, envir = menv), error = function(e) NULL)
  if (!is.null(model$call$robust))
    fit.args$robust <- tryCatch(eval(model$call$robust, envir = menv), error = function(e) NULL)
  fit.args <- fit.args[!vapply(fit.args, is.null, logical(1))]
  .coxph_like <- function(fo, dsub) {
    a <- c(list(formula = fo, data = dsub), fit.args)
    if (!is.null(wcol)) a$weights <- dsub[[wcol]]
    if (!is.null(ccol)) a$cluster <- dsub[[ccol]]
    do.call(survival::coxph, a)
  }

  # per-subset treatment HR/CI/precision from a refit of the model formula
  one <- function(dsub) {
    fo <- .drop_constant_terms(stats::formula(model), dsub, keep = treatment)
    fit <- tryCatch(.coxph_like(fo, dsub),
                    error = function(e) NULL, warning = function(w) NULL)
    if (is.null(fit)) return(NULL)
    est <- stats::coef(fit)
    if (!(tkey$coef %in% names(est))) return(NULL)
    b <- est[[tkey$coef]]; se <- sqrt(diag(stats::vcov(fit))[[tkey$coef]])
    z <- stats::qnorm(1 - alpha / 2)
    if (!is.finite(b) || !is.finite(se) || se <= 0) return(NULL)
    list(hr = exp(b), lower = exp(b - z * se), upper = exp(b + z * se),
         prec = 1 / se^2)
  }

  rows <- list()
  n.overall <- nrow(data)
  if (show.overall) {
    o <- one(data)
    rows[[length(rows) + 1L]] <- .row("overall", "Overall", o, n = n.overall)
  }

  dropped <- character(0)
  for (j in seq_along(sg.vars)) {
    v <- sg.vars[j]
    x <- data[[v]]
    if (is.numeric(x) && length(unique(stats::na.omit(x))) > 10L)
      stop("Subgroup variable `", v, "` looks continuous; bin it into ",
           "categories first.", call. = FALSE)
    f <- if (is.factor(x)) x else factor(x)
    hrow <- .row("header", sg.labs[j], NULL)
    hrow$pint <- .interaction_p(model, data, treatment, v, fitter = .coxph_like)
    rows[[length(rows) + 1L]] <- hrow
    for (lv in levels(f)) {
      dsub <- data[!is.na(x) & x == lv, , drop = FALSE]
      o <- if (nrow(dsub) > 0) one(dsub) else NULL
      if (is.null(o)) { dropped <- c(dropped, paste0(v, "=", lv)); next }
      rows[[length(rows) + 1L]] <- .row("level", as.character(lv), o, n = nrow(dsub))
    }
  }
  if (length(dropped) > 0)
    warning("Dropped subgroup level(s) that could not be estimated (too few ",
            "events / no treatment contrast / non-convergence): ",
            paste(dropped, collapse = ", "), ".", call. = FALSE)

  out <- do.call(rbind, rows)
  attr(out, "trt") <- tkey$trt
  attr(out, "ref") <- tkey$ref
  attr(out, "n.overall") <- n.overall
  out
}

# One table row; `o` is NULL (header / unestimable) or a list(hr,lower,upper,prec).
.row <- function(type, label, o, n = NA_integer_) {
  data.frame(
    type = type, label = label, n = as.integer(n),
    hr    = if (is.null(o)) NA_real_ else o$hr,
    lower = if (is.null(o)) NA_real_ else o$lower,
    upper = if (is.null(o)) NA_real_ else o$upper,
    prec  = if (is.null(o)) NA_real_ else o$prec,
    pint = NA_real_, stringsAsFactors = FALSE)
}

# Identify the single treatment coefficient and, for a two-level factor, the
# treatment / reference labels used in the axis title. Errors if the treatment
# does not map to exactly one coefficient (e.g. >2 levels).
.treatment_key <- function(model, treatment, data) {
  idx <- model$assign[[treatment]]
  if (is.null(idx)) {
    pos <- which(gsub("`", "", names(model$assign)) == treatment)
    if (length(pos) == 1L) idx <- model$assign[[pos]]
  }
  nm <- names(stats::coef(model))
  coefs <- if (!is.null(idx)) nm[idx] else grep(treatment, nm, fixed = TRUE, value = TRUE)
  if (length(coefs) != 1L)
    stop("`treatment` (", treatment, ") must yield a single hazard ratio ",
         "(a two-level factor or a numeric variable); it maps to ",
         length(coefs), " coefficients. Reduce it to two levels.", call. = FALSE)
  x <- data[[treatment]]
  trt <- ref <- NA_character_
  if (is.factor(x) || is.character(x)) {
    lv <- levels(factor(x))
    if (length(lv) == 2L) { ref <- lv[1]; trt <- lv[2] }
  }
  list(coef = coefs, trt = trt, ref = ref)
}

# Interaction p-value for one subgroup variable, comparing the additive model
# (model + subgroup) with the interaction model (+ treatment:subgroup) on the
# shared complete cases, both fitted the way the model itself was (`fitter`).
# Names are back-quoted so non-syntactic variable names do not break the formula.
#
# Which test is used follows the variance the fit carries. With the model-based
# variance this is the likelihood-ratio test. When the fit carries a robust
# variance -- coxph() attaches one to any non-integer weighted, clustered or
# robust = TRUE fit -- differences in the log-partial-likelihood no longer have
# a chi-square distribution (see ?anova.coxph, which declines such a comparison
# for exactly that reason), so the interaction coefficients are tested with a
# Wald chi-square built on the robust variance itself. Returns NA if the fits or
# the test fail.
.interaction_p <- function(model, data, treatment, v,
                           fitter = function(fo, dd) survival::coxph(fo, data = dd)) {
  bt  <- function(x) paste0("`", gsub("`", "", x), "`")
  rhs <- attr(stats::terms(model), "term.labels")
  resp <- deparse(stats::formula(model)[[2]])
  base <- if (v %in% gsub("`", "", rhs)) rhs else c(rhs, bt(v))
  tryCatch({
    add_f <- stats::as.formula(paste(resp, "~", paste(base, collapse = " + ")))
    int_f <- stats::as.formula(paste(resp, "~",
                paste(c(base, paste0(bt(treatment), ":", bt(v))), collapse = " + ")))
    vars <- unique(c(all.vars(add_f), v))
    dd <- data[stats::complete.cases(data[, intersect(vars, colnames(data)), drop = FALSE]), ]
    i <- fitter(int_f, dd)
    if (is.null(i$naive.var)) {                      # model-based variance
      a <- fitter(add_f, dd)
      return(stats::anova(a, i)[["Pr(>|Chi|)"]][2])
    }
    newt <- setdiff(attr(stats::terms(i), "term.labels"),
                    attr(stats::terms(add_f), "term.labels"))
    k <- unlist(i$assign[newt], use.names = FALSE)
    if (!length(k)) return(NA_real_)
    b <- stats::coef(i)[k]
    V <- stats::vcov(i)[k, k, drop = FALSE]
    if (anyNA(b) || anyNA(V)) return(NA_real_)
    stats::pchisq(drop(t(b) %*% solve(V, b)), df = length(k), lower.tail = FALSE)
  }, error = function(e) NA_real_, warning = function(w) NA_real_)
}

# A vector survival kept from the fit (weights), put back on the rows of `data`:
# coxph stores it post-na.action, and `model$na.action` says which rows it dropped.
# Those rows come back as NA, which drops them from the refits as well -- they
# were not in the model either. NULL when the two cannot be lined up.
.fit_vector <- function(x, model, data) {
  if (is.null(x)) return(NULL)
  n <- nrow(data)
  if (length(x) == n) return(as.numeric(x))
  om <- model$na.action
  if (!is.null(om) && length(x) + length(om) == n) {
    full <- rep(NA_real_, n)
    full[-as.integer(om)] <- as.numeric(x)
    return(full)
  }
  NULL
}

# TRUE when `data` is, row for row, the data `model` was fitted on -- checked by
# rebuilding the model's response from `data` and comparing it with the one the
# fit stored. Only consulted when something is being carried across from the fit,
# where a reordered or different `data` would attach a weight to the wrong
# subject. NA when there is nothing to compare against.
.data_is_fit_data <- function(model, data) {
  y <- model$y
  if (is.null(y)) return(NA)
  yd <- tryCatch(eval(stats::formula(model)[[2]], envir = data,
                      enclos = environment(stats::formula(model))),
                 error = function(e) NULL)
  if (is.null(yd)) return(NA)
  om <- model$na.action
  if (!is.null(om) && NROW(yd) == NROW(y) + length(om))
    yd <- yd[-as.integer(om), , drop = FALSE]
  if (NROW(yd) != NROW(y)) return(FALSE)
  isTRUE(all.equal(unclass(yd), unclass(y), check.attributes = FALSE, tolerance = 0))
}

# A vector the model call refers to (weights, cluster), aligned to the rows of
# `data`, or NULL. Evaluated in `data` first -- the usual `weights = w` naming a
# column -- then in the call's own environment for a vector held outside the data
# frame. A length that does not match `data` means the two do not correspond, so
# nothing is carried.
.model_call_vector <- function(q, data, env, numeric = FALSE) {
  if (is.null(q)) return(NULL)
  x <- tryCatch(eval(q, envir = data, enclos = env), error = function(e) NULL)
  if (is.null(x)) x <- tryCatch(eval(q, envir = env), error = function(e) NULL)
  if (is.null(x) || length(x) != nrow(data)) return(NULL)
  if (numeric) {
    if (!is.numeric(x)) return(NULL)
    return(as.numeric(x))
  }
  x
}

# Drop from a model formula any RHS term that is constant in `dsub` (would make
# coxph fail), while always keeping the treatment term.
.drop_constant_terms <- function(fo, dsub, keep) {
  rhs <- attr(stats::terms(fo), "term.labels")
  const <- vapply(rhs, function(t) {
    vs <- intersect(all.vars(stats::as.formula(paste("~", t))), colnames(dsub))
    length(vs) > 0 && all(vapply(vs, function(v)
      length(unique(stats::na.omit(dsub[[v]]))) < 2L, logical(1)))
  }, logical(1))
  const[rhs == keep] <- FALSE
  keepterms <- rhs[!const]
  if (length(keepterms) == 0L) keepterms <- keep
  resp <- deparse(fo[[2]])
  stats::as.formula(paste(resp, "~", paste(keepterms, collapse = " + ")))
}

# Log-spaced HR axis breaks within the data range, thinned to a readable count.
.subgroup_breaks <- function(rng) {
  cand <- 2^seq(-6, 6)
  b <- cand[cand >= rng[1] * 0.9 & cand <= rng[2] * 1.1]
  if (length(b) < 2L) b <- signif(pretty(rng, n = 4), 2)
  while (length(b) > 7L) b <- b[c(TRUE, FALSE)]   # keep every other when crowded
  b
}

# Format break / HR numbers without trailing-zero noise (0.25, 0.5, 1, 2).
.fmt_break <- function(b) sub("\\.?0+$", "", formatC(b, format = "f", digits = 3))

# "0.69 (0.55-0.87)" per estimable row, "" otherwise.
.fmt_hrci <- function(df, noDigits) {
  fmt <- paste0("%.", noDigits, "f")
  ifelse(is.na(df$hr), "",
         sprintf(paste0(fmt, " (", fmt, "-", fmt, ")"), df$hr, df$lower, df$upper))
}

# Capitalise the first letter (default header labels for unnamed subgroups).
.cap1 <- function(x) {
  ok <- nzchar(x)
  x[ok] <- paste0(toupper(substr(x[ok], 1, 1)), substring(x[ok], 2))
  x
}

# Shared theme for the label / statistics side panels: no axes, y matched to the
# forest panel so the rows line up under ggarrange(align = "h").
.subgroup_side_theme <- function(yr) {
  list(
    ggplot2::scale_y_continuous(limits = yr, expand = c(0, 0)),
    ggplot2::coord_cartesian(clip = "off"),
    ggplot2::labs(x = NULL, y = NULL),
    ggplot2::theme_void(),
    ggplot2::theme(plot.margin = ggplot2::margin(2, 2, 2, 2)))
}
