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
#' @param conf.int the confidence level for the intervals. Default 0.95.
#' @param show.overall logical; add an overall (all-subjects) treatment hazard
#'   ratio row at the top. Default \code{TRUE}.
#' @param show.pinteraction logical; show the per-variable interaction p-value.
#'   Default \code{TRUE}.
#' @param show.n logical; show a "No. of patients (\%)" column giving the number of
#'   subjects in each subgroup level (and their percentage of the total). Default
#'   \code{TRUE}. This count is the subgroup size; when the model has adjusting
#'   covariates with missing values it can exceed the complete-case sample the
#'   hazard ratio is actually fit on.
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
                              subgroups, conf.int = 0.95,
                              show.overall = TRUE, show.pinteraction = TRUE,
                              show.n = TRUE, favours = NULL,
                              point.size.by.precision = TRUE,
                              main = "Treatment effect by subgroup",
                              xlab = NULL, noDigits = 2,
                              ggtheme = theme_survminer()) {

  tab <- .subgroup_forest_table(model, data, treatment, subgroups,
                                conf.int, show.overall)
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
                    conf.int * 100)

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
  hrci.panel <- ggplot2::ggplot(est, ggplot2::aes(x = 1, y = y)) +
    ggplot2::geom_text(ggplot2::aes(label = hrci), hjust = 1, size = 3.3) +
    ggplot2::annotate("text", x = 1, y = head.y, label = "HR (95% CI)",
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
  widths <- c(widths, 3.4, 0.5 + 0.085 * max(nchar(c(est$hrci, "HR (95% CI)"))))
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
                                    conf.int = 0.95, show.overall = TRUE) {
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

  alpha <- 1 - conf.int
  tkey  <- .treatment_key(model, treatment, data)   # coef name + labels

  # per-subset treatment HR/CI/precision from a refit of the model formula
  one <- function(dsub) {
    fo <- .drop_constant_terms(stats::formula(model), dsub, keep = treatment)
    fit <- tryCatch(survival::coxph(fo, data = dsub),
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
    hrow$pint <- .interaction_p(model, data, treatment, v)
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

# Likelihood-ratio interaction p-value for one subgroup variable: additive
# (model + subgroup) vs interaction (+ treatment:subgroup), on the shared
# complete cases. Names are back-quoted so non-syntactic variable names do not
# break the formula. Returns NA if either fit fails.
.interaction_p <- function(model, data, treatment, v) {
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
    a <- survival::coxph(add_f, data = dd); i <- survival::coxph(int_f, data = dd)
    stats::anova(a, i)[["Pr(>|Chi|)"]][2]
  }, error = function(e) NA_real_, warning = function(w) NA_real_)
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
