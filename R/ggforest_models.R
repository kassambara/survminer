#' @include utilities.R ggforest_subgroup.R
NULL
#' Forest Plot Comparing One Term Across Several Cox Models
#'
#' @description Draws a forest plot that compares the hazard ratio of a single
#'  covariate across several \code{\link[survival]{coxph}} models -- one row per
#'  model. It is the tool for the crude-versus-adjusted (or naive-versus-time-varying)
#'  comparison: how a covariate's effect changes as the model specification changes.
#'
#'  It is the model-wise sibling of \code{\link{ggforest}} (rows are the terms of one
#'  model) and \code{\link{ggforest_subgroup}} (rows are subgroups).
#'
#' @details The hazard ratio, confidence interval and Wald p-value for \code{term}
#'  are read from each model with base \pkg{survival} (\code{coef()} / \code{vcov()}),
#'  so a robust (\code{cluster()}) variance is used when present. \code{term} is a
#'  model \emph{variable} name (e.g. \code{"age"}, \code{"sex"}); for a factor with
#'  more than one non-reference level it must resolve to a single coefficient,
#'  otherwise an error lists the coefficient names to choose from (e.g.
#'  \code{"rxLev"}). A model that does not contain \code{term} is dropped with a
#'  warning.
#'
#'  Because the models may be fitted on different data or with different
#'  specifications, one model's very wide confidence interval could otherwise crush
#'  the shared axis and make the others incomparable. The interval whiskers are
#'  therefore clamped to a robust window around the point estimates, with an arrow
#'  drawn where a whisker is clipped; the full numeric interval always appears in the
#'  "HR (CI)" column, so nothing is hidden.
#'
#' @param models a named list of \code{\link[survival]{coxph}} models. The list
#'  names are used as the row labels (falling back to \code{model.names}, then
#'  "Model 1", "Model 2", ...).
#' @param term the model variable (or a single coefficient name) whose hazard ratio
#'  is compared across the models.
#' @param conf.int the confidence level for the intervals. Default 0.95.
#' @param model.names optional character vector of row labels, used when the
#'  \code{models} list is unnamed.
#' @param show.p logical. Show the per-model Wald p-value column. Default TRUE.
#' @param show.n logical. Show the events / N column (models fitted on different data
#'  have different sizes). Default TRUE.
#' @param favours optional length-2 character vector, e.g.
#'  \code{c("Favours treatment", "Favours control")}, annotated under the reference
#'  line.
#' @param point.size.by.precision logical. Scale the point size by the estimate's
#'  precision (1/se). Default FALSE -- with few model rows a size difference is easily
#'  misread (a more-adjusted model has a larger standard error, hence a smaller box).
#' @param main plot title. Default "Hazard ratio comparison".
#' @param xlab x-axis label. Default \code{NULL} builds one from \code{term} and the
#'  confidence level; for a factor coefficient (e.g. \code{"sexFemale"}) pass a
#'  friendlier label such as \code{"Hazard ratio (female vs male)"}.
#' @param noDigits number of digits for the hazard ratio and CI. Default 2.
#' @param ggtheme a \pkg{ggplot2} theme. Default \code{\link{theme_survminer}()}.
#'
#' @return a \code{ggplot} (an assembled multi-panel figure).
#'
#' @seealso \code{\link{ggforest}}, \code{\link{ggforest_subgroup}}.
#'
#' @examples
#' library(survival)
#' m.crude    <- coxph(Surv(time, status) ~ age, data = lung)
#' m.adjusted <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data = lung)
#' ggforest_models(list("Crude" = m.crude, "Adjusted" = m.adjusted), term = "age")
#'
#' @export
ggforest_models <- function(models, term, conf.int = 0.95, model.names = NULL,
                            show.p = TRUE, show.n = TRUE, favours = NULL,
                            point.size.by.precision = FALSE,
                            main = "Hazard ratio comparison", xlab = NULL,
                            noDigits = 2, ggtheme = theme_survminer()) {

  if (!is.list(models) || !length(models))
    stop("`models` must be a non-empty list of coxph models.", call. = FALSE)
  if (missing(term) || !is.character(term) || length(term) != 1L)
    stop("`term` must be a single model variable (or coefficient) name.", call. = FALSE)
  bad <- which(!vapply(models, inherits, logical(1), "coxph"))
  if (length(bad))
    stop("Every element of `models` must be a coxph model; element(s) ",
         paste(bad, collapse = ", "), " are not.", call. = FALSE)

  tab <- .models_forest_table(models, term, conf.int, model.names)
  if (!nrow(tab))
    stop("No model contained `", term, "`; nothing to plot.", call. = FALSE)

  # y positions: first model at the top
  tab$y <- rev(seq_len(nrow(tab)))
  head.y <- max(tab$y) + 1.4
  yr <- c(min(tab$y) - 1, head.y + 0.3)
  if (!is.null(favours) && length(favours) == 2L) yr[1] <- yr[1] - 0.6

  if (is.null(xlab))
    xlab <- sprintf("Hazard ratio for %s (%g%% CI, log scale)", term, conf.int * 100)

  # robust drawing window from the point HRs, so one wide CI cannot dominate the
  # shared axis; clip whiskers to it and flag the clipped ends with an arrow.
  dr <- !is.na(tab$hr) & is.finite(tab$hr)
  if (!any(dr)) {                      # every model's term aliased/non-finite
    warning("ggforest_models(): no model has an estimable hazard ratio for `",
            term, "`; drawing the table without points.", call. = FALSE)
    win <- c(1 / 2.2, 2.2)
  } else {
    lr <- log(tab$hr[dr])
    pad <- max(diff(range(lr)) * 0.65, log(2.2))
    win <- exp(c(min(lr) - pad, max(lr) + pad))
    win[1] <- min(win[1], 1 / 1.05); win[2] <- max(win[2], 1.05)
  }
  tab$lo.clip <- pmax(tab$lower, win[1])
  tab$hi.clip <- pmin(tab$upper, win[2])
  tab$arrow.lo <- !is.na(tab$lower) & tab$lower < win[1]
  tab$arrow.hi <- !is.na(tab$upper) & tab$upper > win[2]
  if (any(tab$arrow.lo | tab$arrow.hi, na.rm = TRUE))
    warning("ggforest_models(): a confidence interval extends beyond the plotted ",
            "range and is drawn clipped with an arrow; the full interval is in the ",
            "HR (CI) column.", call. = FALSE)

  breaks <- .subgroup_breaks(win)
  if (point.size.by.precision && any(is.finite(tab$prec) & tab$prec > 0)) {
    w <- tab$prec; w[!is.finite(w) | w <= 0] <- NA
    tab$psize <- 2.5 + 2.8 * sqrt(w / max(w, na.rm = TRUE))
    tab$psize[is.na(tab$psize)] <- 3.2
  } else tab$psize <- 3.4

  est <- tab[dr, , drop = FALSE]
  hr <- lo.clip <- hi.clip <- y <- psize <- NULL
  forest <- ggplot2::ggplot(est, ggplot2::aes(x = hr, y = y)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey55") +
    ggplot2::geom_segment(ggplot2::aes(x = lo.clip, xend = hi.clip, y = y, yend = y),
                          colour = "grey25") +
    ggplot2::geom_point(ggplot2::aes(size = psize), shape = 15, colour = "grey15") +
    ggplot2::scale_size_identity() +
    ggplot2::scale_x_log10(breaks = breaks, labels = .fmt_break(breaks)) +
    ggplot2::coord_cartesian(xlim = win, clip = "off") +
    ggplot2::scale_y_continuous(limits = yr, expand = c(0, 0)) +
    ggplot2::labs(x = xlab, y = NULL) +
    ggtheme +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank())
  # overflow arrows at clipped whisker ends
  .arr <- grid::arrow(length = grid::unit(0.06, "inches"), type = "closed")
  if (any(est$arrow.hi))
    forest <- forest + ggplot2::geom_segment(
      data = est[est$arrow.hi, , drop = FALSE],
      ggplot2::aes(x = hr, xend = hi.clip, y = y, yend = y),
      colour = "grey25", arrow = .arr)
  if (any(est$arrow.lo))
    forest <- forest + ggplot2::geom_segment(
      data = est[est$arrow.lo, , drop = FALSE],
      ggplot2::aes(x = hr, xend = lo.clip, y = y, yend = y),
      colour = "grey25", arrow = .arr)
  if (!is.null(favours) && length(favours) == 2L) {
    yf <- min(tab$y) - 1.1
    forest <- forest +
      ggplot2::annotate("text", x = 1 / 1.08, y = yf, label = favours[1],
                        hjust = 1, size = 3, fontface = "italic", colour = "grey35") +
      ggplot2::annotate("text", x = 1.08, y = yf, label = favours[2],
                        hjust = 0, size = 3, fontface = "italic", colour = "grey35")
  }

  # ---- side panels (no axes, shared y) ----------------------------------------
  void <- .subgroup_side_theme(yr)
  text <- nlab <- hrci <- plab <- NULL

  lab.df <- data.frame(y = tab$y, text = tab$model, stringsAsFactors = FALSE)
  label.panel <- ggplot2::ggplot(lab.df, ggplot2::aes(x = 0, y = y)) +
    ggplot2::geom_text(ggplot2::aes(label = text), hjust = 0, size = 3.3) +
    ggplot2::annotate("text", x = 0, y = head.y, label = "Model",
                      hjust = 0, fontface = "bold", size = 3.15) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) + void
  panels <- list(label.panel)
  widths <- 0.6 + 0.085 * max(nchar(c(lab.df$text, "Model")))

  if (isTRUE(show.n) && any(!is.na(tab$n))) {
    n.df <- tab[!is.na(tab$n), c("y", "n", "nevent"), drop = FALSE]
    n.df$nlab <- sprintf("%d/%d", n.df$nevent, n.df$n)
    n.panel <- ggplot2::ggplot(n.df, ggplot2::aes(x = 1, y = y)) +
      ggplot2::geom_text(ggplot2::aes(label = nlab), hjust = 1, size = 3.2) +
      ggplot2::annotate("text", x = 1, y = head.y, label = "Events/N",
                        hjust = 1, fontface = "bold", size = 3.15) +
      ggplot2::scale_x_continuous(limits = c(0, 1),
                                  expand = ggplot2::expansion(mult = c(0.04, 0.02))) + void
    panels <- c(panels, list(n.panel))
    widths <- c(widths, 0.5 + 0.095 * max(nchar(c(n.df$nlab, "Events/N"))))
  }

  ci.header <- sprintf("HR (%g%% CI)", conf.int * 100)
  tab$hrci <- .fmt_hrci(tab, noDigits)
  hrci.panel <- ggplot2::ggplot(tab, ggplot2::aes(x = 1, y = y)) +
    ggplot2::geom_text(ggplot2::aes(label = hrci), hjust = 1, size = 3.3) +
    ggplot2::annotate("text", x = 1, y = head.y, label = ci.header,
                      hjust = 1, fontface = "bold", size = 3.15) +
    ggplot2::scale_x_continuous(limits = c(0, 1),
                                expand = ggplot2::expansion(mult = c(0.04, 0.02))) + void
  panels <- c(panels, list(forest, hrci.panel))
  widths <- c(widths, 3.4, 0.5 + 0.085 * max(nchar(c(tab$hrci, ci.header))))

  if (isTRUE(show.p)) {
    tab$plab <- ifelse(is.na(tab$p), "",
                       format.pval(tab$p, digits = noDigits, eps = 1e-3))
    p.panel <- ggplot2::ggplot(tab, ggplot2::aes(x = 0.5, y = y)) +
      ggplot2::geom_text(ggplot2::aes(label = plab), hjust = 0.5, size = 3.05,
                         fontface = "italic", colour = "grey30") +
      ggplot2::annotate("text", x = 0.5, y = head.y, label = "P",
                        hjust = 0.5, fontface = "bold", size = 3.15, colour = "grey20") +
      ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) + void
    panels <- c(panels, list(p.panel))
    widths <- c(widths, 0.5 + 0.08 * max(nchar(c(tab$plab, "P"))))
  }

  p <- ggpubr::ggarrange(plotlist = panels, nrow = 1, widths = widths, align = "h")
  if (!is.null(main) && nzchar(main))
    p <- ggpubr::annotate_figure(
      p, top = ggpubr::text_grob(main, face = "bold", size = 13))
  p
}


# Extract one term's HR / CI / p from each coxph model (base survival, robust-safe).
# Returns a data.frame of estimable + dropped rows (one per kept model).
.models_forest_table <- function(models, term, conf.int, model.names) {
  # Row labels: list names, filling only the missing ones (a partially named list
  # keeps its real names), then model.names, then "Model i".
  labs <- names(models); if (is.null(labs)) labs <- rep("", length(models))
  fill <- if (!is.null(model.names)) as.character(model.names)
          else paste("Model", seq_along(models))
  empty <- !nzchar(labs); labs[empty] <- fill[empty]
  labs <- make.unique(labs)
  z <- stats::qnorm(1 - (1 - conf.int) / 2)

  rows <- list(); classes <- character(); coef.names <- character()
  for (i in seq_along(models)) {
    m <- models[[i]]
    # term is a model variable name (via `assign`, backtick-tolerant either way) or,
    # as an escape hatch, an exact coefficient name.
    idx <- m$assign[[term]]
    if (is.null(idx)) idx <- m$assign[[gsub("`", "", term)]]
    if (is.null(idx)) idx <- m$assign[[paste0("`", term, "`")]]
    if (is.null(idx)) idx <- which(names(stats::coef(m)) == term)  # exact coef
    if (length(idx) == 0L) {
      warning("ggforest_models(): model \"", labs[i], "\" does not contain `",
              term, "`; dropped.", call. = FALSE)
      next
    }
    if (length(idx) > 1L)
      stop("`term = \"", term, "\"` matches several coefficients in model \"",
           labs[i], "\": ", paste(names(stats::coef(m))[idx], collapse = ", "),
           ". Pass one of them.", call. = FALSE)
    b <- stats::coef(m)[idx]; se <- sqrt(diag(stats::vcov(m))[idx])
    coef.names <- c(coef.names, names(stats::coef(m))[idx])
    dcl <- attr(m$terms, "dataClasses")
    classes <- c(classes, if (!is.null(dcl) && term %in% names(dcl)) dcl[[term]] else NA)
    if (is.na(b) || is.na(se) || !is.finite(b) || !is.finite(se)) {
      rows[[length(rows) + 1L]] <- data.frame(
        model = labs[i], hr = NA_real_, lower = NA_real_, upper = NA_real_,
        p = NA_real_, prec = NA_real_, n = m$n, nevent = m$nevent,
        stringsAsFactors = FALSE)
      next
    }
    rows[[length(rows) + 1L]] <- data.frame(
      model = labs[i], hr = exp(b), lower = exp(b - z * se), upper = exp(b + z * se),
      p = 2 * stats::pnorm(-abs(b / se)), prec = 1 / se, n = m$n, nevent = m$nevent,
      stringsAsFactors = FALSE)
  }
  cl <- stats::na.omit(classes)
  if (length(unique(cl)) > 1L)
    warning("ggforest_models(): `", term, "` has different types across models (",
            paste(unique(cl), collapse = ", "), "); the hazard ratios may not be ",
            "comparable.", call. = FALSE)
  # A factor coded with a different reference level across models resolves to a
  # different coefficient (e.g. sexFemale vs sexMale -> reciprocal HRs); warn so the
  # rows are not silently read as comparable.
  if (length(unique(coef.names)) > 1L)
    warning("ggforest_models(): `", term, "` resolves to different coefficients ",
            "across models (", paste(unique(coef.names), collapse = ", "),
            "); the hazard ratios may not be comparable (different reference level?).",
            call. = FALSE)
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}
