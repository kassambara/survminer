#' @include utilities.R
NULL
#'Non-Proportional-Hazards Diagnostic Panel
#'
#'@description Draws a small-multiple of complementary diagnostics for the
#'  proportional-hazards (PH) assumption of a Cox model, for one covariate of
#'  interest, and -- when PH does not hold -- shows the \emph{shape} of the
#'  time-varying effect. Up to four panels are produced:
#'  \enumerate{
#'    \item \strong{cloglog} -- log cumulative hazard \eqn{\log(-\log S(t))}
#'      against log time, by group. Parallel curves are consistent with PH;
#'      converging or crossing curves indicate non-proportional hazards.
#'    \item \strong{schoenfeld} -- the scaled Schoenfeld residuals for the
#'      covariate with a natural-spline smooth and a pointwise band, an estimate
#'      of the time-varying coefficient \eqn{\beta(t)} (Grambsch and Therneau,
#'      1994). A curve flat at \eqn{\hat\beta} is consistent with PH; the
#'      Grambsch-Therneau test p-value is shown.
#'    \item \strong{hr} -- the hazard-ratio \emph{trend} \eqn{\exp(\beta(t))}
#'      from the same smooth, on a log axis, with the constant Cox hazard ratio
#'      \eqn{\exp(\hat\beta)} and \eqn{HR = 1} for reference. This shows the
#'      \emph{direction and shape} of a time-varying effect; it is a first-order
#'      residual-based diagnostic and \emph{understates the magnitude} of strong
#'      departures -- for a quantitative time-varying hazard ratio fit a model
#'      with \code{tt()} or a piecewise/interaction term.
#'    \item \strong{rmst} -- the difference in restricted mean survival time
#'      between two groups as a function of the horizon \eqn{\tau}, with a
#'      pointwise band. A band that stays away from zero as \eqn{\tau} grows
#'      indicates a durable benefit; one that shrinks toward zero indicates an
#'      effect confined to earlier times.
#'  }
#'
#'  Panels \code{cloglog} and \code{rmst} are \emph{marginal} (unadjusted, by
#'  group); \code{schoenfeld} and \code{hr} are the covariate's effect
#'  \emph{adjusted} for the model's other terms. They answer complementary
#'  questions and can disagree, which is itself informative.
#'
#'@param fit a \code{\link[survival]{coxph}} model object.
#'@param variable name of the covariate to diagnose. Required when the model has
#'  more than one term; if the model has a single term it is used by default.
#'@param data the data frame used to fit \code{fit}. Only needed when the model
#'  data cannot be reconstructed from \code{fit} automatically.
#'@param panels which panels to draw, any of \code{"cloglog"}, \code{"schoenfeld"},
#'  \code{"hr"}, \code{"rmst"}. Panels that do not apply to the chosen covariate
#'  are dropped with a message (e.g. \code{hr} needs a single-coefficient term;
#'  \code{cloglog}/\code{rmst} need groups).
#'@param transform time transform passed to \code{\link[survival]{cox.zph}} for
#'  the \code{schoenfeld}/\code{hr} panels. One of \code{"km"} (default),
#'  \code{"rank"}, \code{"identity"}, \code{"log"}. The smooth is always drawn on
#'  the real-time axis; the transform affects how event times are spaced for the
#'  fit and can change the drawn shape.
#'@param tau maximum horizon for the \code{rmst} panel. \code{NULL} (default)
#'  uses the largest admissible time.
#'@param conf.int confidence level for the pointwise bands. Default \code{0.95}.
#'@param df degrees of freedom of the natural-spline smooth of the Schoenfeld
#'  residuals. Default \code{4}; \code{df = 2} gives an almost-linear fit.
#'@param numeric.split for a numeric covariate, whether to split it at its median
#'  to form the two groups used by the \code{cloglog}/\code{rmst} panels. Default
#'  \code{FALSE} (those panels are skipped for a numeric covariate).
#'@param palette the colour palette for the group panels, passed to
#'  \code{\link[ggpubr]{ggpar}} (e.g. a name like \code{"jco"} or a colour
#'  vector).
#'@param ggtheme a ggplot2 theme. Default \code{\link{theme_survminer}()}.
#'@param title an overall title used when the panel is printed.
#'@param ... additional arguments (currently unused).
#'
#'@return an object of class \code{c("ggcoxnph", "ggsurv", "list")}: a named list
#'  of the drawn ggplots (one per surviving panel). Printing it arranges the
#'  panels into a grid. The computed data behind the panels are attached as
#'  \code{attr(x, "data")} (a list with, where applicable, \code{beta_t} and
#'  \code{rmst_tau} data frames).
#'
#'@references Grambsch, P. M. and Therneau, T. M. (1994). Proportional hazards
#'  tests and diagnostics based on weighted residuals. \emph{Biometrika},
#'  81(3), 515-526. \doi{10.1093/biomet/81.3.515}.
#'
#'  Royston, P. and Parmar, M. K. B. (2013). Restricted mean survival time: an
#'  alternative to the hazard ratio for the design and analysis of randomized
#'  trials with a time-to-event outcome. \emph{BMC Medical Research Methodology},
#'  13, 152. \doi{10.1186/1471-2288-13-152}.
#'
#'@seealso \code{\link{ggcoxzph}} for the standalone scaled-Schoenfeld plot (one
#'  panel per coefficient), \code{\link{ggcoxdiagnostics}}, \code{\link{ggrmst}}.
#'
#'@examples
#'\donttest{
#'library(survival)
#'
#'# Two-arm treatment: all four panels
#'lung2 <- lung
#'lung2$sex <- factor(lung2$sex, labels = c("Male", "Female"))
#'fit <- coxph(Surv(time, status) ~ sex + age, data = lung2)
#'ggcoxnph(fit, variable = "sex")
#'
#'# A single covariate is used by default
#'fit1 <- coxph(Surv(time, status) ~ sex, data = lung2)
#'ggcoxnph(fit1)
#'}
#'
#'@rdname ggcoxnph
#'@export
ggcoxnph <- function(fit, variable = NULL, data = NULL,
                     panels = c("cloglog", "schoenfeld", "hr", "rmst"),
                     transform = "km", tau = NULL, conf.int = 0.95, df = 4,
                     numeric.split = FALSE,
                     palette = NULL, ggtheme = theme_survminer(),
                     title = NULL, ...) {

  if (!methods::is(fit, "coxph"))
    stop("`fit` must be a coxph model.", call. = FALSE)
  panels <- match.arg(panels, several.ok = TRUE)
  z <- stats::qnorm(1 - (1 - conf.int) / 2)

  # ---- resolve the covariate and its term structure ----------------------
  term.labels <- attr(stats::terms(fit), "term.labels")
  plain <- sub("^strata\\(|\\)$", "", term.labels)
  if (is.null(variable)) {
    if (length(term.labels) != 1L)
      stop("The model has several terms; set `variable` to the covariate to ",
           "diagnose. Model terms are: ", paste(term.labels, collapse = ", "),
           ".", call. = FALSE)
    variable <- term.labels[1]
  }
  if (!variable %in% term.labels)
    stop("`variable` (\"", variable, "\") is not a model term. Model terms are: ",
         paste(term.labels, collapse = ", "), ".", call. = FALSE)
  if (grepl("^strata\\(", variable))
    stop("`variable` is a strata() term, which has no coefficient to diagnose.",
         call. = FALSE)
  if (grepl(":", variable, fixed = TRUE))
    stop("`variable` is an interaction term, which cannot be diagnosed as a ",
         "single covariate. Pick one of its main-effect terms.", call. = FALSE)

  # number of coefficients that belong to this term (>1 for a >=3-level factor,
  # spline, interaction): the Schoenfeld/HR panels need a single coefficient.
  n.coef <- sum(fit$assign[[variable]] > 0)
  if (length(n.coef) == 0 || is.na(n.coef)) n.coef <- length(fit$assign[[variable]])
  single.coef <- isTRUE(n.coef == 1L)

  # the raw covariate values (for grouping and its class). model.frame(fit)
  # reconstructs from the fit's formula environment; fall back to explicit `data`.
  mf <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
  if (is.null(mf)) {
    if (is.null(data))
      stop("Could not reconstruct the model data automatically; pass `data`.",
           call. = FALSE)
    mf <- stats::model.frame(stats::formula(fit), data = data)
  }
  if (!variable %in% names(mf))
    stop("Could not find `", variable, "` in the model frame.", call. = FALSE)
  xvar <- mf[[variable]]
  is.num <- is.numeric(xvar)

  # ---- response: require ordinary right-censored data --------------------
  resp <- stats::model.response(mf)
  right.censored <- inherits(resp, "Surv") &&
    identical(attr(resp, "type"), "right") && ncol(resp) == 2L
  time <- status <- NULL
  if (right.censored) {
    time <- as.numeric(resp[, 1]); status <- as.numeric(resp[, 2])
  }

  # ---- decide grouping for the marginal panels (cloglog, rmst) -----------
  group <- NULL; group.msg <- NULL
  if (is.num) {
    if (numeric.split) {
      med <- stats::median(xvar, na.rm = TRUE)
      lo <- "\u2264 median"; hi <- "> median"   # "<=" glyph, escaped for ASCII source
      group <- factor(ifelse(xvar <= med, lo, hi), levels = c(lo, hi))
      group.msg <- "marginal median split"
    }
  } else {
    group <- factor(xvar)
  }

  # ---- drop panels that do not apply, with a message ---------------------
  want <- panels
  drop_panel <- function(p, why) {
    if (p %in% want) {
      want <<- setdiff(want, p)
      message("ggcoxnph(): skipping the \"", p, "\" panel -- ", why, ".")
    }
  }
  if (!right.censored) {
    drop_panel("cloglog", "it needs ordinary right-censored survival data")
    drop_panel("rmst", "it needs ordinary right-censored survival data")
  }
  if (is.null(group)) {
    if (is.num && !numeric.split) {
      drop_panel("cloglog", "a numeric covariate has no groups (set numeric.split = TRUE)")
      drop_panel("rmst", "a numeric covariate has no groups (set numeric.split = TRUE)")
    }
  }
  if (!single.coef) {
    drop_panel("schoenfeld", paste0("\"", variable,
               "\" has ", n.coef, " coefficients; use ggcoxzph() for its per-contrast Schoenfeld residuals"))
    drop_panel("hr", paste0("the hazard-ratio trend needs a single-coefficient term (\"",
               variable, "\" has ", n.coef, ")"))
  }
  if (!is.null(group) && nlevels(group) > 2 && "rmst" %in% want) {
    # the RMST panel shows a between-group difference; for >2 groups it draws
    # per-group RMST(tau) curves instead (handled in the builder).
  }

  # ---- shared beta(t) smooth for schoenfeld + hr -------------------------
  beta_t <- NULL; beta_hat <- NA_real_; gt_pval <- NA_real_
  if (any(c("schoenfeld", "hr") %in% want)) {
    zph <- tryCatch(survival::cox.zph(fit, transform = transform),
                    error = function(e) e)
    if (inherits(zph, "error")) {
      # cox.zph failed (e.g. a singular tiny model): drop the Schoenfeld/HR
      # panels but keep the marginal cloglog/rmst panels if they were requested.
      drop_panel("schoenfeld", paste0("cox.zph() failed (", conditionMessage(zph), ")"))
      drop_panel("hr", paste0("cox.zph() failed (", conditionMessage(zph), ")"))
    } else {
      col <- match(variable, rownames(zph$table))
      # the term's single coefficient (index into coef(fit)); its name is e.g.
      # "sexFemale" for a factor, "age" for a numeric.
      cidx <- fit$assign[[variable]][1]
      cname <- names(stats::coef(fit))[cidx]
      beta_hat <- unname(stats::coef(fit)[cidx])
      # cox.zph$y columns are named by term; fall back to the coefficient name.
      ycol <- which(colnames(zph$y) == variable)
      if (length(ycol) != 1L) ycol <- which(colnames(zph$y) == cname)
      if (length(ycol) != 1L) ycol <- match(cidx, seq_len(ncol(zph$y)))
      if (is.na(ycol) || length(ycol) != 1L) ycol <- 1L
      gt_pval <- if (!is.na(col)) zph$table[col, ncol(zph$table)] else NA_real_
      beta_t <- tryCatch(.nph_beta_smooth(zph, ycol, df = df, nsmo = 40, z = z),
                         error = function(e) e)
      if (inherits(beta_t, "error")) {
        drop_panel("schoenfeld", conditionMessage(beta_t))
        drop_panel("hr", conditionMessage(beta_t))
        beta_t <- NULL
      }
    }
  }

  # ---- build the requested panels ---------------------------------------
  plots <- list()
  if ("cloglog" %in% want)
    plots$cloglog <- .nph_panel_cloglog(time, status, group, variable,
                                        palette, ggtheme)
  if ("schoenfeld" %in% want)
    plots$schoenfeld <- .nph_panel_schoenfeld(beta_t, beta_hat, gt_pval,
                                              variable, ggtheme)
  if ("hr" %in% want)
    plots$hr <- .nph_panel_hr(beta_t, beta_hat, variable, ggtheme)
  rmst_tau <- NULL
  if ("rmst" %in% want) {
    rp <- .nph_panel_rmst(time, status, group, tau, z, conf.int, variable,
                          palette, ggtheme, group.msg)
    plots$rmst <- rp$plot
    rmst_tau <- rp$data
  }

  if (length(plots) == 0)
    stop("No panels could be drawn for `variable` = \"", variable,
         "\". See the messages above.", call. = FALSE)

  # add A/B/C/... corner tags (only when there is more than one panel) and a
  # uniform, tile-sized title/subtitle so the panels read as one figure
  tags <- if (length(plots) > 1) LETTERS[seq_along(plots)] else rep(NA, length(plots))
  for (i in seq_along(plots))
    plots[[i]] <- plots[[i]] +
      ggplot2::labs(tag = if (is.na(tags[i])) NULL else tags[i]) +
      ggplot2::theme(
        plot.tag = ggplot2::element_text(face = "bold"),
        plot.title = ggplot2::element_text(size = 12, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 9.5, colour = "grey30"),
        plot.caption = ggplot2::element_text(size = 8, colour = "grey40"))

  attr(plots, "data") <- list(beta_t = beta_t, rmst_tau = rmst_tau,
                              beta_hat = beta_hat, gt_pval = gt_pval)
  attr(plots, "title") <- title
  attr(plots, "variable") <- variable
  class(plots) <- c("ggcoxnph", "ggsurv", "list")
  plots
}

# Natural-spline smooth of the scaled Schoenfeld residuals (an estimate of
# beta(t)), drawn on the real-time axis. Replicates survival's plot.cox.zph /
# ggcoxzph smoother (splines::ns df, QR fit, delta-method pointwise SE) and maps
# the transformed grid back to real time so several panels share one time axis.
.nph_beta_smooth <- function(zph, col, df = 4, nsmo = 40, z = 1.96) {
  xx <- zph$x
  yy <- zph$y
  d <- nrow(yy)
  df <- max(df)
  pred.x <- seq(from = min(xx), to = max(xx), length = nsmo)
  lmat <- splines::ns(c(pred.x, xx), df = df, intercept = TRUE)
  pmat <- lmat[1:nsmo, ]
  xmat <- lmat[-(1:nsmo), ]
  qmat <- qr(xmat)
  if (qmat$rank < df)
    stop("The Schoenfeld spline fit is singular; try a smaller `df`.",
         call. = FALSE)
  y <- yy[, col]
  yhat <- as.vector(pmat %*% qr.coef(qmat, y))
  bk <- backsolve(qmat$qr[1:df, 1:df], diag(df))
  xtx <- bk %*% t(bk)
  # pointwise SE of the smooth: under survival >= 3.0, cox.zph$var is already the
  # variance scale of a single scaled Schoenfeld residual, so there is no extra
  # `d` (number of events) factor -- matching survival:::plot.cox.zph.
  seval <- as.vector(((pmat %*% xtx) * pmat) %*% rep(1, df))
  se <- sqrt(zph$var[col, col] * seval)
  # map the transformed prediction grid back to real event time
  realt <- pred.x
  if (!is.null(zph$transform) && zph$transform != "identity") {
    xtime <- as.numeric(rownames(yy))
    indx <- !duplicated(xx)
    realt <- stats::approx(xx[indx], xtime[indx], xout = pred.x, rule = 2)$y
  }
  data.frame(time = realt, beta = yhat, se = se,
             lower = yhat - z * se, upper = yhat + z * se)
}

# neutral colour for the single-covariate curves (schoenfeld / hr / rmst-diff),
# so they are never confused with the group palette used by cloglog / rmst-groups.
.nph_neutral <- "grey20"

.nph_panel_cloglog <- function(time, status, group, variable, palette, ggtheme) {
  d <- data.frame(time = time, status = status, .g = group)
  d <- d[stats::complete.cases(d), , drop = FALSE]
  d$.g <- droplevels(factor(d$.g))
  km <- survival::survfit(survival::Surv(time, status) ~ .g, data = d)
  p <- ggsurvplot(km, data = d, fun = "cloglog", conf.int = FALSE,
                  palette = palette, legend.title = variable,
                  legend.labs = levels(d$.g), ggtheme = ggtheme)$plot
  # clip the empty early/late log range to where the data live
  xr <- range(d$time[d$time > 0], na.rm = TRUE)
  suppressMessages(
    p <- p + ggplot2::coord_cartesian(xlim = xr) +
      ggplot2::labs(title = "cloglog survival",
                    subtitle = "parallel = proportional hazards") +
      ggplot2::theme(legend.position = "bottom")
  )
  p
}

.nph_panel_schoenfeld <- function(beta_t, beta_hat, gt_pval, variable, ggtheme) {
  sub <- "flat = PH"
  if (!is.na(gt_pval))
    sub <- paste0(sub, "   (p = ", signif(gt_pval, 2), ")")
  # clip to the smooth and its band's central range so a wide tail band does not
  # flatten the beta(t) curve to a line
  ylim <- range(c(beta_t$beta, beta_hat,
                  stats::quantile(beta_t$lower, 0.05, na.rm = TRUE),
                  stats::quantile(beta_t$upper, 0.95, na.rm = TRUE)), na.rm = TRUE)
  p <- ggplot2::ggplot(beta_t, ggplot2::aes(x = .data$time)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
                         fill = .nph_neutral, alpha = 0.15) +
    ggplot2::geom_line(ggplot2::aes(y = .data$beta), colour = .nph_neutral,
                       linewidth = 0.9) +
    ggplot2::coord_cartesian(ylim = ylim) +
    ggtheme +
    ggplot2::labs(title = paste0("Schoenfeld residuals: ", variable),
                  subtitle = sub, x = "Time",
                  y = bquote(beta(t)))
  if (!is.na(beta_hat))
    p <- p + ggplot2::geom_hline(yintercept = beta_hat, colour = .nph_neutral,
                                 linetype = "dashed", linewidth = 0.4)
  p
}

.nph_panel_hr <- function(beta_t, beta_hat, variable, ggtheme) {
  d <- data.frame(time = beta_t$time,
                  hr = exp(beta_t$beta),
                  lower = exp(beta_t$lower),
                  upper = exp(beta_t$upper))
  # The delta-method band on exp(beta(t)) can span many orders of magnitude where
  # events are sparse, which would crush the trend line. Clip the y-axis to a
  # readable window around the estimate (and the two reference lines); the ribbon
  # runs to the panel edges to signal the uncertainty without dominating the axis.
  anchor <- c(d$hr, 1, if (!is.na(beta_hat)) exp(beta_hat))
  ylim <- range(anchor, na.rm = TRUE)
  ylim <- c(min(ylim) / 5, max(ylim) * 5)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$time)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
                         fill = .nph_neutral, alpha = 0.08) +
    ggplot2::geom_hline(yintercept = 1, colour = "grey55", linewidth = 0.5) +
    ggplot2::geom_line(ggplot2::aes(y = .data$hr), colour = .nph_neutral,
                       linewidth = 0.9) +
    ggplot2::scale_y_log10() +
    ggplot2::coord_cartesian(ylim = ylim) +
    ggtheme +
    ggplot2::labs(title = paste0("Hazard-ratio trend: ", variable),
                  subtitle = "shape over time, not magnitude",
                  x = "Time", y = "HR(t)  (log scale)",
                  caption = if (!is.na(beta_hat))
                    "grey: HR = 1   dashed: Cox HR" else "grey: HR = 1")
  if (!is.na(beta_hat))
    p <- p + ggplot2::geom_hline(yintercept = exp(beta_hat), colour = .nph_neutral,
                                 linetype = "dashed", linewidth = 0.4)
  p
}

# RMST at each tau for one arm, from a single survfit -- mirrors ggrmst's
# validated `.rmst_one_arm` exact-KM integration and Greenwood-type variance.
.nph_rmst_at <- function(tt, Y, d, S, tau) {
  keep <- tt <= tau
  ttk <- tt[keep]; Yk <- Y[keep]; dk <- d[keep]; Sk <- S[keep]
  gt <- c(0, ttk, tau)
  gS <- c(1, Sk, if (length(Sk)) Sk[length(Sk)] else 1)
  w <- diff(gt); Sv <- gS[-length(gS)]
  rmst <- sum(Sv * w)
  seg <- Sv * w
  A.start <- rev(cumsum(rev(seg)))
  A.i <- A.start[match(ttk, gt)]
  var <- sum(ifelse(dk > 0 & Yk > dk, A.i^2 * dk / (Yk * (Yk - dk)), 0))
  c(rmst = rmst, var = var)
}

.nph_rmst_curve <- function(time, status, taus) {
  f <- survival::survfit(survival::Surv(time, status) ~ 1)
  tt <- f$time; Y <- f$n.risk; d <- f$n.event; S <- f$surv
  out <- vapply(taus, function(tu) .nph_rmst_at(tt, Y, d, S, tu),
                numeric(2))
  data.frame(tau = taus, rmst = out["rmst", ], var = out["var", ])
}

.nph_panel_rmst <- function(time, status, group, tau, z, conf.int, variable,
                            palette, ggtheme, group.msg) {
  d <- data.frame(time = time, status = status, .g = group)
  d <- d[stats::complete.cases(d), , drop = FALSE]
  d$.g <- droplevels(factor(d$.g))
  levs <- levels(d$.g)

  # admissible tau: min over groups of the largest time (Inf if last event)
  tmax <- min(vapply(levs, function(g) {
    tg <- d$time[d$.g == g]; sg <- d$status[d$.g == g]
    m <- max(tg)
    if (all(sg[tg == m] == 1)) Inf else m
  }, numeric(1)))
  if (!is.finite(tmax)) tmax <- max(d$time, na.rm = TRUE)
  if (is.null(tau)) {
    tau <- tmax
  } else if (tau > tmax) {
    warning("`tau` (", signif(tau, 4), ") exceeds the largest admissible horizon (",
            signif(tmax, 4), "); using ", signif(tmax, 4), ".", call. = FALSE)
    tau <- tmax
  }
  taus <- seq(tau / 20, tau, length.out = 40)

  curves <- lapply(levs, function(g) {
    sub <- d[d$.g == g, , drop = FALSE]
    cbind(.nph_rmst_curve(sub$time, sub$status, taus), .g = g)
  })
  base_sub <- if (is.null(group.msg))
    "clear of 0 = durable benefit"
  else paste0("clear of 0 = durable benefit (", group.msg, ")")

  if (length(levs) == 2L) {
    a <- curves[[1]]; b <- curves[[2]]
    diff <- data.frame(tau = taus,
                       d = b$rmst - a$rmst,
                       se = sqrt(a$var + b$var))
    diff$lower <- diff$d - z * diff$se
    diff$upper <- diff$d + z * diff$se
    p <- ggplot2::ggplot(diff, ggplot2::aes(x = .data$tau)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
                           fill = .nph_neutral, alpha = 0.15) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.5) +
      ggplot2::geom_line(ggplot2::aes(y = .data$d), colour = .nph_neutral,
                         linewidth = 0.9) +
      ggtheme +
      ggplot2::labs(title = "RMST difference vs horizon",
                    subtitle = base_sub,
                    x = expression("Horizon " * tau),
                    y = paste0("RMST(", levs[2], " - ", levs[1], ")"))
    dat <- do.call(rbind, curves)
    dat$type <- "group"
    diff$type <- "difference"
    out.data <- list(groups = dat, difference = diff)
  } else {
    dat <- do.call(rbind, curves)
    grp_sub <- "diverging curves = growing group difference"
    if (!is.null(group.msg)) grp_sub <- paste0(grp_sub, " (", group.msg, ")")
    p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$tau, y = .data$rmst,
                                           colour = .data$.g)) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggtheme +
      ggplot2::labs(title = "RMST vs horizon", subtitle = grp_sub,
                    x = expression("Horizon " * tau), y = "RMST", colour = variable)
    p <- .nph_apply_palette(p, palette, length(levs))
    out.data <- list(groups = dat)
  }
  list(plot = p, data = out.data)
}

.nph_apply_palette <- function(p, palette, n) {
  if (is.null(palette)) return(p)
  suppressWarnings(suppressMessages(ggpubr::ggpar(p, palette = palette)))
}

#' @param x a \code{ggcoxnph} object.
#' @param newpage logical; whether to draw on a fresh graphics page.
#' @method print ggcoxnph
#' @rdname ggcoxnph
#' @export
print.ggcoxnph <- function(x, ..., newpage = TRUE) {
  n <- length(x)
  ncol <- if (n <= 1) 1 else 2
  # pull one shared group legend (the by-group panels share it) and strip the
  # per-panel legends, so there is a single legend and the panels align in height
  leg <- .nph_extract_legend(x)
  panels <- lapply(x, function(p) p + ggplot2::theme(legend.position = "none"))
  grobs <- lapply(panels, ggplot2::ggplotGrob)
  if (length(grobs) > 1) {
    maxw <- do.call(grid::unit.pmax, lapply(grobs, function(g) g$widths[2:5]))
    for (i in seq_along(grobs)) grobs[[i]]$widths[2:5] <- maxw
  }
  g <- gridExtra::arrangeGrob(grobs = grobs, ncol = ncol,
                              top = attr(x, "title"))
  if (!is.null(leg))
    g <- gridExtra::arrangeGrob(g, leg, ncol = 1,
                                heights = grid::unit.c(grid::unit(1, "null"),
                                                       grid::unit(2.2, "lines")))
  if (newpage) grid::grid.newpage()
  grid::grid.draw(g)
  invisible(x)
}

# extract a single group legend (as a grob) from whichever panel carries one;
# returns NULL if no panel has a group legend.
.nph_extract_legend <- function(plots) {
  for (p in plots) {
    g <- ggplot2::ggplotGrob(p + ggplot2::theme(legend.position = "bottom"))
    idx <- which(grepl("guide-box", vapply(g$grobs,
                       function(gr) if (is.null(gr$name)) "" else gr$name,
                       character(1))))
    for (i in idx) {
      gb <- g$grobs[[i]]
      if (inherits(gb, "gtable") && length(gb$grobs) > 0 &&
          any(vapply(gb$grobs, function(z) !inherits(z, "zeroGrob"), logical(1))))
        return(gb)
    }
  }
  NULL
}

#' @param recording logical; passed by \code{grid.draw} (unused here).
#' @method grid.draw ggcoxnph
#' @rdname ggcoxnph
#' @export
grid.draw.ggcoxnph <- function(x, recording = TRUE) print(x, newpage = FALSE)
