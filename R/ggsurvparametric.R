#' @include utilities.R
NULL
#'Parametric Survival Curves over Kaplan-Meier
#'
#'@description Overlays the fitted survival curve(s) of a parametric model on the
#'  Kaplan-Meier estimate, so the parametric fit can be judged against the data.
#'  Accepts a \code{\link[survival]{survreg}} or a
#'  \code{\link[flexsurv]{flexsurvreg}} model. The Kaplan-Meier is drawn as a solid
#'  step function and the fitted model as a dashed line in the same colour, one per
#'  group.
#'
#'@param fit a fitted parametric survival model: a
#'  \code{\link[survival]{survreg}} or \code{\link[flexsurv]{flexsurvreg}} object.
#'@param data the data frame used to fit the model. If \code{NULL}, it is
#'  retrieved from the model.
#'@param conf.int logical. If \code{TRUE}, add a pointwise confidence band around
#'  the parametric curve(s). Default \code{FALSE}. For \code{survreg} the band is a
#'  parametric-bootstrap band from the model's covariance; for \code{flexsurvreg}
#'  it is the model's own band.
#'@param linewidth width of the parametric line. Default 1.
#'@param conf.level confidence level for the parametric band. Default 0.95.
#'@param nsim number of draws for the \code{survreg} bootstrap band. Default 2000.
#'@param ggtheme a ggplot2 theme. Default \code{\link{theme_survminer}()}.
#'@param ... further arguments passed to \code{\link{ggsurvplot}} for the
#'  Kaplan-Meier layer (e.g. \code{palette}, \code{legend.labs}, \code{risk.table}).
#'  Note that \code{conf.int} here controls the \emph{parametric} band, not the
#'  Kaplan-Meier band.
#'
#'@details The parametric curve is drawn only over the observed follow-up; it is
#'  not extrapolated beyond the data by default. The tail of a parametric fit is
#'  driven by the distribution assumption, so compare distributions before relying
#'  on it.
#'
#'@return the \code{\link{ggsurvplot}} object, with the parametric overlay added to
#'  its \code{$plot}. The computed parametric survival data is attached as
#'  \code{attr(x$plot, "parametric")}.
#'
#'@seealso \code{\link{ggflexsurvplot}} (the flexsurv-only predecessor),
#'  \code{\link{ggsurvplot}}.
#'
#'@examples
#'\donttest{
#'library(survival)
#'
#'# Weibull model over the Kaplan-Meier, by sex
#'fit <- survreg(Surv(time, status) ~ sex, data = lung, dist = "weibull")
#'ggsurvparametric(fit, data = lung)
#'
#'# with a bootstrap confidence band
#'ggsurvparametric(fit, data = lung, conf.int = TRUE)
#'}
#'
#'@export
ggsurvparametric <- function(fit, data = NULL, conf.int = FALSE, linewidth = 1,
                         conf.level = 0.95, nsim = 2000,
                         ggtheme = theme_survminer(), ...) {

  is.survreg <- inherits(fit, "survreg")
  is.flex <- inherits(fit, "flexsurvreg")
  if (!is.survreg && !is.flex) {
    if (inherits(fit, "coxph"))
      stop("`fit` is a Cox model, which has no parametric survival curve. Use ",
           "ggadjustedcurves() for a covariate-adjusted curve.", call. = FALSE)
    if (inherits(fit, c("survfit", "survfitms")))
      stop("`fit` is already a survival estimate; use ggsurvplot() to plot it.",
           call. = FALSE)
    stop("`fit` must be a survreg or flexsurvreg model.", call. = FALSE)
  }
  if (is.flex && !requireNamespace("flexsurv", quietly = TRUE))
    stop("The flexsurv package is needed for a flexsurvreg fit.", call. = FALSE)

  data <- .get_data(fit, data = data, complain = TRUE)
  fml <- stats::formula(fit)
  rhs <- all.vars(fml[[3]])

  # ---- Kaplan-Meier base (stratifies numeric grouping correctly) ----------
  # surv_fit() keeps the formula/data on the object so ggsurvplot() can summarise
  # it even though `fml` is local here.
  km <- surv_fit(fml, data = data)
  gg <- ggsurvplot(km, data = data, ggtheme = ggtheme, ...)
  km.strata <- levels(gg$plot$data$strata)          # display labels + order
  if (length(km.strata) > 15L)
    warning("The model has ", length(km.strata), " groups (a continuous ",
            "covariate on the right-hand side?); the overlay may be unreadable. ",
            "A categorical grouping is expected.", call. = FALSE)
  # the survfit's own strata order (e.g. "sex=1","sex=2"); the KM display labels
  # correspond to these by POSITION, so the overlay aligns even when the KM was
  # relabelled with `legend.labs` or recoloured with a custom palette.
  raw.strata <- if (is.null(km$strata)) "all" else names(km$strata)

  # ---- parametric survival per group over the observed time grid ----------
  times <- seq(0, max(km$time, na.rm = TRUE), length.out = 100)
  newdata <- .parametric_newdata(data, rhs, raw.strata)
  psurv <- .parametric_surv(fit, newdata, times, conf.int, conf.level, nsim)
  psurv$strata <- factor(km.strata[psurv$.row], levels = km.strata)

  # ---- overlay: dashed same-colour line (+ optional band) ------------------
  p <- gg$plot
  if (conf.int && all(c("lower", "upper") %in% names(psurv)))
    p <- p + ggplot2::geom_ribbon(
      data = psurv,
      ggplot2::aes(x = .data$time, ymin = .data$lower, ymax = .data$upper,
                   fill = .data$strata, group = .data$strata),
      alpha = 0.15, inherit.aes = FALSE, show.legend = FALSE)
  # dashed same-colour line, with a linetype legend keying the fitted model (so the
  # solid-vs-dashed meaning is in the guide, not only a caption -- and it survives a
  # risk table). The legend key is neutral; the strata colour legend is separate.
  dist.lab <- paste0(.parametric_dist(fit), " fit")
  p <- p + ggplot2::geom_line(
    data = psurv,
    ggplot2::aes(x = .data$time, y = .data$surv, colour = .data$strata,
                 group = .data$strata, linetype = dist.lab),
    linewidth = linewidth, inherit.aes = FALSE) +
    ggplot2::scale_linetype_manual(name = NULL, values = stats::setNames("dashed", dist.lab)) +
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1),
                    linetype = ggplot2::guide_legend(order = 2))
  attr(p, "parametric") <- psurv
  gg$plot <- p
  gg
}

# a readable distribution name for the legend (survreg dist names and flexsurv
# short codes such as "weibull.quiet"/"lnorm"/"llogis" -> pretty labels).
.parametric_dist <- function(fit) {
  d <- if (inherits(fit, "survreg")) as.character(fit$dist)
       else tryCatch(fit$dlist$name, error = function(e) NULL)
  if (is.null(d) || length(d) != 1L || is.na(d)) return("parametric")
  d <- sub("\\.quiet$", "", d)
  pretty <- c(weibull = "Weibull", weibullph = "Weibull (PH)",
              exp = "Exponential", exponential = "Exponential",
              lnorm = "Log-normal", lognormal = "Log-normal",
              llogis = "Log-logistic", loglogistic = "Log-logistic",
              gaussian = "Gaussian", logistic = "Logistic",
              gengamma = "Generalised gamma", gengamma.orig = "Generalised gamma",
              gompertz = "Gompertz", gamma = "Gamma", genf = "Generalised F",
              rayleigh = "Rayleigh", extreme = "Extreme")
  if (d %in% names(pretty)) return(unname(pretty[d]))
  paste0(toupper(substr(d, 1, 1)), substr(d, 2, nchar(d)))
}

# one newdata row per survfit stratum, in the survfit strata order. Matches the
# observed covariate combinations to the survfit's "var=level, ..." strata names,
# so the overlay lines up with the KM by position regardless of covariate type
# (numeric grouping included).
.parametric_newdata <- function(data, rhs, raw.strata) {
  if (length(rhs) == 0)
    return(data.frame(row.names = seq_along(raw.strata)))
  combos <- unique(data[, rhs, drop = FALSE])
  combos <- combos[stats::complete.cases(combos), , drop = FALSE]
  labs <- apply(combos, 1, function(r)
    paste(paste0(rhs, "=", r), collapse = ", "))
  ord <- match(raw.strata, labs)
  combos[ord, , drop = FALSE]
}

# model-agnostic parametric S(t): a long data frame time, surv, .row (the newdata
# row index) and, when conf.int, lower/upper.
.parametric_surv <- function(fit, newdata, times, conf.int = FALSE,
                             conf.level = 0.95, nsim = 2000) {
  if (inherits(fit, "survreg"))
    .parametric_surv_survreg(fit, newdata, times, conf.int, conf.level, nsim)
  else
    .parametric_surv_flexsurv(fit, newdata, times, conf.int, conf.level)
}

.parametric_surv_survreg <- function(fit, newdata, times, conf.int, conf.level, nsim) {
  # S(t) = 1 - psurvreg(t, mean = linear predictor, scale = fit$scale, dist).
  # psurvreg handles each distribution's parameterisation, so no per-dist algebra.
  if (length(fit$scale) > 1L)
    stop("ggsurvparametric() does not support a survreg model with a per-stratum ",
         "scale (a strata() term); fit a common-scale model.", call. = FALSE)
  lp <- as.numeric(stats::predict(fit, newdata = newdata, type = "lp"))
  dist <- fit$dist
  scale <- fit$scale
  surv_of <- function(lp_i)
    1 - survival::psurvreg(times, mean = lp_i, scale = scale, distribution = dist)
  out <- do.call(rbind, lapply(seq_along(lp), function(i)
    data.frame(time = times, surv = surv_of(lp[i]), .row = i)))
  if (!conf.int) return(out)

  # parametric-bootstrap band: draw the parameters from the model covariance,
  # recompute S(t), take pointwise quantiles. Distribution-agnostic.
  beta <- stats::coef(fit)
  nb <- length(beta)
  V <- stats::vcov(fit)
  # survreg's vcov carries an estimated log-scale as the last row/col, UNLESS the
  # scale is fixed (exponential, rayleigh) -- then vcov is beta-only.
  scale.free <- ncol(V) > nb
  if (scale.free) {
    theta <- c(beta, log(fit$scale)); k <- nb + 1L
  } else {
    theta <- beta; k <- nb
  }
  V <- V[seq_len(k), seq_len(k), drop = FALSE]
  L <- tryCatch(t(chol(V)), error = function(e) NULL)
  a <- (1 - conf.level) / 2
  X <- stats::model.matrix(fit, data = newdata)   # design for the newdata rows
  lo <- hi <- matrix(NA_real_, length(times), nrow(newdata))
  if (!is.null(L)) {
    for (i in seq_len(nrow(newdata))) {
      thetas <- theta + L %*% matrix(stats::rnorm(nsim * k), k, nsim)
      lp_i <- as.numeric(X[i, ] %*% thetas[seq_len(nb), , drop = FALSE])
      sc_i <- if (scale.free) exp(thetas[k, ]) else rep(fit$scale, nsim)
      S <- vapply(seq_len(nsim), function(j)
        1 - survival::psurvreg(times, mean = lp_i[j], scale = sc_i[j],
                               distribution = dist), numeric(length(times)))
      lo[, i] <- apply(S, 1, stats::quantile, probs = a, na.rm = TRUE)
      hi[, i] <- apply(S, 1, stats::quantile, probs = 1 - a, na.rm = TRUE)
    }
  }
  out$lower <- as.vector(lo)
  out$upper <- as.vector(hi)
  out
}

.parametric_surv_flexsurv <- function(fit, newdata, times, conf.int, conf.level) {
  s <- summary(fit, newdata = newdata, t = times, ci = conf.int,
               cl = conf.level, tidy = TRUE)
  # summary() returns one block per newdata row in order; recover the row index
  nrow.nd <- max(1L, nrow(newdata))
  out <- data.frame(time = s$time, surv = s$est,
                    .row = rep(seq_len(nrow.nd), each = length(times)))
  if (conf.int && all(c("lcl", "ucl") %in% names(s))) {
    out$lower <- s$lcl
    out$upper <- s$ucl
  }
  out
}
