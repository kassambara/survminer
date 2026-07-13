#'Graphical Test of Proportional Hazards with ggplot2
#'@description Displays a graph of the scaled Schoenfeld residuals, along with a
#'  smooth curve using \pkg{ggplot2}. Wrapper around \link[survival]{plot.cox.zph}.
#'@param fit an object of class \link[survival]{cox.zph}, or a
#'  \link[survival]{coxph} model, in which case \code{\link[survival]{cox.zph}()}
#'  is run on it automatically.
#'@param resid	a logical value, if TRUE the residuals are included on the plot,
#'  as well as the smooth fit.
#'@param se a logical value, if TRUE, confidence bands at two standard errors
#'  will be added.
#'@param df	the degrees of freedom for the fitted natural spline, df=2 leads to
#'  a linear fit.
#'@param nsmo	number of points used to plot the fitted spline.
#'@param var the set of variables for which plots are desired. By default, plots
#'  are produced in turn for each variable of a model.
#'@param point.col,point.size,point.shape,point.alpha color, size, shape and visibility to be used for points.
#'@param caption the caption of the final \link[grid]{grob} (\code{bottom} in \link[gridExtra]{arrangeGrob})
#'@param var_pval optional numeric threshold; an alternative to \code{var} that
#'  selects only the terms whose Grambsch-Therneau proportional-hazards test
#'  p-value is below this value (e.g. \code{var_pval = 0.05} to plot only terms
#'  flagged for non-proportionality). Cannot be used together with \code{var}.
#'  Default \code{NULL} (all terms).
#'@param add.beta.line logical; if TRUE, overlay a horizontal reference line at
#'  the model's estimated coefficient for each term (under proportional hazards
#'  the scaled Schoenfeld residuals scatter around \eqn{\beta}). Requires a
#'  \code{coxph} model as input (a \code{cox.zph} object does not carry the
#'  coefficients). The line is drawn only for single-coefficient terms; a
#'  multi-level factor term has one panel but several coefficients, so no single
#'  reference line applies and it is left without one. Default FALSE.
#'@param beta.line.col,beta.line.type,beta.line.size color, line type and line
#'  width of the beta reference line (used when \code{add.beta.line = TRUE}).
#'@param ggtheme function, ggplot2 theme name.
#'  Allowed values include ggplot2 official themes: see \code{\link[ggplot2]{theme}}.
#'@param ... further arguments passed to either the print() function or to the \code{\link[ggpubr]{ggpar}} function for customizing the plot (see Details section).
#'@details \strong{Customizing the plots}: The plot can be easily
#'  customized using additional arguments to be passed to the function ggpar().
#'  Read ?ggpubr::ggpar. These arguments include
#'  \emph{font.main,font.submain,font.caption,font.x,font.y,font.tickslab,font.legend}:
#'  a vector of length 3 indicating respectively the size (e.g.: 14), the style
#'  (e.g.: "plain", "bold", "italic", "bold.italic") and the color (e.g.: "red")
#'  of main title, subtitle, caption, xlab and ylab and axis tick labels,
#'  respectively. For example \emph{font.x = c(14, "bold", "red")}.  Use font.x
#'  = 14, to change only font size; or use font.x = "bold", to change only font
#'  face.
#'@return Returns an object of class \code{ggcoxzph} which is a list of ggplots.
#'
#'@author Marcin Kosinski , \email{m.p.kosinski@@gmail.com}
#'
#'@examples
#'
#' library(survival)
#' fit <- coxph(Surv(futime, fustat) ~ age + ecog.ps + rx, data=ovarian)
#' cox.zph.fit <- cox.zph(fit)
#' # plot all variables
#' ggcoxzph(cox.zph.fit)
#' # plot all variables in specified order
#' ggcoxzph(cox.zph.fit, var = c("ecog.ps", "rx", "age"), font.main = 12)
#' # plot specified variables in specified order
#' ggcoxzph(cox.zph.fit, var = c("ecog.ps", "rx"), font.main = 12, caption = "Caption goes here")
#'
#'@describeIn ggcoxzph Graphical Test of Proportional Hazards using ggplot2.
#'@export
ggcoxzph <- function (fit, resid = TRUE, se = TRUE, df = 4, nsmo = 40, var,
                      point.col = "red", point.size = 1, point.shape = 19, point.alpha = 1,
                      caption = NULL, var_pval = NULL,
                      add.beta.line = FALSE, beta.line.col = "blue",
                      beta.line.type = "dashed", beta.line.size = 0.5,
                      ggtheme = theme_survminer(), ...){

  x <- fit
  # Capture the fitted coefficients for the optional beta reference line, BEFORE a
  # coxph model is turned into a cox.zph object -- a cox.zph object alone does not
  # carry the coefficients, so the line can only be drawn when a coxph model is
  # supplied (#767).
  betas <- if(methods::is(x, "coxph")) stats::coef(x) else NULL
  # Accept a coxph model directly and run the proportional-hazards test on it, so
  # users don't have to call survival::cox.zph() first. A cox.zph object (the
  # previous input) is not a coxph, so it takes the unchanged path (#410).
  if(methods::is(x, "coxph"))
    x <- survival::cox.zph(x)
  if(!methods::is(x, "cox.zph"))
    stop("Can't handle an object of class ", class(x))

  xx <- x$x
  yy <- x$y
  df <- max(df)
  nvar <- ncol(yy)
  pred.x <- seq(from = min(xx), to = max(xx), length = nsmo)
  temp <- c(pred.x, xx)
  lmat <- splines::ns(temp, df = df, intercept = TRUE)
  pmat <- lmat[1:nsmo, ]
  xmat <- lmat[-(1:nsmo), ]
  qmat <- qr(xmat)
  if (qmat$rank < df)
    stop("Spline fit is singular, try a smaller degrees of freedom")
  # NB: the pointwise SE band is computed per panel below (from `x$var[i, i]`);
  # there is no `d` (number of events) factor -- that matches survival's own
  # plot.cox.zph under survival >= 3.0.
  ylab <- paste("Beta(t) for", dimnames(yy)[[2]])
  # Variable selection. `var_pval` is an alternative to `var`: keep only the terms
  # whose Grambsch-Therneau test p-value (x$table[, 3], per-variable rows, GLOBAL
  # excluded) is below the threshold -- useful for showing just the terms flagged
  # for non-proportionality. `var` (missing -> all) is unchanged (#767).
  if (!is.null(var_pval)) {
    if (!missing(var))
      stop("Provide either `var` or `var_pval`, not both.")
    pvals <- x$table[seq_len(nvar), 3]
    var <- which(pvals < var_pval)
    if (length(var) == 0)
      stop("No variable has a proportional-hazards test p-value < ", var_pval,
           " (min is ", signif(min(pvals), 3), ").")
  }
  else if (missing(var))
    var <- 1:nvar
  else {
    if (is.character(var))
      var <- match(var, dimnames(yy)[[2]])
    if (any(is.na(var)) || max(var) > nvar || min(var) <
        1)
      stop("Invalid variable requested")
  }
  # The beta reference line needs the model coefficients, which a cox.zph object
  # (as opposed to a coxph model) does not carry. Skip with a clear note (#767).
  if (add.beta.line && is.null(betas)) {
    warning("`add.beta.line = TRUE` needs a coxph model as input; a cox.zph ",
            "object doesn't carry the coefficients, so the beta line is skipped. ",
            "Pass the coxph model to ggcoxzph() instead.", call. = FALSE)
    add.beta.line <- FALSE
  }
  if (x$transform == "log") {
    xx <- exp(xx)
    pred.x <- exp(pred.x)
  }
  else if (x$transform != "identity") {
    xtime <- as.numeric(dimnames(yy)[[1]])
    indx <- !duplicated(xx)
    apr1 <- approx(xx[indx], xtime[indx], seq(min(xx), max(xx),
                                              length = 17)[2 * (1:8)])
    temp <- signif(apr1$y, 2)
    apr2 <- approx(xtime[indx], xx[indx], temp)
    xaxisval <- apr2$y
    xaxislab <- rep("", 8)
    for (i in 1:8) xaxislab[i] <- format(temp[i])
  }
  plots <- list()
  lapply(var, function(i) {
    invisible(round(x$table[i, 3],4) -> pval)
    ggplot() + labs(title = paste0('Schoenfeld Individual Test p: ', pval)) + ggtheme -> gplot
    y <- yy[, i]
    yhat <- as.vector(pmat %*% qr.coef(qmat, y))
    if (resid)
      yr <- range(yhat, y)
    else yr <- range(yhat)
    if (se) {
      bk <- backsolve(qmat$qr[1:df, 1:df], diag(df))
      xtx <- bk %*% t(bk)
      seval <- ((pmat %*% xtx) * pmat) %*% rep(1, df)
      temp <- as.vector(2 * sqrt(x$var[i, i] * seval))
      yup <- yhat + temp
      ylow <- yhat - temp
      yr <- range(yr, yup, ylow)
    }
    # coefficient for this panel, matched by term name. A multi-column term
    # (multi-level factor, spline) is collapsed by cox.zph into one panel whose
    # name is NOT a coef() name, so betas[name] is NA and the line is skipped --
    # there is no single coefficient to draw for such a term. For a term that has
    # a line, extend the y-range so it stays visible (#767).
    beta_i <- if (add.beta.line) unname(betas[dimnames(yy)[[2]][i]]) else NA_real_
    if (!is.na(beta_i)) yr <- range(yr, beta_i)
    if (x$transform == "identity") {
      gplot + geom_line(aes(x=pred.x, y=yhat)) +
        xlab("Time") +
        ylab(ylab[i]) +
        ylim(yr) -> gplot
    } else if (x$transform == "log") {
      # pred.x is already on the original time scale (exp()-ed above), like the
      # residual points (xx) and the SE bands. Draw the fit line on that same
      # scale and put the axis on a log scale, instead of drawing the line at
      # log(pred.x) (which put the fit line on a different x-scale than the
      # points, squeezing it to the left); reproduces plot.cox.zph(log = "x")
      # (#454, #588).
      gplot + geom_line(aes(x=pred.x, y=yhat)) +
        xlab("Time") +
        ylab(ylab[i]) +
        ylim(yr) +
        scale_x_log10() -> gplot
    } else {
      gplot + geom_line(aes(x=pred.x, y=yhat)) +
        xlab("Time") +
        ylab(ylab[i]) +
        scale_x_continuous(breaks = xaxisval,
                           labels = xaxislab) +
        ylim(yr)-> gplot
    }

    if (resid)
      gplot <- gplot + geom_point(aes(x = xx, y =y),
                                  col = point.col, shape = point.shape, size = point.size, alpha = point.alpha)

    if (se) {
      gplot <- gplot + geom_line(aes(x=pred.x, y=yup), lty = "dashed") +
        geom_line(aes( x = pred.x, y = ylow), lty = "dashed")
    }

    # optional horizontal reference line at the fitted coefficient: under
    # proportional hazards the scaled Schoenfeld residuals scatter around beta (#767)
    if (!is.na(beta_i))
      gplot <- gplot + geom_hline(yintercept = beta_i, colour = beta.line.col,
                                  linetype = beta.line.type, linewidth = beta.line.size)

    ggpubr::ggpar(gplot, ...)


  }) -> plots
  names(plots) <- var
  class(plots) <- c("ggcoxzph", "ggsurv", "list")

  if("GLOBAL" %in% rownames(x$table)) # case of multivariate Cox
    global_p <- x$table["GLOBAL", 3]
  else global_p <- NULL # Univariate Cox
  attr(plots, "global_pval") <- global_p
  attr(plots, "caption") <- caption
  plots

}

#' @param x an object of class ggcoxzph
#' @param newpage open a new page. See \code{\link[gridExtra]{grid.arrange}}.
#' @method print ggcoxzph
#' @rdname ggcoxzph
#' @export
print.ggcoxzph <- function(x, ..., newpage = TRUE){
  if(!inherits(x, "ggcoxzph"))
    stop("An object of class ggcoxzph is required.")
  plots <- x
  pval <- attr(x, "global_pval")
  grobs <- widths <- list()
  for (i in 1:length(plots)) {
    grobs[[i]] <- ggplotGrob(plots[[i]])
    widths[[i]] <- grobs[[i]]$widths[2:5]
  }
  maxwidth <- do.call(grid::unit.pmax, widths)
  for (i in 1:length(grobs)) {
    grobs[[i]]$widths[2:5] <- as.list(maxwidth)
  }

  if(!is.null(pval)) main <- paste0("Global Schoenfeld Test p: ", signif(pval, 4), "\n")
  else main <- NULL

  caption <- attr(plots, "caption")

  do.call(gridExtra::grid.arrange, c(grobs, top = main, bottom = caption, newpage = newpage))
}

