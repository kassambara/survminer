#'Graphical Test of Proportional Hazards with ggplot2
#'@description Displays a graph of the scaled Schoenfeld residuals, along with a
#'  smooth curve using \pkg{ggplot2}. Wrapper around \link{plot.cox.zph}.
#'@param fit an object of class \link{cox.zph}.
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
#'@param caption the caption of the final \link{grob} (\code{bottom} in \link{arrangeGrob})
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
                      caption = NULL,
                      ggtheme = theme_survminer(), ...){

  x <- fit
  if(!methods::is(x, "cox.zph"))
    stop("Can't handle an object of class ", class(x))

  xx <- x$x
  yy <- x$y
  d <- nrow(yy)
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
  if (se) {
    bk <- backsolve(qmat$qr[1:df, 1:df], diag(df))
    xtx <- bk %*% t(bk)
    seval <- d * ((pmat %*% xtx) * pmat) %*% rep(1, df)
  }
  ylab <- paste("Beta(t) for", dimnames(yy)[[2]])
  if (missing(var))
    var <- 1:nvar
  else {
    if (is.character(var))
      var <- match(var, dimnames(yy)[[2]])
    if (any(is.na(var)) || max(var) > nvar || min(var) <
        1)
      stop("Invalid variable requested")
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
      temp <- as.vector(2 * sqrt(x$var[i, i] * seval))
      yup <- yhat + temp
      ylow <- yhat - temp
      yr <- range(yr, yup, ylow)
    }
    if (x$transform == "identity") {
      gplot + geom_line(aes(x=pred.x, y=yhat)) +
        xlab("Time") +
        ylab(ylab[i]) +
        ylim(yr) -> gplot
    } else if (x$transform == "log") {
      gplot + geom_line(aes(x=log(pred.x), y=yhat)) +
        xlab("Time") +
        ylab(ylab[i]) +
        ylim(yr)  -> gplot
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
#' @param newpage open a new page. See \code{\link{grid.arrange}}.
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

