#'Diagnostic Plots for Cox Proportional Hazards Model with ggplot2
#'@description Displays diagnostics graphs presenting goodness of Cox Proportional Hazards Model fit, that
#'can be calculated with \link{coxph} function.
#'@param fit an object of class \link{coxph.object} - created with \link{coxph} function.
#'@param type the type of residuals to present on Y axis of a diagnostic plot.
#'The same as in \link{residuals.coxph}: character string indicating the type of
#'residual desired. Possible values are \code{"martingale", "deviance", "score", "schoenfeld", "dfbeta", "dfbetas"}
#' and \code{"scaledsch"}. Only enough of the string to
#' determine a unique match is required.
#' @param linear.predictions (deprecated, see \code{ox.scale}) a logical value indicating whether to show linear
#' predictions for observations (\code{TRUE}) or just indexed of observations
#' (\code{FALSE}) on X axis.
#' @param ox.scale one value from \code{c("linear.predictions", "observation.id", "time")}.
#' It defines what will be presented on OX scale. Possible values: y hat for \code{"linear.predictions"},
#' Id of an observation for \code{"observation.id"} or Time for \code{"time"}.
#'@param ... further arguments passed to \code{\link[survival]{residuals.coxph}} or
#' to the function \code{\link[ggpubr]{ggpar}} for customizing the plot.
#'@param point.col,point.size,point.shape,point.alpha color, size, shape and visibility to be used for points.
#'@param hline.col,hline.size,hline.lty,hline.alpha,hline.yintercept color, size, linetype, visibility and Y-axis coordinate to be used for \link{geom_hline}.
#'Used only when \code{hline = TRUE}.
#'@param sline.col,sline.size,sline.lty,sline.alpha color, size, linetype and visibility to be used for \link{geom_smooth}.
#'Used only when \code{sline = TRUE}.
#'@param hline a logical - should the horizontal line be added to highlight the \code{Y=0} level.
#'@param sline,sline.se a logical - should the smooth line be added to highlight the local average for residuals.
#'@param ggtheme function, ggplot2 theme name. Default value is ggplot2::theme_bw().
#'  Allowed values include ggplot2 official themes: see \code{\link[ggplot2]{theme}}.
#'@param title,subtitle,caption main title, subtitle and caption.
#'
#'@return Returns an object of class \code{ggplot}.
#'
#'@author Marcin Kosinski , \email{m.p.kosinski@@gmail.com}
#'
#'@importFrom stats predict
#'@importFrom stats residuals
#'@examples
#'
#' library(survival)
#' coxph.fit2 <- coxph(Surv(futime, fustat) ~ age + ecog.ps, data=ovarian)
#' ggcoxdiagnostics(coxph.fit2, type = "deviance")
#'
#' ggcoxdiagnostics(coxph.fit2, type = "schoenfeld", title = "Diagnostic plot")
#' ggcoxdiagnostics(coxph.fit2, type = "deviance", ox.scale = "time")
#' ggcoxdiagnostics(coxph.fit2, type = "schoenfeld", ox.scale = "time",
#'                  title = "Diagnostic plot", subtitle = "Data comes from survey XYZ",
#'                  font.subtitle = 9)
#' ggcoxdiagnostics(coxph.fit2, type = "deviance", ox.scale = "linear.predictions",
#'                  caption = "Code is available here - link", font.caption = 10)
#' ggcoxdiagnostics(coxph.fit2, type = "schoenfeld", ox.scale = "observation.id")
#' ggcoxdiagnostics(coxph.fit2, type = "scaledsch", ox.scale = "time")
#'
#'@describeIn ggcoxdiagnostics Diagnostic Plots for Cox Proportional Hazards Model with \pkg{ggplot2}
#'@export
ggcoxdiagnostics <- function (fit,
                      type = c("martingale", "deviance", "score", "schoenfeld",
                               "dfbeta", "dfbetas", "scaledsch","partial"),
                      ...,
                      linear.predictions = type %in% c("martingale", "deviance"),
                      ox.scale = ifelse(linear.predictions, "linear.predictions", "observation.id"),
                      hline = TRUE,
                      sline = TRUE, sline.se = TRUE,
                      hline.col = "red", hline.size = 1, hline.alpha = 1, hline.yintercept = 0, hline.lty = 'dashed',
                      sline.col = "blue", sline.size = 1, sline.alpha = 0.3, sline.lty = 'dashed',
                      point.col = "black", point.size = 1, point.shape = 19, point.alpha = 1,
                      title = NULL, subtitle = NULL, caption = NULL,
                      ggtheme = ggplot2::theme_bw()){

  model <- fit
  if(!methods::is(model, "coxph"))
    stop("Can't handle an object of class ", class(fit))
  type <- match.arg(type)

  res <- as.data.frame(resid(fit, type = type))
  .facet <- FALSE

  xlabel <- "The index number of observations"
  ylabel <- paste0("Residuals (type = ", type, ")" )

  switch(ox.scale,
         linear.predictions = {
           if (!(type %in% c("martingale", "deviance")))
             warning("ox.scale='linear.predictions' works only with type=martingale/deviance")
           xval <- predict(fit, type="lp")
           xlabel <- "Linear Predictions"
         },
         observation.id = {
           xval <- 1:nrow(res)
           xlabel <- "Observation Id"
         },
         time = {
           if (!(type %in% c("schoenfeld", "scaledsch")))
             warning("ox.scale='time' works only with type=schoenfeld/scaledsch")
           xval <- as.numeric(rownames(res))
           xlabel <- "Time"
         },
         {warning("ox.scale should be one of linear.predictions/observation.id/time")})

  # Case of multivariate Cox model
  if(type %in% c("martingale", "deviance")) col_names <- "residuals"
  else col_names <- names(stats::coef(fit))
  colnames(res) <- col_names
  res$xval <- xval
  data2plot <- tidyr::gather_(res,
                              key_col = "covariate", value_col = "res",
                              gather_col = col_names)

  gplot <- ggplot(aes(xval, res), data = data2plot) +
           geom_point(col = point.col, shape = point.shape,
                       size = point.size, alpha = point.alpha)

  if (hline) gplot <- gplot + geom_hline(yintercept=hline.yintercept, col = hline.col,
                                         size = hline.size, lty = hline.lty, alpha = hline.alpha)

  if (sline) gplot <- gplot + geom_smooth(col = sline.col, se = sline.se, method = "loess",
                                         size = sline.size, lty = sline.lty, alpha = sline.alpha)

  gplot <- gplot + labs(x = xlabel, y = ylabel, title = title, subtitle = subtitle, caption = caption) + ggtheme
  # customization
  gplot <- ggpubr::ggpar(gplot, ...)

  gplot <- gplot + facet_wrap(~covariate, scales = "free")
  gplot
}

