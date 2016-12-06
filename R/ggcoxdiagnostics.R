#'Diagnostic Plots for Cox Proportional Hazards Model with \pkg{ggplot2}
#'@description Displays diagnostics graphs presenting goodness of Cox Proportional Hazards Model fit, that
#'can be calculated with \link{coxph} function.
#'@param fit an object of class \link{coxph.object} - created with \link{coxph} function.
#'@param type the type of residuals to present on Y axis of a diagnostic plot.
#'The same as in \link{residuals.coxph}: character string indicating the type of
#'residual desired. Possible values are \code{"martingale", "deviance", "score", "schoenfeld", "dfbeta", "dfbetas"}
#' and \code{"scaledsch"}. Only enough of the string to
#' determine a unique match is required.
#' @param linear.predictions a logical value indicating whether to show linear
#' predictions for observations (\code{TRUE}) or just indexed of observations
#' (\code{FALSE}) on X axis.
#'@param ... furthere arguments passed to \link{residuals.coxph}.
#'@param point.col,point.size,point.shape,point.alpha color, size, shape and visibility to be used for points.
#'@param hline.col,hline.size,hline.lty,hline.alpha,hline.yintercept color, size, linetype, visibility and Y-axis coordinate to be used for \link{geom_hline}.
#'Used only when \code{hline = TRUE}.
#'@param hline a logical - should the horizontal line be added to highlight the \code{Y=0} level.
#'@param ggtheme function, ggplot2 theme name. Default value is ggplot2::theme_bw().
#'  Allowed values include ggplot2 official themes: see \code{\link[ggplot2]{theme}}.
#'@param font.main,font.x,font.y,font.tickslab a vector of length 3
#'  indicating respectively the size (e.g.: 14), the style (e.g.: "plain",
#'  "bold", "italic", "bold.italic") and the color (e.g.: "red") of main title,
#'  xlab and ylab and axis tick labels, respectively. For example \emph{font.x =
#'  c(14, "bold", "red")}.  Use font.x = 14, to change only font size; or use
#'  font.x = "bold", to change only font face.
#'
#'@return Returns an object of class \code{ggplot}.
#'
#'@author Marcin Kosinski , \email{m.p.kosinski@@gmail.com}
#'
#'@importFrom stats predict
#'@importFrom stats residuals
#'@examples
#'
#'if(require(RTCGA.clinical)){
#' # TCGA data exmaple (http://cancergenome.nih.gov/)
#' # source("https://bioconductor.org/biocLite.R")
#' # biocLite("RTCGA.clinical") # data for examples
#' # library(RTCGA.clinical) # also loads 'RTCGA' package
#' survivalTCGA(BRCA.clinical, OV.clinical,
#'             extract.cols = c("admin.disease_code", "patient.days_to_birth")) -> BRCAOV.survInfo
#' BRCAOV.survInfo$age  <- round((-as.numeric(BRCAOV.survInfo$patient.days_to_birth))/365,2)
#'
#' library(survival)
#' coxph.fit <- coxph(Surv(times, patient.vital_status) ~ admin.disease_code + age,
#'               data = BRCAOV.survInfo)
#'
#' ggcoxdiagnostics(coxph.fit, ggtheme = theme_dark(), point.col = "white", point.aplha = 0.3)
#' ggcoxdiagnostics(coxph.fit, ggtheme = theme_RTCGA(), # library(RTCGA.clinical)
#'  type = "deviance") + ylab('Deviance Residuals')
#' ggcoxdiagnostics(coxph.fit, ggtheme = theme_light(), linear.predictions = FALSE)
#' # ggcoxdiagnostics(coxph.fit, ggtheme = theme_void(), type = "deviance", linear.predictions = FALSE)
#' }
#'
#' # traditional example
#' library(survival)
#' coxph.fit2 <- coxph(Surv(futime, fustat) ~ age + ecog.ps, data=ovarian)
#' ggcoxdiagnostics(coxph.fit2, type = "deviance")
#'
#'@describeIn ggcoxdiagnostics Diagnostic Plots for Cox Proportional Hazards Model with \pkg{ggplot2}
#'@export
ggcoxdiagnostics <- function (fit,
                      type = c("martingale", "deviance", "score", "schoenfeld",
                               "dfbeta", "dfbetas", "scaledsch","partial"),
                      ...,
                      linear.predictions = TRUE,
                      hline = TRUE,
                      hline.col = "red", hline.size = 1, hline.alpha = 1, hline.yintercept = 0, hline.lty = 'dashed',
                      point.col = "black", point.size = 1, point.shape = 19, point.alpha = 1,
                      font.main = c(16, "plain", "black"),
                      font.x = c(14, "plain", "black"), font.y = c(14, "plain", "black"),
                      font.tickslab = c(12, "plain", "black"),
                      ggtheme = ggplot2::theme_bw()){

  model <- fit
  if(!methods::is(model, "coxph"))
    stop("Can't handle an object of class ", class(fit))
  type <- match.arg(type)

  res <- as.data.frame(resid(fit, type = type))
  .facet <- FALSE

  xlabel <- "The index number of observations"
  ylabel <- paste0("Residuals (type = ", type, ")" )

  if(linear.predictions){
    xval <- predict(fit, type="lp")
    xlabel <- "Linear Predictions"
  }else xval <- 1:fit$n

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

  gplot <- gplot + labs(x = xlabel, y = ylabel) + ggtheme
  # customization
  gplot <-.labs(p = gplot, font.main = font.main, font.x = font.x, font.y = font.y)
  gplot <- .set_ticks(gplot, font.tickslab = font.tickslab)

  gplot <- gplot + facet_wrap(~covariate, scales = "free", ncol = 1)
  gplot
}

