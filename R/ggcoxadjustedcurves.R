#' Adjusted Survival Curves for Cox Proportional Hazards Model
#' @importFrom tidyr gather
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom survival survfit
#' @description This function plots adjusted survival curves for coxph model.
#' The idea behind this function is described in \code{https://cran.r-project.org/web/packages/survival/vignettes/adjcurve.pdf}.
#' For every observation in the dataset a prediction for survival cure is made.
#' Then the predictions are averaged with respect to a selected variable.
#'@param fit an object of class \link{coxph.object} - created with \link{coxph} function.
#'@param data a dataset for predictions. If not supplied then data will be extracted from `fit` object.
#'@param variable a variable (vector) with values corresponding to groups to be plotted
#'@param individual.curves if TRUE then all individual predicted survival curves will be plotted
#'@param curve.size,curve.alpha size and alpha for individual survival curves
#'@param ylab y axis label.
#'@param font.main,font.x,font.y,font.tickslab a vector of length 3
#'  indicating respectively the size (e.g.: 14), the style (e.g.: "plain",
#'  "bold", "italic", "bold.italic") and the color (e.g.: "red") of main title,
#'  xlab and ylab and axis tick labels, respectively. For example \emph{font.x =
#'  c(14, "bold", "red")}.  Use font.x = 14, to change only font size; or use
#'  font.x = "bold", to change only font face.
#'@param ggtheme function, ggplot2 theme name. Default value is \link{theme_classic2}.
#'  Allowed values include ggplot2 official themes: see \code{\link[ggplot2]{theme}}.
#'@inheritParams ggpubr::ggpar
#'@return Returns an object of class \code{gg}.
#'
#'@author Przemyslaw Biecek, \email{przemyslaw.biecek@@gmail.com}
#'
#'@examples
#'
#' library(survival)
#' fit <- coxph( Surv(time, status) ~ rx + adhere, data = colon )
#'
#' ggcoxadjustedcurves(fit, data = colon)
#'
#' fit2 <- coxph( Surv(stop, event) ~ rx + size, data = bladder )
#' ggcoxadjustedcurves(fit2, data = bladder)
#' ggcoxadjustedcurves(fit2, individual.curves = TRUE, data = bladder, curve.alpha=0.01)
#' ggcoxadjustedcurves(fit2, data = bladder, variable=factor( bladder[,"rx"]))
#' ggcoxadjustedcurves(fit2, data = bladder, variable=factor(bladder[,"rx"]),
#'    individual.curves=TRUE, curve.alpha=0.01)
#'
#'@export
ggcoxadjustedcurves <- function(fit,
                                variable = NULL,
                                individual.curves = FALSE,
                                data = NULL,
                                palette = "hue",
                                curve.size = 2, curve.alpha = 0.2,
                                font.main = c(16, "plain", "black"),
                                font.x = c(14, "plain", "black"), font.y = c(14, "plain", "black"),
                                font.tickslab = c(12, "plain", "black"),
                                ylab = "Survival rate",
                                ggtheme = theme_classic2()){
  data <- .get_data(fit, data)

  pred <- survfit(fit, data)
  timepoints <- c(0, pred$time)

  adj_surv <- as.data.frame(cbind(time0 = 1, t(pred$surv)))
  colnames(adj_surv) <- paste0("time", timepoints)

  # this dirty hack hides the problem with CHECK NOTES generated for
  # non standard evaluation used by dplyr/tidyr
  .id <- time <- value <- NULL

  if (is.null(variable)) {
    # only one curve
    both <- cbind(.id = seq(data[,1]), adj_surv)
    curves <- gather(both, time, value, -.id)
    curves$time <- as.numeric(gsub(curves$time, pattern = "time", replacement = ""))
    curve <- summarise(group_by(curves, time), value = mean(value, na.rm=TRUE))
    pl <- ggplot(curve, aes(x = time, y = value)) +
      geom_step(size=curve.size, color = color)
  } else {
    # one per level
    both <- cbind(.id = seq(data[,1]), variable, adj_surv)
    curves <- gather(both, time, value, -.id, -variable)
    curves$time <- as.numeric(gsub(curves$time, pattern = "time", replacement = ""))
    curve <- summarise(group_by(curves, time, variable), value = mean(value, na.rm=TRUE))
    pl <- ggplot(curve, aes(x = time, y = value, color=variable)) +
      geom_step(size=curve.size)
  }
  if (individual.curves)
    pl <- pl + geom_step(data = curves, aes(group=.id), alpha=curve.alpha)
  pl <- ggpubr::ggpar(pl, font.main = font.main, font.x = font.x, font.y = font.y,
                      font.tickslab = font.tickslab, palette = palette,
                      ggtheme = ggtheme)
  pl +
    scale_y_continuous(limits = c(0, 1)) +
    ylab(ylab)
}
