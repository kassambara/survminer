#' Adjusted Survival Curves for Cox Proportional Hazards Model
#' @description This function plots adjusted survival curves for coxph model.
#' The idea behind this function is described in \code{https://cran.r-project.org/web/packages/survival/vignettes/adjcurve.pdf}.
#' For every observation in the dataset a prediction for survival cure is made.
#' Then the predictions are averaged with respect to a selected variable.
#'@param fit an object of class \link{coxph.object} - created with \link{coxph} function.
#'@param xlim,ylim x and y axis limits e.g. xlim = c(0, 1000), ylim = c(0, 1).
#'@param ylab y axis label.
#'@param point.col,point.size,point.shape,point.alpha color, size, shape and visibility to be used for points.
#'@param font.main,font.x,font.y,font.tickslab a vector of length 3
#'  indicating respectively the size (e.g.: 14), the style (e.g.: "plain",
#'  "bold", "italic", "bold.italic") and the color (e.g.: "red") of main title,
#'  xlab and ylab and axis tick labels, respectively. For example \emph{font.x =
#'  c(14, "bold", "red")}.  Use font.x = 14, to change only font size; or use
#'  font.x = "bold", to change only font face.
#'@param ggtheme function, ggplot2 theme name. Default value is \link{theme_classic2}.
#'  Allowed values include ggplot2 official themes: see \code{\link[ggplot2]{theme}}.
#'@return Returns an object of class \code{gg}.
#'
#'@author Przemyslaw Biecek, \email{przemyslaw.biecek@@gmail.com}
#'
#'@examples
#'
#' library(survival)
#' data(mgus)
#' res.cox <- coxph(Surv(futime, death) ~ mspike + log(mspike) + I(mspike^2) +
#'      age + I(log(age)^2) + I(sqrt(age)), data = mgus)
#'
#' ggcoxfunctional(res.cox, point.col = "blue", point.alpha = 0.5)
#'
#'@export
ggcoxadjustedcurves <- function(fit,
                             split_var = NULL,
                             plot_all = FALSE,
                             point.col = "red", point.size = 1, point.shape = 19, point.alpha = 1,
                             font.main = c(16, "plain", "black"),
                             font.x = c(14, "plain", "black"), font.y = c(14, "plain", "black"),
                             font.tickslab = c(12, "plain", "black"),
                             xlim = NULL, ylim = NULL,
                             ylab = "Martingale Residuals \nof Null Cox Model",
                             ggtheme = theme_classic2()){
  # this is a very risky way to get the data for a model
  # it works only in the call$data exists in the search path and may be evaluated
  # unfortunately it is quite common in this package and I do not see a better way ;-)
  data <- eval(fit$call$data)

  pred <- survfit(fit, data)
  timepoints <- c(0, pred$time)

  adj_surv <- as.data.frame(cbind(time0 = 1, t(pred$surv)))
  colnames(adj_surv) <- paste0("time", timepoints)

  both <- cbind(data, adj_surv)

  if (is.null(split_var)) {
    # only one curve
    both <- cbind(.id = seq(data[,1]), adj_surv)
    curves <- gather(both, time, value, -.id)
    curves$time <- as.numeric(gsub(curves$time, pattern = "time", replacement = ""))
    curve <- summarise(group_by(curves, time), value = mean(value))
    pl <- ggplot(curve, aes(x = time, y = value)) +
      geom_step(size=2)
  } else {
    # one per level
    both <- cbind(.id = seq(data[,1]), split_var, adj_surv)
    curves <- gather(both, time, value, -.id, -split_var)
    curves$time <- as.numeric(gsub(curves$time, pattern = "time", replacement = ""))
    curve <- summarise(group_by(curves, time, split_var), value = mean(value))
    pl <- ggplot(curve, aes(x = time, y = value, color=split_var)) +
      geom_step(size=2)
  }
  if (plot_all)
    pl <- pl + geom_step(data = curves, aes(group=.id), alpha=0.2)

  pl <-.labs(p = pl, font.main = font.main, font.x = font.x, font.y = font.y)
  pl <- .set_ticks(pl, font.tickslab = font.tickslab)
  pl +
    scale_y_continuous(limits = c(0, 1)) +
    theme_bw()
}
