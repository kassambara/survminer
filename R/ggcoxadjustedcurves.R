#' @include utilities.R
NULL
#'  Adjusted Survival Curves for Cox Proportional Hazards Model
#' @importFrom tidyr gather
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom survival survfit
#' @description This function plots adjusted survival curves for coxph model.
#' The idea behind this function is described in \code{https://cran.r-project.org/web/packages/survival/vignettes/adjcurve.pdf}.
#' For every observation in the dataset a prediction for survival curve is made.
#' Then the predictions are averaged with respect to a selected variable.
#'@inheritParams ggsurvplot_arguments
#'@param fit an object of class \link{coxph.object} - created with \link{coxph} function.
#'@param data a dataset for predictions. If not supplied then data will be extracted from `fit` object.
#'@param variable a variable (vector) with values corresponding to groups to be plotted
#'@param individual.curves if TRUE then all individual predicted survival curves will be plotted
#'@param curve.size,curve.alpha size and alpha for individual survival curves
#'@param ylab y axis label.
#'@param ggtheme function, ggplot2 theme name.
#'  Allowed values include ggplot2 official themes: see \code{\link[ggplot2]{theme}}.
#'@inheritParams ggpubr::ggpar
#'@param ... further arguments passed to the function \code{\link[ggpubr]{ggpar}} for customizing the plot.
#'@return Returns an object of class \code{gg}.
#'
#'@author Przemyslaw Biecek, \email{przemyslaw.biecek@@gmail.com}
#'
#'@examples
#'
#' library(survival)
#' fit2 <- coxph( Surv(stop, event) ~ rx + size, data = bladder )
#' ggcoxadjustedcurves(fit2, data = bladder)
#' ggcoxadjustedcurves(fit2, individual.curves = TRUE, data = bladder, curve.alpha=0.01)
#' ggcoxadjustedcurves(fit2, data = bladder, variable= bladder[,"rx"])
#' ggcoxadjustedcurves(fit2, data = bladder, variable= bladder[,"rx"],
#'    individual.curves=TRUE, curve.alpha=0.01)
#'
#'@export
ggcoxadjustedcurves <- function(fit,
                                variable = NULL,
                                individual.curves = FALSE,
                                data = NULL,
                                fun = NULL,
                                palette = "hue",
                                curve.size = 2, curve.alpha = 0.2,
                                ylab = "Survival rate",
                                ggtheme = theme_survminer(), ...){
  data <- .get_data(fit, data)
  ylim <- NULL
  if(is.null(fun)) ylim <- c(0, 1)

  pred <- survfit(fit, data)
  timepoints <- c(0, pred$time)

  adj_surv <- as.data.frame(cbind(time0 = 1, t(pred$surv)))
  colnames(adj_surv) <- paste0("time", timepoints)

  # this dirty hack hides the problem with CHECK NOTES generated for
  # non standard evaluation used by dplyr/tidyr
  .id <- time <- surv <- NULL

  if (is.null(variable)) {
    # only one curve
    both <- cbind(.id = seq(data[,1]), adj_surv)
    curves <- gather(both, time, surv, -.id)
    curves$time <- as.numeric(gsub(curves$time, pattern = "time", replacement = ""))
    curve <- summarise(group_by(curves, time), surv = mean(surv, na.rm=TRUE))
    curve <- .apply_surv_func(curve, fun = fun)
    pl <- ggplot(curve, aes(x = time, y = surv)) +
      geom_step(size=curve.size)
  } else {
    # one per level
    variable <- factor(variable)
    both <- cbind(.id = seq(data[,1]), variable, adj_surv)
    curves <- gather(both, time, surv, -.id, -variable)
    curves$time <- as.numeric(gsub(curves$time, pattern = "time", replacement = ""))
    curve <- summarise(group_by(curves, time, variable), surv = mean(surv, na.rm=TRUE))
    curve <- .apply_surv_func(curve, fun = fun)
    pl <- ggplot(curve, aes(x = time, y = surv, color=variable)) +
      geom_step(size=curve.size)
  }
  if (individual.curves){
    curves <- .apply_surv_func(curves, fun)
    pl <- pl + geom_step(data = curves, aes(group=.id), alpha=curve.alpha)
  }
  pl <- pl + ggtheme +
    scale_y_continuous(limits = ylim) +
    ylab(ylab)
  ggpubr::ggpar(pl,  palette = palette, ...)

}
