#' @include utilities.R
#' @importFrom stats lowess
#' @importFrom stats approx
#' @importFrom stats resid
#' @importFrom survival coxph
#' @importFrom magrittr %>%
NULL
#' Functional Form of Continuous Variable in Cox Proportional Hazards Model
#'@description Displays graphs of continuous explanatory variable against martingale residuals of null
#'cox proportional hazards model, for each term in of the right side of \code{formula}. This might help to properly
#'choose the functional form of continuous variable in cox model (\link{coxph}). Fitted lines with \link{lowess} function
#'should be linear to satisfy cox proportional hazards model assumptions.
#'@param formula a formula object, with the response on the left of a ~ operator, and the terms on the right. The response must be a survival object as returned by the \link{Surv} function.
#'@param data a \code{data.frame} in which to interpret the variables named in the formula,
#'@param iter parameter of \link{lowess}.
#'@param f parameter of \link{lowess}.
#'@param xlim,ylim x and y axis limits e.g. xlim = c(0, 1000), ylim = c(0, 1).
#'@param ylab y axis label.
#'@param point.col,point.size,point.shape,point.alpha color, size, shape and visibility to be used for points.
#'@param font.main,font.x,font.y,font.tickslab a vector of length 3
#'  indicating respectively the size (e.g.: 14), the style (e.g.: "plain",
#'  "bold", "italic", "bold.italic") and the color (e.g.: "red") of main title,
#'  xlab and ylab and axis tick labels, respectively. For example \emph{font.x =
#'  c(14, "bold", "red")}.  Use font.x = 14, to change only font size; or use
#'  font.x = "bold", to change only font face.
#'@param ggtheme function, ggplot2 theme name. Default value is survminer::theme_classic2().
#'  Allowed values include ggplot2 official themes: theme_gray(), theme_bw(),
#'  theme_minimal(), theme_classic(), theme_void(), ....
#'@return Returns an object of class \code{ggcoxfunctional} which is a list of ggplots.
#'
#'@author Marcin Kosinski , \email{m.p.kosinski@@gmail.com}
#'
#'@examples
#'
#' library(survival)
#' data(mgus)
#' ggcoxfunctional(Surv(futime, death) ~ mspike + log(mspike) + I(mspike^2) +
#'                   age + I(log(age)^2) + I(sqrt(age)), data = mgus,
#'                 point.col = "blue", point.alpha = 0.5)
#'
#'
#'@describeIn ggcoxfunctional Functional Form of Continuous Variable in Cox Proportional Hazards Model.
#'@export
ggcoxfunctional <- function (formula, data, iter = 0, f = 0.6,
                             point.col = "red", point.size = 1, point.shape = 19, point.alpha = 1,
                             font.main = c(16, "plain", "black"),
                             font.x = c(14, "plain", "black"), font.y = c(14, "plain", "black"),
                             font.tickslab = c(12, "plain", "black"),
                             xlim = NULL, ylim = NULL,
                             ylab = "Martingale Residuals \nof Null Cox Model",
                             ggtheme = theme_classic2()){

  attr(stats::terms(formula), "term.labels") -> explanatory.variables.names
  stats::model.matrix(formula, data = data) -> explanatory.variables.values
  SurvFormula <- deparse(formula[[2]])
  martingale_resid <- lowess_x <- lowess_y <- NULL
  lapply(explanatory.variables.names, function(i){
    which_col <- which(colnames(explanatory.variables.values) == i)
    explanatory.variables.values[, which_col]-> explanatory

    cox.model <- coxph(stats::as.formula(paste0(SurvFormula, " ~ ", i)),
                       data = data)
    data2viz <- data.frame(explanatory = explanatory,
                           martingale_resid = resid(cox.model),
                           lowess_x = lowess(explanatory,
                                             resid(cox.model),
                                             iter = iter,
                                             f = f)$x,
                           lowess_y = lowess(explanatory,
                                             resid(cox.model),
                                             iter = iter,
                                             f = f)$y)

    if(is.null(xlim)) xlim <- c(min(data2viz$explanatory), max(data2viz$explanatory))
    if(is.null(ylim)) ylim <- c(min(data2viz$lowess_y), max(data2viz$lowess_y))

    ggplot(data2viz, aes(x = explanatory,
                         y = martingale_resid)) +
      geom_point(col = point.col, shape = point.shape, size = point.size, alpha = point.alpha) +
      geom_line(aes(lowess_x, lowess_y)) + ggtheme +
      xlab(i) +
      ylab(NULL) +
      ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) -> gplot

    gplot <-.labs(p = gplot, font.main = font.main, font.x = font.x, font.y = font.y)
    gplot <- .set_ticks(gplot, font.tickslab = font.tickslab)
  }) -> plots

  names(plots) <- explanatory.variables.names
  class(plots) <- c("ggcoxfunctional", "list")
  attr(plots, "y.text") <- ylab
  plots

}

#' @param x an object of class ggcoxfunctional
#' @param ... further arguments passed to print, but really it's unused
#' @method print ggcoxfunctional
#' @rdname ggcoxfunctional
#' @export
print.ggcoxfunctional <- function(x, ...){
  if(!inherits(x, "ggcoxfunctional"))
    stop("An object of class ggcoxfunctional is required.")
  plots <- x
  grobs <- widths <- list()
  for (i in 1:length(plots)) {
    grobs[[i]] <- ggplotGrob(plots[[i]])
    widths[[i]] <- grobs[[i]]$widths[2:5]
  }
  maxwidth <- do.call(grid::unit.pmax, widths)
  for (i in 1:length(grobs)) {
    grobs[[i]]$widths[2:5] <- as.list(maxwidth)
  }
  y.text <- attr(plots, "y.text")
  do.call(gridExtra::grid.arrange, c(grobs, left = y.text))
}

