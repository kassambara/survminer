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
#'choose the functional form of continuous variable in cox model (\link[survival]{coxph}). Fitted lines with \link[stats]{lowess} function
#'should be linear to satisfy cox proportional hazards model assumptions.
#'@param fit an object of class \link[survival]{coxph.object} - created with \link[survival]{coxph} function.
#'@param formula a formula object, with the response on the left of a ~ operator, and the terms on the right. The response must be a survival object as returned by the \link[survival]{Surv} function.
#'@param data a \code{data.frame} in which to interpret the variables named in the formula,
#'@param iter parameter of \link[stats]{lowess}.
#'@param f parameter of \link[stats]{lowess}.
#'@param xlim,ylim x and y axis limits e.g. xlim = c(0, 1000), ylim = c(0, 1).
#'@param ylab y axis label.
#'@param title the title of the final \link[grid]{grob} (\code{top} in \link[gridExtra]{arrangeGrob})
#'@param caption the caption of the final \link[grid]{grob} (\code{bottom} in \link[gridExtra]{arrangeGrob})
#'@param point.col,point.size,point.shape,point.alpha color, size, shape and visibility to be used for points.
#'@param ggtheme function, ggplot2 theme name.
#'  Allowed values include ggplot2 official themes: see \code{\link[ggplot2]{theme}}.
#'@param ... further arguments passed to the function \code{\link[ggpubr]{ggpar}} for customizing the plot.
#'@return Returns an object of class \code{ggcoxfunctional} which is a list of ggplots.
#'
#'@author Marcin Kosinski , \email{m.p.kosinski@@gmail.com}
#'
#'@examples
#'
#' library(survival)
#' data(mgus)
#' res.cox <- coxph(Surv(futime, death) ~ mspike + log(mspike) + I(mspike^2) +
#'     age + I(log(age)^2) + I(sqrt(age)), data = mgus)
#' ggcoxfunctional(res.cox,  data = mgus, point.col = "blue", point.alpha = 0.5)
#' ggcoxfunctional(res.cox, data = mgus, point.col = "blue", point.alpha = 0.5,
#'                 title = "Pass the title", caption = "Pass the caption")
#'
#'
#'@describeIn ggcoxfunctional Functional Form of Continuous Variable in Cox Proportional Hazards Model.
#'@export
ggcoxfunctional <- function (formula, data = NULL, fit, iter = 0, f = 0.6,
                             point.col = "red", point.size = 1, point.shape = 19, point.alpha = 1,
                             xlim = NULL, ylim = NULL,
                             ylab = "Martingale Residuals \nof Null Cox Model",
                             title = NULL, caption = NULL,
                             ggtheme = theme_survminer(), ...){

  if(!missing(formula)){
    if(inherits(formula, "coxph")) fit <- formula
    else{
      warning("arguments formula is deprecated; ",
              "will be removed in the next version; ",
              "please use fit instead.", call. = FALSE)
      fit <- list(formula = formula, call = list(data = data))
    }
  }
  formula <- fit$formula
  data <- .get_data(fit, data)

  attr(stats::terms(formula), "term.labels") -> explanatory.variables.names
  stats::model.matrix(formula, data = data) -> explanatory.variables.values
  # Restrict data to the model-matrix rows (i.e. the complete cases for the
  # formula). model.matrix() drops rows with a missing value in any term, so
  # without this the null-model martingale residuals (computed from data below)
  # would be longer than the covariate values and error with "'x' and 'y'
  # lengths differ" (#248). For data without missing values this is a no-op.
  data <- data[rownames(explanatory.variables.values), , drop = FALSE]
  SurvFormula <- deparse(formula[[2]])

  # Keep only continuous terms. A functional-form (linearity) check plots
  # martingale residuals against the covariate, which is only meaningful for
  # continuous covariates. Factor/character terms (and terms such as strata())
  # are expanded/renamed in the model matrix (e.g. 'sex' -> 'sex2'), so they do
  # not match their term label and previously triggered a cryptic
  # "'x' and 'y' lengths differ" error (#357). Drop them with a warning.
  is_continuous <- explanatory.variables.names %in% colnames(explanatory.variables.values)
  if(any(!is_continuous)){
    warning("Skipping non-continuous term(s) in ggcoxfunctional(): ",
            paste(explanatory.variables.names[!is_continuous], collapse = ", "),
            ". Functional-form checks apply to continuous covariates only.",
            call. = FALSE)
    explanatory.variables.names <- explanatory.variables.names[is_continuous]
  }
  if(length(explanatory.variables.names) == 0)
    stop("No continuous covariate to plot in ggcoxfunctional().", call. = FALSE)
  martingale_resid <- lowess_x <- lowess_y <- NULL
  lapply(explanatory.variables.names, function(i){
    which_col <- which(colnames(explanatory.variables.values) == i)
    explanatory.variables.values[, which_col]-> explanatory

    cox.model <- coxph(stats::as.formula(paste0(SurvFormula, " ~ ", 1)),
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
    # ylim is left as supplied (NULL by default) so coord_cartesian() auto-scales
    # the y-axis to include ALL martingale-residual points. Previously ylim
    # defaulted to the range of the lowess smoother (lowess_y), which is much
    # narrower than the residuals and clipped most of the plotted points from
    # view (#465). A user-supplied ylim is still honoured unchanged.

    ggplot(data2viz, aes(x = explanatory,
                         y = martingale_resid)) +
      geom_point(col = point.col, shape = point.shape, size = point.size, alpha = point.alpha) +
      geom_line(aes(lowess_x, lowess_y)) + ggtheme +
      xlab(i) +
      ylab(NULL) +
      ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) -> gplot

    gplot <- ggpubr::ggpar(gplot, ...)
  }) -> plots

  names(plots) <- explanatory.variables.names
  class(plots) <- c("ggcoxfunctional", "ggsurv", "list")
  attr(plots, "y.text") <- ylab
  attr(plots, "caption") <- caption
  attr(plots, "title") <- title
  plots

}

#' @param x an object of class ggcoxfunctional
#' @param newpage open a new page. See \code{\link[gridExtra]{grid.arrange}}.
#' @method print ggcoxfunctional
#' @rdname ggcoxfunctional
#' @export
print.ggcoxfunctional <- function(x, ..., newpage = TRUE){
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
  caption <- attr(plots, "caption")
  title <- attr(plots, "title")
  do.call(gridExtra::grid.arrange, c(grobs, left = y.text, top = title,
                                     bottom = caption, newpage = newpage))
}

