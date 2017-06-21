#' @include utilities.R
NULL
#' Adjusted Survival Curves for Cox Proportional Hazards Model
#' @importFrom tidyr gather
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom survival survfit
#' @description This function plots adjusted survival curves for the \code{coxph} model.
#' The idea behind this function is described in \code{https://cran.r-project.org/web/packages/survival/vignettes/adjcurve.pdf}.
#' For every observation in the dataset a prediction for survival curve is made.
#' Then the predictions are averaged with respect to a selected variable.
#'@inheritParams ggsurvplot_arguments
#'@param fit an object of class \link{coxph.object} - created with \link{coxph} function.
#'@param data a dataset for predictions. If not supplied then data will be extracted from the \code{fit} object.
#'@param reference a dataset for with reference population, to which dependent variables should be balanced. If not specified, then the \code{data} will be used instead. Note that the \code{reference} dataset should contain variables used in \code{fit} object.
#'@param variable a character, name of the grouping variable to be plotted. If not suplied then it will be extracted from the model formula from the \code{strata()} component. If there is no \code{strata()} component then only a single curve will be plotted - average for the population.
#'@param ylab a label for oy axis.
#'@param ggtheme function, ggplot2 theme name. Allowed values include ggplot2 official themes: see \code{\link[ggplot2]{theme}}.
#'@inheritParams ggpubr::ggpar
#'@param ... further arguments passed to the function \code{\link[ggpubr]{ggpar}} for customizing the plot.
#'@return Returns an object of class \code{gg}.
#'
#'@author Przemyslaw Biecek, \email{przemyslaw.biecek@@gmail.com}
#'
#'@examples
#'
#' library(survival)
#'\dontrun{
#' fdata <- flchain[flchain$futime >=7,]
#' fdata$age2 <- cut(fdata$age, c(0,54, 59,64, 69,74,79, 89, 110),
#'                   labels = c(paste(c(50,55,60,65,70,75,80),
#'                                    c(54,59,64,69,74,79,89), sep='-'), "90+"))
#' fdata$group <- factor(1+ 1*(fdata$flc.grp >7) + 1*(fdata$flc.grp >9),
#'                       levels=1:3,
#'                       labels=c("FLC < 3.38", "3.38 - 4.71", "FLC > 4.71"))
#' # single curve
#' fit <- coxph( Surv(futime, death) ~ age*sex, data = fdata)
#' ggadjustedcurves(fit, data = fdata, method = "single")
#'
#' # average in groups
#' fit <- coxph( Surv(futime, death) ~ age*sex + strata(group), data = fdata)
#' ggadjustedcurves(fit, data = fdata, method = "average")
#'
#' # conditional balancing in groups
#' ggadjustedcurves(fit, data = fdata, method = "conditional", reference = fdata)
#'
#' # marginal balancing in groups
#' ggadjustedcurves(fit, data = fdata, method = "marginal")
#' }
#'
#' fit2 <- coxph( Surv(stop, event) ~  size, data = bladder )
#' # single curve
#' ggadjustedcurves(fit2, data = bladder)
#'
#' fit2 <- coxph( Surv(stop, event) ~  size + strata(rx), data = bladder )
#' # average in groups
#' ggadjustedcurves(fit2, data = bladder, method = "average", variable = "rx")
#'
#' # conditional balancing in groups
#' ggadjustedcurves(fit2, data = bladder, method = "conditional", variable = "rx")
#'
#' # marginal balancing in groups
#' ggadjustedcurves(fit2, data = bladder, method = "marginal", variable = "rx")
#'
#'@export
ggadjustedcurves <- function(fit,
                                variable = NULL,
                                data = NULL,
                                reference = NULL,
                                method = "conditional",
                                fun = NULL,
                                ylab = "Survival rate",
                                ggtheme = theme_survminer(), ...) {
  stopifnot(method %in% c("marginal", "average", "conditional", "single"))
  data <- .get_data(fit, data)
  ylim <- NULL
  if (is.null(fun)) ylim <- c(0, 1)

  # deal with default arguments
  # reference = NULL
  if (is.null(reference))
    reference <- data

  # variable = NULL
  if (is.null(variable)) {
    # is there a 'strata' component?
    term.labels <- attr(terms(fit2$formula), "term.labels")
    strata.term.labels <- grep(term.labels, pattern = "strata(", fixed = TRUE, value = TRUE)
    if (length(strata.term.labels) > 1) {
      variable <- gsub(
        gsub(
          strata.term.labels,
          pattern = "strata(", replacement = "", fixed = TRUE)[1],
        pattern = "[\\) ]", replacement = "")
    } else {
      method = "single"
    }
    # if not then leave variable = NULL
  }

  pl <- switch(method,
         single = ggadjustedcurves.single(reference, fit),
         average =  ggadjustedcurves.average(reference, fit, variable),
         conditional = ggadjustedcurves.conditional(data, fit, variable, reference),
         marginal = ggadjustedcurves.marginal(reference, fit, variable))

  pl <- pl + ggtheme +
    scale_y_continuous(limits = ylim) +
    ylab(ylab)
  ggpubr::ggpar(pl,  palette = palette, ...)

}


ggadjustedcurves.single <- function(data, fit) {
  lev <- unique(data[,variable])
  pred <- survexp(~1, data = data, ratetable = fit)

  curve <- data.frame(time = rep(c(0,pred$time), length(lev)),
                      variable = factor(rep(lev, each=1+length(pred$time))),
                      surv = c(rbind(1, pred$surv)))

  ggplot(curve, aes(x = time, y = surv, color=variable)) +
    geom_step()
}

ggadjustedcurves.average <- function(data, fit, variable) {
  lev <- unique(data[,variable])
  pred <- survexp(as.formula(paste("~", variable)), data = data,
                  ratetable = fit)

  curve <- data.frame(time = rep(c(0,pred$time), length(lev)),
                      variable = factor(rep(lev, each=1+length(pred$time))),
                      surv = c(rbind(1, pred$surv)))

  ggplot(curve, aes(x = time, y = surv, color=variable)) +
    geom_step()
}

ggadjustedcurves.conditional <- function(data, fit, variable, reference) {
  lev <- unique(data[,variable])
  reference[,variable] = "_reference_"
  df0 <- reference
  form <- paste(variable, "~", gsub(as.character(formula(fit))[3], pattern="\\+ *strata.*[^\\)].", replacement=""))

  allRes <- list()
  rwt <- numeric(nrow(data))
  for (level in lev) {
    indexes <- which(data[,variable] == level)
    if (length(indexes) > 0) {
      df1 <- data[indexes, ]
      ndf <- rbind(df0, df1)
      ndf[,variable] <- factor(ndf[,variable])
      model <- glm(as.formula(form), ndf, family="binomial")
      allRes[[level]] <- predict(model, newdata = data, type = "response")
      rwt[indexes] <- 1/allRes[[level]][indexes]
    }
  }

  nform <- paste(as.character(formula(fit))[2], "~", variable)
  nfit <- coxph(as.formula(nform), data=data, weight=rwt)

  pred <- survexp(as.formula(paste("~", variable)), data = data, ratetable = nfit)

  curve <- data.frame(time = rep(c(0,pred$time), length(lev)),
                      variable = factor(rep(lev, each=1+length(pred$time))),
                      surv = c(rbind(1, pred$surv)))

  ggplot(curve, aes(x = time, y = surv, color=variable)) +
    geom_step()
}

ggadjustedcurves.marginal <- function(data, fit, variable) {
  lev <- unique(data[,variable])
  ndata <- data[rep(1:nrow(data), each=length(lev)),
                setdiff(colnames(data), variable)]
  ndata[,variable] = rep(lev, nrow(data))

  pred <- survexp(as.formula(paste("~", variable)), data = ndata,
                  ratetable = fit)

  curve <- data.frame(time = rep(c(0,pred$time), length(lev)),
                      variable = factor(rep(lev, each=1+length(pred$time))),
                      surv = c(rbind(1, pred$surv)))

  ggplot(curve, aes(x = time, y = surv, color=variable)) +
    geom_step()
}
