#' Distribution of Events' Times
#'
#' @param surv an object of \link{Surv}. If not suplied, the censoring variable is extracted from the model.
#' @param fit an object of class \link{survfit}.
#' @param data a dataset for predictions. If not supplied then data will be extracted from `fit` object.
#' @param type one of \code{c("cumulative", "radius", "fraction")}. \code{"cumulative"} stands for cumulative number of events, \code{"radius"} stands for number of events within a given radius,
#' @param normalized if \code{TRUE} relative number of events is presented,
#' @param ggtheme function, ggplot2 theme name.  Allowed values include ggplot2 official themes: see theme.
#' @param censored.on.top is TRUE then censored events are on the top
#' @param palette the color palette to be used for coloring of significant variables.
#' @param ... other graphical parameters to be passed to the function \link[ggpubr]{ggpar}.
#'
#' @return return an object of class ggplot
#' @importFrom stats as.formula terms
#' @author Przemyslaw Biecek, \email{przemyslaw.biecek@@gmail.com}
#'
#' @examples
#' require("survival")
#' # from Surv
#' surv <- Surv(lung$time, lung$status)
#' ggsurvevents(surv)
#'
#' surv2 <- Surv(colon$time, colon$status)
#' ggsurvevents(surv2)
#' ggsurvevents(surv2, normalized = TRUE)
#'
#' # from survfit
#' fit <- survfit(Surv(time, status) ~ sex, data = lung)
#' ggsurvevents(fit = fit, data = lung)
#'
#' # from coxph
#' model <- coxph( Surv(time, status) ~ sex + rx + adhere, data = colon )
#' ggsurvevents(fit = model, data = colon)
#' ggsurvevents(surv2, normalized = TRUE, type = "radius")
#' ggsurvevents(surv2, normalized = TRUE, type = "fraction")
#'
#' @export

ggsurvevents <- function(surv = NULL,
                         fit = NULL,
                         data = NULL,
                         type = "fraction",
                         normalized = TRUE,
                         censored.on.top = TRUE,
                         ggtheme = theme_survminer(),
                         palette = c("grey75", "grey25"),
                         ...) {
  if (!is.null(surv)) {
    stopifnot(class(surv) == "Surv")
  } else {
    # extract Surv from the model
    warning ("The `surv` argument is not provided. The censored variable will be extracted from model fit.")
    data <- .get_data(fit, data)

    form <- as.formula(fit$call)
    surv <- eval(attr(terms(form),"variables"), envir = data)[[1]]
  }

  # to remove NOTES
  cum0 <- cum1 <- time <- NULL
  class(surv) <- NULL
  surv <- as.data.frame(surv)

  stopifnot(type %in% c("cumulative", "radius", "fraction"))
  surv$cum0 <- 0
  surv$cum1 <- 0
  radius <- max(surv$time) / 12
  radiusN <- 1 / 12
  surv$timeN <- rank(surv$time)/length(surv$time)
  for (i in seq_along(surv$time)) {
    if (type == "cumulative") {
      plot.title = "Cumulative Distribution Of Events' Times"
      surv$cum0[i] <- sum(surv$time[surv$status == 0] <= surv$time[i], na.rm=TRUE)
      surv$cum1[i] <- sum(surv$time[surv$status == 1] <= surv$time[i], na.rm=TRUE)
    }
    if (type == "radius") {
      plot.title = "Distribution Of Events' Times"
      surv$cum0[i] <- sum(abs(surv$time[surv$status == 0] - surv$time[i]) < radius, na.rm=TRUE)
      surv$cum1[i] <- sum(abs(surv$time[surv$status == 1] - surv$time[i]) < radius, na.rm=TRUE)
    }
    if (type == "fraction") {
      plot.title = "Distribution Of Events' Times"
      surv$cum0[i] <- sum(abs(surv$timeN[surv$status == 0] - surv$timeN[i]) < radiusN, na.rm=TRUE)
      surv$cum1[i] <- sum(abs(surv$timeN[surv$status == 1] - surv$timeN[i]) < radiusN, na.rm=TRUE)
    }
  }

  .ylab <- "Number of events"
  if (normalized) {
    .ylab <- "Fraction of events"
    plot.title = paste("Ratio of", plot.title)
    tmp <- surv$cum0 + surv$cum1
    surv$cum0 <- surv$cum0/tmp
    surv$cum1 <- surv$cum1/tmp
  }

  if (censored.on.top) {
    p <- ggplot(surv) +
      geom_ribbon(aes(time, ymin=0, ymax=cum1), fill=palette[2]) +
      geom_ribbon(aes(time, ymin=cum1, ymax=cum0+cum1), fill=palette[1])
  } else {
    p <- ggplot(surv) +
      geom_ribbon(aes(time, ymin=0, ymax=cum0), fill=palette[1]) +
      geom_ribbon(aes(time, ymin=cum0, ymax=cum0+cum1), fill=palette[2])
  }
  p <- p + ggtheme +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), legend.position = "none") +
    labs(title = plot.title,
         subtitle = "black for status = 0 / grey for status = 1",
         x = "Time", y = .ylab)
  ggpubr::ggpar(p, ...)
}
