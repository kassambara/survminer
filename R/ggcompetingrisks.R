#' Cumulative Incidence Curves for Competing Risks
#' @importFrom survival survfit
#' @description This function plots Cumulative Incidence Curves. For \code{cuminc} objects it's a \code{ggplot2} version of \code{plot.cuminc}.
#' For \code{survfitms} objects a different geometry is used, as suggested by \code{@@teigentler}.
#' @param fit an object of a class \code{cmprsk::cuminc} - created with \code{cmprsk::cuminc} function or \code{survfitms} created with \link{survfit} function.
#' @param gnames a vector with group names. If not supplied then will be extracted from \code{fit} object (\code{cuminc} only).
#' @param gsep a separator that extracts group names and event names from \code{gnames} object (\code{cuminc} only).
#' @param multiple_panels if \code{TRUE} then groups will be plotted in different panels (\code{cuminc} only).
#' @param ggtheme function, \code{ggplot2} theme name. Default value is \link{theme_survminer}.
#'  Allowed values include ggplot2 official themes: see \code{\link[ggplot2]{theme}}.
#' @param ... further arguments passed to the function \code{\link[ggpubr]{ggpar}} for customizing the plot.
#' @param conf.int if \code{TRUE} then additional layer (\code{geom_ribbon}) is added around the point estimate. The ribon is plotted with boundries +- \code{coef}*standard deviation.
#' @param coef see \code{conf.int}, scaling actor for the ribbon. The default value is 1.96.
#' @return Returns an object of class \code{gg}.
#'
#' @author Przemyslaw Biecek, \email{przemyslaw.biecek@@gmail.com}
#'
#' @examples
#' \dontrun{
#' if(require("cmprsk")){
#'
#' set.seed(2)
#' ss <- rexp(100)
#' gg <- factor(sample(1:3,100,replace=TRUE),1:3,c('BRCA','LUNG','OV'))
#' cc <- factor(sample(0:2,100,replace=TRUE),0:2,c('no event', 'death', 'progression'))
#' strt <- sample(1:2,100,replace=TRUE)
#'
#' # handles cuminc objects
#' print(fit <- cmprsk::cuminc(ss,cc,gg,strt))
#' ggcompetingrisks(fit)
#' ggcompetingrisks(fit, multiple_panels = FALSE)
#' ggcompetingrisks(fit, conf.int = TRUE)
#' ggcompetingrisks(fit, multiple_panels = FALSE, conf.int = TRUE)
#'
#' # handles survfitms objects
#' library(survival)
#' df <- data.frame(time = ss, group = gg, status = cc, strt)
#' fit2 <- survfit(Surv(time, status, type="mstate") ~ 1, data=df)
#' ggcompetingrisks(fit2)
#' fit3 <- survfit(Surv(time, status, type="mstate") ~ group, data=df)
#' ggcompetingrisks(fit3)
#' }
#'
#'   library(ggsci)
#'   library(cowplot)
#'   ggcompetingrisks(fit3) + theme_cowplot() + scale_fill_jco()
#' }
#' @export

ggcompetingrisks <- function(fit, gnames = NULL, gsep=" ",
                             multiple_panels = TRUE,
                             ggtheme = theme_survminer(),
                             coef = 1.96, conf.int = FALSE, ...) {
  stopifnot(any(class(fit) %in% c("cuminc", "survfitms")))

  if (any(class(fit) == "cuminc")) {
   pl <- ggcompetingrisks.cuminc(fit = fit, gnames=gnames,
                                  gsep=gsep, multiple_panels=multiple_panels,
                                 coef = coef, conf.int = conf.int)
  }
  if (any(class(fit) == "survfitms")) {
    pl <- ggcompetingrisks.survfitms(fit = fit)
  }

  pl <- pl + ggtheme +
    ylab("Probability of an event") + xlab("Time") +
    ggtitle("Cumulative incidence functions")
  ggpubr::ggpar(pl, ...)
}


ggcompetingrisks.cuminc <- function(fit, gnames = NULL, gsep=" ",
                                    multiple_panels = TRUE, coef = 1.96, conf.int = FALSE) {
  if (!is.null(fit$Tests))
    fit <- fit[names(fit) != "Tests"]
  fit2 <- lapply(fit, `[`, 1:3)
  if (is.null(gnames)) gnames <- names(fit2)
  fit2_list <- lapply(seq_along(gnames), function(ind) {
    df <- as.data.frame(fit2[[ind]])
    df$name <- gnames[ind]
    df
  })
  time <- est <- event <- group <- NULL
  df <- do.call(rbind, fit2_list)
  df$event <- sapply(strsplit(df$name, split=gsep), `[`, 2)
  df$group <- sapply(strsplit(df$name, split=gsep), `[`, 1)
  df$std <- std <- sqrt(df$var)
  pl <- ggplot(df, aes(time, est, color=event))
  if (multiple_panels) {
    pl <- ggplot(df, aes(time, est, color=event)) + facet_wrap(~group)
  } else {
    pl <- ggplot(df, aes(time, est, color=event, linetype=group))
  }
  if (conf.int) {
    pl <- pl + geom_ribbon(aes(ymin = est - coef*std, ymax=est + coef*std, fill = event), alpha = 0.2, linetype=0)
  }
  pl +
    geom_line()
}

ggcompetingrisks.survfitms <- function(fit) {
  times <- fit$time
  psta <- as.data.frame(fit$pstate)
  colnames(psta) <- fit$states
  if (is.null(fit$strata)) {
    psta$strata <- "all"
  } else {
    psta$strata <- rep(names(fit$strata), fit$strata)
  }
  psta$times <- times
  psta <- .rename_empty_colname(
    psta, newname = "."
    )

  event <- value <- strata <- NULL
  pstal <- gather(psta, event, value, -strata, -times)
  ggplot(pstal, aes(times, value, fill=event)) +
    geom_area() + facet_wrap(~strata)

}

.rename_empty_colname <- function(df, newname = "."){
  empty.col <- colnames(df) == ""
  empty.col.exist <- length(empty.col) > 0
  if(empty.col.exist)
    colnames(df)[empty.col] <- newname
  df
}
