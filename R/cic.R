#' Cumulative Incidence Curves for Competing Risks
#' @importFrom cmprsk cuminc
#' @description This function plots Cumulative Incidence Curves
#' @param fit an object of class \link{cuminc} - created with \link{cuminc} function.
#' @param gnames a vector with group names. If not suplied then will be extracted from \code{fit} object.
#' @param gsep a separator that extracts group names and event names from \code{gnames} object.
#' @param multiple_panels if \code{TRUE} then groups will be plotted in different panels.
#' @param ggtheme function, ggplot2 theme name. Default value is \link{theme_classic2}.
#'  Allowed values include ggplot2 official themes: see \code{\link[ggplot2]{theme}}.
#' @return Returns an object of class \code{gg}.
#'
#' @author Przemyslaw Biecek, \email{przemyslaw.biecek@@gmail.com}
#'
#' @examples
#'
#' set.seed(2)
#' ss <- rexp(100)
#' gg <- factor(sample(1:3,100,replace=TRUE),1:3,c('BRCA','LUNG','OV'))
#' cc <- factor(sample(0:2,100,replace=TRUE),0:2,c('no event', 'death', 'progression'))
#' strt <- sample(1:2,100,replace=TRUE)
#' print(fit <- cmprsk::cuminc(ss,cc,gg,strt))
#' ggcompetingrisks(fit)
#' ggcompetingrisks(fit, multiple_panels = FALSE)
#'
#' @export

ggcompetingrisks <- function(fit, gnames = NULL, gsep=" ",
                             multiple_panels = TRUE,
                             ggtheme = theme_survminer()) {
  if (!is.null(fit$Tests))
    fit <- fit[names(fit) != "Tests"]
 fit2 <- lapply(fit, `[`, 1:2)
 if (is.null(gnames)) gnames <- names(fit2)
 fit2_list <- lapply(seq_along(gnames), function(ind) {
   df <- as.data.frame(fit2[[ind]])
   df$name <- gnames[ind]
   df
 })
 df <- do.call(rbind, fit2_list)
 df$event <- sapply(strsplit(df$name, split=gsep), `[`, 2)
 df$group <- sapply(strsplit(df$name, split=gsep), `[`, 1)
 pl <- ggplot(df, aes(time, est, color=event))
 if (multiple_panels) {
   pl <- ggplot(df, aes(time, est, color=event)) + facet_wrap(~group)
 } else {
   pl <- ggplot(df, aes(time, est, color=event, linetype=group))
 }
 pl +
   geom_line() + ggtheme +
   ylab("Probability") + ggtitle("Cumulative incidence functions")
}
