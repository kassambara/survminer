#' @include utilities.R surv_summary.R
NULL
#'Number at Risk Table
#'@description Number at risk table. Normally, users don't need to
#'  use this function directly. Internally used by the function
#'  \code{\link{ggsurvplot}}.
#'@inheritParams ggsurvplot
#'@param type risk table type. Allowed values include:
#'  "absolute" or "percentage": to show the \bold{absolute number} and the
#'  \bold{percentage} of subjects at risk by time, respectively. Use "abs_pct"
#'  to show both absolute number and percentage.
#'@param title the title of the plot.
#'@param y.text logical. Default is TRUE. If FALSE, the table y axis. tick
#'  labels will be hidden.
#'@param y.text.col logical. Default value is FALSE. If TRUE, the table tick
#'  labels will be colored by strata.
#'@param fontsize text font size.
#'@param ... other arguments passed to the function \code{\link[ggpubr]{ggpar}}.
#'@return a ggplot.
#'@author Alboukadel Kassambara, \email{alboukadel.kassambara@@gmail.com}
#' @examples
#' # Fit survival curves
#'require("survival")
#'fit<- survfit(Surv(time, status) ~ sex, data = lung)
#'
#'# Basic plot
#'ggrisktable(fit, data = lung)
#'
#'# Change color by strata
#'# Remove y tick labels
#'ggrisktable(fit, data = lung, color = "strata",
#'   y.text = FALSE)
#'
#'
#'
#'@export
ggrisktable <- function (fit, data = NULL, type = c("absolute", "percentage", "abs_pct", "nrisk_cumcensor", "nrisk_cumevents"),
                         color = "black", palette = NULL, break.time.by = NULL,  xlim = NULL,
                         title = NULL, xlab = "Time", ylab = "Strata",
                         legend = "top",
                         legend.title = "Strata", legend.labs = NULL, y.text = TRUE, y.text.col = TRUE, fontsize = 4.5,
                         ggtheme = theme_light(), ...)
  {

  if(!inherits(fit, "survfit"))
    stop("Can't handle an object of class ", class(fit))
  if(is.null(xlim)) xlim <- c(0, max(fit$time))
  .check_legend_labs(fit, legend.labs)

  type <- match.arg(type)

  if(is.null(title)){
    title <- switch(type,
                    absolute = "Number at risk",
                    percentage = "Percentage at risk",
                    abs_pct = "Number at risk: n (%)",
                    nrisk_cumcensor = "Number at risk (number censored)",
                    nrisk_cumevents = "Number at risk (number of events)",
                    "Number at risk")
  }

  data <- .get_data(fit, data = data)

  # Define time axis breaks
  if(is.null(break.time.by)) times <- .get_default_breaks(fit$time)
  else times <- seq(0, max(c(fit$time, xlim)), by = break.time.by)

  survsummary <- .get_timepoints_survsummary(fit, data, times)

  if (!is.null(legend.labs))
    survsummary$strata <- factor(survsummary$strata, labels = legend.labs)
  if(is.null(legend.labs)) legend.labs <- levels(survsummary$strata)

  # Adjust table y axis tick labels in case of long strata
  yticklabs <- rev(levels(survsummary$strata))
  n_strata <- length(levels(survsummary$strata))
  if(!y.text) yticklabs <- rep("-", n_strata)

  # risk table labels depending on the type argument
  time <- strata <- label <- pct.risk <- abs_pct.risk <- n.risk <- NULL
  llabels <- switch(type,
                    percentage = round(survsummary$n.risk*100/survsummary$strata_size),
                    abs_pct = paste0(survsummary$n.risk, " (", survsummary$pct.risk, ")"),
                    nrisk_cumcensor = paste0(survsummary$n.risk, " (", survsummary$cum.n.censor, ")"),
                    nrisk_cumevents = paste0(survsummary$n.risk, " (", survsummary$cum.n.event, ")"),
                    survsummary$n.risk
                  )
  survsummary$llabels <- llabels

  p <- ggplot(data = survsummary,
              aes(x = time, y = rev(strata), label = llabels, shape = rev(strata)))

  p <- p + ggpubr::geom_exec(geom_text, data = survsummary, size = fontsize, color = color) +
    scale_shape_manual(values = 1:length(levels(survsummary$strata)))+
    ggtheme +
    scale_y_discrete(breaks = as.character(levels(survsummary$strata)), labels = yticklabs ) +
    coord_cartesian(xlim = xlim) +
    scale_x_continuous(breaks = times) +
    labs(title = title, x = xlab, y = ylab, color = legend.title, shape = legend.title)

  p <- .set_risktable_gpar(p, ...)
  p <- ggpubr::ggpar(p, legend = legend, palette = palette,...)


  if(!y.text) p <- .set_large_dash_as_ytext(p)

  # color table tick labels by strata
  if(y.text.col){
    g <- ggplot2::ggplot_build(p)
    cols <- unlist(unique(g$data[[1]]["colour"]))
    if(length(cols) == 1) cols <- rep(cols, length(legend.labs))
    names(cols) <- legend.labs # Give every color an appropriate name
    p <- p + theme(axis.text.y = element_text(colour = rev(cols)))
  }

  p

}

# For backward compatibility
# Specific graphical params to risk.table
.set_risktable_gpar <- function(p,  ...){
  extra.params <- list(...)
  ggpubr:::.labs(p,
        font.main = extra.params$font.risk.table.title,
        font.x = extra.params$font.risk.table.x,
        font.y = extra.params$font.risk.table.y,
        submain = extra.params$risk.table.subtitle,
        caption = extra.params$risk.table.caption,
        font.submain = extra.params$font.risk.table.subtitle,
        font.caption = extra.params$font.risk.table.caption)
}



