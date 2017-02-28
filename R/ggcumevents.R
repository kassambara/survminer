#' @include utilities.R surv_summary.R
NULL
#'Cumulative Number of Events Table
#'@description Cumulative number of events table. Normally, users don't need to
#'  use this function directly. Internally used by the function
#'  \code{\link{ggsurvplot}}.
#'@inheritParams ggsurvplot
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
#'ggcumevents(fit, data = lung)
#'
#'# Change color by strata
#'# Remove y tick labels
#'ggcumevents(fit, data = lung, color = "strata",
#'   y.text = FALSE)
#'
#'
#'
#'@export
ggcumevents <- function (fit, data = NULL, color = "black", palette = NULL, break.time.by = NULL,  xlim = NULL,
                         title = "Cumulative number of events", xlab = "Time", ylab = "Strata",
                         legend = "top",
                         legend.title = "Strata", legend.labs = NULL, y.text = TRUE, y.text.col = TRUE, fontsize = 4.5,
                         ggtheme = theme_survminer(), ...)
  {

  if(!inherits(fit, "survfit"))
    stop("Can't handle an object of class ", class(fit))
  if(is.null(xlim)) xlim <- c(0, max(fit$time))
  .check_legend_labs(fit, legend.labs)

  data <- .get_data(fit, data = data)
  # Define time axis breaks
  if(is.null(break.time.by)) times <- .get_default_breaks(fit$time)
  else times <- seq(0, max(c(fit$time, xlim)), by = break.time.by)

  survsummary <- .get_timepoints_survsummary(fit, data, times)
  survsummary$cumevent <- unlist(by(survsummary$n.event, survsummary$strata, cumsum))
  if (!is.null(legend.labs))
    survsummary$strata <- factor(survsummary$strata, labels = legend.labs)
  if(is.null(legend.labs)) legend.labs <- levels(survsummary$strata)

  # Adjust risk table y axis tick labels in case of long strata
  yticklabs <- rev(levels(survsummary$strata))
  n_strata <- length(levels(survsummary$strata))
  if(!y.text) yticklabs <- rep("-", n_strata)

  time <- strata <- label <- n.event <- cumevent <- NULL
  p <- ggplot(survsummary,
              aes(x = time, y = rev(strata), label = cumevent, shape = rev(strata)))
  p <- p + scale_shape_manual(values = 1:length(levels(survsummary$strata)))+
    ggpubr::geom_exec(geom_text, data = survsummary, size = fontsize, color = color) +
    ggtheme +
    scale_y_discrete(breaks = as.character(levels(survsummary$strata)),labels = yticklabs ) +
    coord_cartesian(xlim = xlim) +
    scale_x_continuous(breaks = times)+
    labs(title = title, x = xlab, y = ylab, color = legend.title, shape = legend.title)

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


