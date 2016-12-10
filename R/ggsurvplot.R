#' @include utilities.R theme_classic2.R surv_summary.R
#' @importFrom methods is
#' @importFrom stats pchisq
  NULL
#'Drawing Survival Curves Using ggplot2
#'@description Drawing survival curves using ggplot2
#'@param fit an object of class survfit.
#'@param fun an arbitrary function defining a transformation of the survival
#'  curve.  Often used transformations can be specified with a character
#'  argument: "event" plots cumulative events (f(y) = 1-y), "cumhaz" plots the
#'  cumulative hazard function (f(y) = -log(y)), and "pct" for survival
#'  probability in percentage.
#'@param surv.scale scale transformation of survival curves. Allowed values are
#'  "default" or "percent".
#'@param color color to be used for the survival curves. This argument is
#'  ignored when the number of strata (groups > 1). In this case, use the
#'  argument palette.
#'@param palette the color palette to be used. Allowed values include "hue" for
#'  the default hue color scale; "grey" for grey color palettes; brewer palettes
#'  e.g. "RdBu", "Blues", ...; or custom color palette e.g. c("blue", "red").
#'@param linetype line types. Allowed values includes i) "strata" for changing
#'  linetypes by strata (i.e. groups); ii) a numeric vector (e.g., c(1, 2)) or a
#'  character vector c("solid", "dashed").
#'@param break.time.by numeric value controlling time axis breaks. Default value
#'  is NULL.
#'@param conf.int logical value. If TRUE, plots confidence interval.
#'@param conf.int.fill fill color to be used for confidence interval.
#'@param conf.int.style confidence interval style. Allowed values include
#'  c("ribbon", "step").
#'@param censor logical value. If TRUE, censors will be drawn.
#'@param pval logical value. If TRUE, the p-value is added on the plot.
#'@param pval.size numeric value specifying the p-value text size. Default is 5.
#'@param pval.coord numeric vector, of length 2, specifying the x and y
#'  coordinates of the p-value. Default values are NULL.
#'@param main,xlab,ylab main title and axis labels
#'@param font.main,font.x,font.y,font.tickslab,font.legend a vector of length 3
#'  indicating respectively the size (e.g.: 14), the style (e.g.: "plain",
#'  "bold", "italic", "bold.italic") and the color (e.g.: "red") of main title,
#'  xlab and ylab and axis tick labels, respectively. For example \emph{font.x =
#'  c(14, "bold", "red")}.  Use font.x = 14, to change only font size; or use
#'  font.x = "bold", to change only font face.
#'@param xlim,ylim x and y axis limits e.g. xlim = c(0, 1000), ylim = c(0, 1).
#'@param legend character specifying legend position. Allowed values are one of
#'  c("top", "bottom", "left", "right", "none"). Default is "top" side position.
#'  to remove the legend use legend = "none". Legend position can be also
#'  specified using a numeric vector c(x, y); see details section.
#'@param legend.title legend title.
#'@param legend.labs character vector specifying legend labels. Used to replace
#'  the names of the strata from the fit. Should be given in the same order as
#'  those strata.
#'@param risk.table Allowed values include: \itemize{ \item TRUE or FALSE
#'  specifying whether to show or not the risk table. Default is FALSE. \item
#'  "absolute" or "percentage": to show the \bold{absolute number} and the
#'  \bold{percentage} of subjects at risk by time, respectively. Use "abs_pct"
#'  to show both absolute number and percentage.}
#'
#'@param risk.table.title Title to be used for risk table.
#'@param risk.table.col color to be used for risk table. Default value is
#'  "black". If you want to color by strata (i.e. groups), use risk.table.col =
#'  "strata".
#'@param risk.table.fontsize font size to be used for the risk table.
#'@param risk.table.y.text logical. Default is TRUE. If FALSE, risk table y axis
#'  tick labels will be hidden.
#'@param risk.table.y.text.col logical. Default value is FALSE. If TRUE, risk
#'  table tick labels will be colored by strata.
#'@param risk.table.height the height of the risk table on the grid. Increase
#'  the value when you have many strata. Default is 0.25. Ignored when
#'  risk.table = FALSE.
#'@param surv.plot.height the height of the survival plot on the grid. Default
#'  is 0.75. Ignored when risk.table = FALSE.
#'@param ncensor.plot logical value. If TRUE, the number of censored subjects at
#'  time t is plotted. Default is FALSE.
#'@param surv.median.line character vector for drawing a horizontal/vertical
#'  line at median survival. Allowed values include one of c("none", "hv", "h",
#'  "v"). v: vertical, h:horizontal.
#'@param ggtheme function, ggplot2 theme name. Default value is
#'  \link{theme_classic2}. Allowed values include ggplot2 official themes: see
#'  \code{\link[ggplot2]{theme}}.
#'@param ... other arguments to be passed to ggplot2 geom_*() functions such as
#'  linetype, size, ...
#'@details \strong{legend position}: The argument \strong{legend} can be also a
#'  numeric vector c(x,y). In this case it is possible to position the legend
#'  inside the plotting area. x and y are the coordinates of the legend box.
#'  Their values should be between 0 and 1. c(0,0) corresponds to the "bottom
#'  left" and c(1,1) corresponds to the "top right" position. For instance use
#'  legend = c(0.8, 0.2).
#'@return return an object of class ggsurvplot which is list containing two
#'  ggplot objects, including: \itemize{ \item plot: the survival plot \item
#'  table: the number at risk table per time }
#'
#'@author Alboukadel Kassambara, \email{alboukadel.kassambara@@gmail.com}
#' @examples
#'
#'#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#'# Example 1: Survival curves with two groups
#'#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#'
#'# Fit survival curves
#'#++++++++++++++++++++++++++++++++++++
#'require("survival")
#'fit<- survfit(Surv(time, status) ~ sex, data = lung)
#'
#'# Drawing survival curves
#'ggsurvplot(fit)
#'
#'# Change font size, style and color
#'#++++++++++++++++++++++++++++++++++++
#'\dontrun{
#' # Change font size, style and color at the same time
#' ggsurvplot(fit, main = "Survival curve",
#'    font.main = c(16, "bold", "darkblue"),
#'    font.x = c(14, "bold.italic", "red"),
#'    font.y = c(14, "bold.italic", "darkred"),
#'    font.tickslab = c(12, "plain", "darkgreen"))
#'}
#'
#'# Legend: title, labels and position
#'#++++++++++++++++++++++++++++++++++++
#'
#'# Change the legend title and labels
#'ggsurvplot(fit, legend = "bottom",
#'           legend.title = "Sex",
#'           legend.labs = c("Male", "Female"))
#'
#'# Specify legend position by its coordinates
#'ggsurvplot(fit, legend = c(0.2, 0.2))
#'
#'
#'# format
#'#++++++++++++++++++++++++++++++++++++
#'# change line size --> 1
#'# Change line types by groups (i.e. "strata")
#'# and change color palette
#'ggsurvplot(fit,  size = 1,  # change line size
#'           linetype = "strata", # change line type by groups
#'           break.time.by = 250, # break time axis by 250
#'           palette = c("#E7B800", "#2E9FDF"), # custom color palette
#'           conf.int = TRUE, # Add confidence interval
#'           pval = TRUE # Add p-value
#')
#'
#'# Use brewer color palette "Dark2"
#'# Add risk table
#'ggsurvplot(fit, linetype = "strata",
#'           conf.int = TRUE, pval = TRUE,
#'           palette = "Dark2", risk.table = TRUE)
#'
#'
#'# Change color, linetype by strata, risk.table color by strata
#'ggsurvplot(fit,
#'           pval = TRUE, conf.int = TRUE,
#'           risk.table = TRUE, # Add risk table
#'           risk.table.col = "strata", # Change risk table color by groups
#'           linetype = "strata", # Change line type by groups
#'           ggtheme = theme_bw(), # Change ggplot2 theme
#'           palette = c("#E7B800", "#2E9FDF"))
#'
#'
#'#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#'# Example 3: Survival curve with multiple group
#'#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#'
#'# Fit (complexe) survival curves
#'#++++++++++++++++++++++++++++++++++++
#' \dontrun{
#'require("survival")
#'fit2 <- survfit( Surv(time, status) ~ rx + adhere,
#'                 data = colon )
#'
#'# Visualize
#'#++++++++++++++++++++++++++++++++++++
#'
#'# Visualize: add p-value, chang y limits
#'# change color using brewer palette
#'# Adjust risk table and survival plot heights
#'ggsurvplot(fit2, pval = TRUE,
#'           break.time.by = 400,
#'           risk.table = TRUE,
#'           risk.table.col = "strata",
#'           risk.table.height = 0.5, # Useful when you have multiple groups
#'           palette = "Dark2")
#'}
#'
#'
#'#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#'# Example 4: Facet ggsurvplot() output by
#' # a combination of factors
#'#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#'
#'# Fit (complexe) survival curves
#'#++++++++++++++++++++++++++++++++++++
#' \dontrun{
#'require("survival")
#'fit3 <- survfit( Surv(time, status) ~ sex + rx + adhere,
#'                 data = colon )
#'
#'# Visualize
#'#++++++++++++++++++++++++++++++++++++
#' ggsurv <- ggsurvplot(fit3, fun = "cumhaz", conf.int = TRUE,
#'   risk.table = TRUE, risk.table.col="strata",
#'   ggtheme = theme_bw())
#'
#' # Faceting survival curves
#' curv_facet <- ggsurv$plot + facet_grid(rx ~ adhere)
#' curv_facet
#'
#' # Faceting risk tables:
#' # Generate risk table for each facet plot item
#' ggsurv$table + facet_grid(rx ~ adhere, scales = "free")+
#'  theme(legend.position = "none")
#'
#'  # Generate risk table for each facet columns
#' tbl_facet <- ggsurv$table + facet_grid(.~ adhere, scales = "free")
#' tbl_facet + theme(legend.position = "none")
#'
#' # Arrange faceted survival curves and risk tables
#' g2 <- ggplotGrob(curv_facet)
#' g3 <- ggplotGrob(tbl_facet)
#' min_ncol <- min(ncol(g2), ncol(g3))
#' g <- gridExtra::rbind.gtable(g2[, 1:min_ncol], g3[, 1:min_ncol], size="last")
#' g$widths <- grid::unit.pmax(g2$widths, g3$widths)
#' grid::grid.newpage()
#' grid::grid.draw(g)
#'
#'
#'
#'
#'}
#'
#'@describeIn ggsurvplot Draws survival curves using ggplot2.
#'@export
ggsurvplot <- function(fit, fun = NULL,
                       color = NULL, palette = "hue", linetype = 1, break.time.by = NULL,
                       surv.scale = c("default", "percent"),
                       conf.int = FALSE, conf.int.fill = "gray", conf.int.style = "ribbon",
                       censor = TRUE,
                       pval = FALSE, pval.size = 5, pval.coord = c(NULL, NULL),
                       main = NULL, xlab = "Time", ylab = "Survival probability",
                       font.main = c(16, "plain", "black"),
                       font.x = c(14, "plain", "black"), font.y = c(14, "plain", "black"),
                       font.tickslab = c(12, "plain", "black"),
                       xlim = NULL, ylim = NULL,
                       legend = c("top", "bottom", "left", "right", "none"),
                       legend.title = "Strata", legend.labs = NULL,
                       font.legend = c(10, "plain", "black"),
                       risk.table = FALSE, risk.table.title = NULL,
                       risk.table.col = "black", risk.table.fontsize = 4.5,
                       risk.table.y.text = TRUE,
                       risk.table.y.text.col = TRUE,
                       risk.table.height = 0.25, surv.plot.height = 0.75,
                       ncensor.plot = FALSE,
                       surv.median.line = c("none", "hv", "h", "v"),
                       ggtheme = theme_classic2(),
                       ...
                       ){

  if(!methods::is(fit, "survfit"))
    stop("Can't handle an object of class ", class(fit))
  size <- ifelse(is.null(list(...)$size), 1, list(...)$size)
  if(is.null(xlim)) xlim <- c(0, max(fit$time))
  if(is.null(ylim) & is.null(fun)) ylim <- c(0, 1)
  if(!is(legend, "numeric")) legend <- match.arg(legend)
  surv.median.line <- match.arg(surv.median.line)
  # Adapt ylab value according to the value of the argument fun
  ylab <- .check_ylab(ylab, fun)
  # Check and get linetypes
  lty <- .get_lty(linetype)
  linetype <- lty$lty
  linetype.manual <- lty$lty.manual
  # Check legend
  .check_legend_labs(fit, legend.labs)
  # risk.table argument
  risktable <- .parse_risk_table_arg(risk.table)
  risk.table <- risktable$display
  risk.table.type <- risktable$type
  if(is.null(risk.table.title)){
  if(risk.table.type == "percentage") risk.table.title = "Percentage at risk by time"
  else if(risk.table.type == "abs_pct") risk.table.title = "Number at risk by time: n (%)"
  else risk.table.title = "Number at risk by time"
  }

  # Data for survival plot
  d <- surv_summary(fit)

  # Number of strata and strata names
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  .strata <- d$strata
  # Multiple groups
  if (!is.null(.strata)){
    strata_names <- levels(.strata)
    n.strata <- length(strata_names)
    if(is.null(legend.labs)) legend.labs <- strata_names
    if(missing(color)) color <- "strata"
  }
  # One group
  else{
    n.strata <- 1
    if (is.null(legend.labs)) {
      .strata <- as.factor(rep("All", nrow(d)))
      legend.labs <- strata_names <- "All"
    }
    else {
    .strata <- as.factor(rep(legend.labs, nrow(d)))
     strata_names <- legend.labs
    }

    if(missing(conf.int)) conf.int = TRUE
    if(missing(color)) color <- "black"
  }
  d$strata <- .strata

  # Connect surv data to the origin for plotting
  # time = 0, surv = 1
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  d <- .connect2origin(d, fit)

  #  Transformation of the survival curve
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  d <- .apply_surv_func(d, fun = fun)

  # Scale transformation
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  surv.scale <- match.arg(surv.scale)
  scale_labels <-  ggplot2::waiver()
  if (surv.scale == "percent") scale_labels <- scales::percent


  # Drawing survival curves
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  d$strata <- factor(d$strata, levels = strata_names, labels = legend.labs)
  d <- d[order(d$strata), , drop = FALSE]
  surv.color <- ifelse(n.strata > 1, "strata", color)
  p <- ggplot2::ggplot(d, ggplot2::aes_string("time", "surv")) +
      .geom_exec(ggplot2::geom_step, data = d, size = size, color = surv.color, linetype = linetype, ...) +
       ggplot2::scale_y_continuous(labels = scale_labels, limits = ylim) +
       ggplot2::coord_cartesian(xlim = xlim)+
       #.ggcolor(palette, breaks = strata_names, labels = legend.labs) +
       #.ggfill(palette, breaks = strata_names, labels = legend.labs) +
        ggtheme

  # if palette != hue
  if(!("hue" %in% palette)){
    p <- p + .ggcolor(palette)+
      .ggfill(palette)
  }


  if(is.null(break.time.by)) times <- .get_x_major_breaks(p)
  else times <- seq(0, max(c(fit$time, xlim)), by = break.time.by)

  p <- p + ggplot2::scale_x_continuous(breaks = times)


  # Add confidence interval
  if(conf.int){
    if(missing(conf.int.fill)) conf.int.fill <- surv.color
    if(conf.int.style == "ribbon"){
      p <- p + .geom_exec(.geom_confint, data = d,
                          ymin = "lower", ymax = "upper",
                          fill = conf.int.fill,  alpha = 0.3, na.rm = TRUE)
    }
    else if(conf.int.style == "step"){
      p <- p + .geom_exec(ggplot2::geom_step, data = d,
                          y = "lower", linetype = "dashed",
                          color = surv.color, na.rm = TRUE)+
        .geom_exec(ggplot2::geom_step, data = d,
                   y = "upper", linetype = "dashed",
                   color = surv.color, na.rm = TRUE)

    }
  }
  # Add cencored
  if (censor & any(d$n.censor >= 1)) {
    p <- p + .geom_exec(ggplot2::geom_point, data = d[d$n.censor > 0, , drop = FALSE],
                          colour = surv.color, size = size*4.5, shape = "+")
  }

  # Add pvalue
  if(pval & !is.null(fit$strata)){
    pval <- .get_pvalue(fit)
    pvaltxt <- ifelse(pval < 1e-04, "p < 0.0001",
                    paste("p =", signif(pval, 2)))

    pval.x <- ifelse(is.null(pval.coord[1]), 0.1*max(fit$time), pval.coord[1])
    pval.y <- ifelse(is.null(pval.coord[2]), 0.2, pval.coord[2])
    p <- p + ggplot2::annotate("text", x = pval.x, y = pval.y,
                               label = pvaltxt, size = pval.size)
  }

  # Drawing a horizontal line at 50% survival
  #if(surv.scale == "percent") fun <- "pct"
  if(surv.median.line %in% c("hv", "h", "v"))
    p <- .add_surv_median(p, fit, type = surv.median.line, fun = fun)

  # Axis limits
  p <- p + ggplot2::expand_limits(x = 0, y = 0)
  # Axis label and legend title
  lty.leg.title <- ifelse(linetype == "strata", legend.title, linetype)
  p <- p + ggplot2::labs(x = xlab, y = ylab, title = main,
                         color = legend.title, fill = legend.title,
                         linetype = lty.leg.title
                         )
  p <-.labs(p = p, font.main = font.main, font.x = font.x, font.y = font.y)
  p <- .set_ticks(p, font.tickslab = font.tickslab)
  p <- p + ggplot2::theme(legend.position = legend)
  p <- .set_legend_font(p, font.legend)
  if(!is.null(linetype.manual)) p <- p + scale_linetype_manual(values = linetype.manual)



  # Add risk table
   if(risk.table){
     if(inherits(fit, "survfit.cox")) {
       legend.labs <- "All"
       risk.table.y.text.col <- FALSE
     }
     risktable <- .risk_table_plot(fit, times = times,
                                   xlim = xlim, legend.labs = legend.labs,
                                   risk.table.col = risk.table.col, palette = palette,
                                   ggtheme = ggtheme, risk.table.fontsize = risk.table.fontsize,
                                   risk.table.title = risk.table.title,
                                   risk.table.y.text = risk.table.y.text,
                                   font.tickslab = font.tickslab, type = risk.table.type
                                   )
     risktable <-.labs(risktable, font.main = font.main, font.x = font.x, font.y = font.y, xlab = xlab, ylab = legend.title)

     # risktable <- .set_ticks(risktable, font.tickslab = font.tickslab)
     risktable <- risktable + ggplot2::labs(color = legend.title, shape = legend.title)
     if("left" %in% legend) risktable <- risktable + ggplot2::theme(legend.position = legend)
     else risktable <- risktable + ggplot2::theme(legend.position = "none")
     # color risk.table ticks by strata
     if(risk.table.y.text.col){
       g <- ggplot2::ggplot_build(p)
       cols <- unlist(unique(g$data[[1]]["colour"]))
       if(length(cols)==1) cols <- rep(cols, length(legend.labs))
       names(cols) <- legend.labs # Give every color an appropriate name
       risktable <- risktable + ggplot2::theme(axis.text.y = ggplot2::element_text(colour = rev(cols)))
     }

    res <- list(plot = p, table = risktable)
   }
  else res <- list(plot = p)

  # Plot of censored subjects
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if(ncensor.plot){
    ncensor_data <- d
    if(inherits(fit, "survfit.cox")){
      ncensor_data <- data.frame(time = fit$time, n.censor = fit$n.censor,
                                 strata = rep("All", length(fit$n.censor)))
      surv.color <- "black"
      strata_names <- legend.labs <- "All"
    }

    ncensor_plot <- ggplot(d, aes_string("time", "n.censor")) +
      .geom_exec(geom_bar, d, color = surv.color, fill = surv.color,
                 stat = "identity", position = "dodge")+
      coord_cartesian(xlim = xlim)+
      scale_x_continuous(breaks = times)+
      ggtheme
    # if palette != hue
    if(!("hue" %in% palette)){
      ncensor_plot <- ncensor_plot +
        .ggcolor(palette, breaks = strata_names, labels = legend.labs)+
        .ggfill(palette, breaks = strata_names, labels = legend.labs)
    }

    ncensor_plot <-.labs(ncensor_plot, font.main = font.main,
                         font.x = font.x, font.y = font.y, xlab = xlab, ylab = "n.censor")
    ncensor_plot <- .set_ticks(ncensor_plot, font.tickslab = font.tickslab)
    ncensor_plot <- ncensor_plot + ggplot2::labs(color = legend.title, fill = legend.title)
    if("left" %in% legend) ncensor_plot <- ncensor_plot + ggplot2::theme(legend.position = legend)
    else ncensor_plot <- ncensor_plot + ggplot2::theme(legend.position = "none")
    res$ncensor.plot <- ncensor_plot
  }

  class(res) <- c("ggsurvplot", "list")
  attr(res, "surv.plot.height") <- surv.plot.height
  attr(res, "risk.table.height") <- risk.table.height
  attr(res, "risk.table.y.text.col") <- risk.table.y.text.col


  return(res)
}

#' @param x an object of class ggsurvplot
#' @method print ggsurvplot
#' @rdname ggsurvplot
#' @export
print.ggsurvplot <- function(x, surv.plot.height = NULL, risk.table.height = NULL, ...){
  if(!inherits(x, "ggsurvplot"))
    stop("An object of class ggsurvplot is required.")

  surv.plot.height <- ifelse(is.null(surv.plot.height), attr(x, "surv.plot.height"), surv.plot.height)
  risk.table.height <- ifelse(is.null(risk.table.height), attr(x, "risk.table.height"), risk.table.height)

  if(is.null(risk.table.height)) risk.table.height <- 0.25
  ncensor.plot.height <- ifelse(is.null(x$ncensor.plot), 0, 0.25)
  if(is.null(surv.plot.height)) surv.plot.height <- 1-risk.table.height-ncensor.plot.height

  if(!is.null(x$table)){
    # Hide legende: don't use  theme(legend.position = "none") because awkward legend when position = "left"
    x$table <- .hide_legend(x$table)
    # Make sure that risk.table.y.text.col will be the same as the plot legend colors
    risk.table.y.text.col <- attr(x, 'risk.table.y.text.col')
    if(risk.table.y.text.col){
      g <- ggplot2::ggplot_build(x$plot)
      cols <- unlist(unique(g$data[[1]]["colour"]))
      legend.labs <- levels(g$plot$data$strata)
      if(length(cols)==1) cols <- rep(cols, length(legend.labs))
      names(cols) <- legend.labs # Give every color an appropriate name
      x$table <- x$table + ggplot2::theme(axis.text.y = ggplot2::element_text(colour = rev(cols)))
    }
  }

  if(is.null(x$table) & is.null(x$ncensor.plot)) print(x$plot)
  else{
    if(is.null(x$ncensor.plot))
      heights = list(c(surv.plot.height, risk.table.height))
    else if(is.null(x$table)){
      heights = list(c(surv.plot.height, ncensor.plot.height))
    }
    else  heights = list(c(surv.plot.height, risk.table.height, ncensor.plot.height))

    nplot <- length(heights[[1]])

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
    do.call(gridExtra::grid.arrange, c(grobs, nrow = nplot, heights = heights ))
  }
}



.hide_legend <- function(p){
p <- p + theme(legend.key.height = NULL, legend.key.width = NULL,
                           legend.key = element_rect(colour = NA, fill = NA),
                           legend.text = element_text(colour = NA),
                           legend.title = element_text(colour = NA)) +
  guides(color = FALSE)
}


# Function defining a transformation of the survival curve
# ++++++++++++++++++++++++++++++++++++++++++++++
# see ?survival::plot.survfit
# d: data frame containing the column surv, upper and lower
# fun the function
.apply_surv_func <- function(d, fun = NULL){

  if (!is.null(fun)) {
    if (is.character(fun)) {
      fun <- switch(fun, log = function(y) log(y),
                    event = function(y) 1 - y,
                    cumhaz = function(y) -log(y),
                    cloglog = function(y) log(-log(y)),
                    pct = function(y) y * 100,
                    logpct = function(y) 100 * y,
                    identity = function(y) y,
                    stop("Unrecognized survival function argument"))
    }
    else if (!is.function(fun)) {
      stop("Invalid 'fun' argument")
    }
    d$surv <- fun(d$surv)
    d$upper <- fun(d$upper)
    d$lower <- fun(d$lower)
  }
  return(d)
}

# Adapt ylab according to the value of the argument fun
#%%%%%%%%%%%%%%%%%%%%%%%%%
.check_ylab <- function(ylab, fun){
  if(!is.null(fun)){
    if(ylab == "Survival probability"){
      ylab <- switch(fun, log = "log(Survival probability)",
                    event = "Cumulative event",
                    cumhaz = "Cumulative hazard",
                    pct = "Survival probability (%)",
                    identity = "Survival probability",
                    cloglog = "log(-log(S(t)))",
                    "Survival probability")
    }
  }
  ylab
}




# get survdiff pvalue
.get_pvalue <- function(fit){
  # One group
  if(length(levels(summary(fit)$strata)) == 0)  return(NULL)
    ssubset <- fit$call$subset
    if(is.null(ssubset))
      sdiff <- survival::survdiff(eval(fit$call$formula), data = eval(fit$call$data))
    else
      sdiff <- survival::survdiff(eval(fit$call$formula), data = eval(fit$call$data),
                                     subset = eval(fit$call$subset))
    pvalue <- stats::pchisq(sdiff$chisq, length(sdiff$n) - 1, lower.tail = FALSE)
    return (pvalue)
}

# Draw risk table
# on the left
.risk_table_plot <- function(fit, times, legend.labs = NULL,
                             xlim = c(0, max(fit$time)),
                             risk.table.col = "black",
                             palette = NULL, ggtheme = ggplot2::theme_classic(),
                             risk.table.fontsize = 5, risk.table.title = "Number at risk by time",
                             risk.table.y.text = TRUE,
                             font.tickslab = c(12, "plain", "black"), type = "absolute"
)
  {

  ntimes <- length(summary(fit, times = times, extend = TRUE)$time)

  if (is.null(fit$strata)) {
    .strata <- factor(rep("All", length(times)))
    strata_names <- "All"
    strata_size <- rep(fit$n, length(.strata))
  }
  else {
    .strata <- factor(summary(fit, times = times, extend = TRUE)$strata)
    strata_names <- names(fit$strata)
    nstrata = length(strata_names)
    strata_size <- rep(fit$n, each = length(.strata)/nstrata)
  }
  risk.data <- data.frame(
    strata = .clean_strata(as.factor(.strata)),
    time = summary(fit, times = times, extend = TRUE)$time,
    n.risk = round(summary(fit, times = times, extend = TRUE)$n.risk),
    strata_size = strata_size
  )
  risk.data$pct.risk <- round(risk.data$n.risk*100/risk.data$strata_size)
  risk.data$abs_pct.risk <- paste0(risk.data$n.risk, " (", risk.data$pct.risk, ")")

  if(!is.null(fit$strata)){
    variables <- .get_variables(risk.data$strata, fit)
    for(variable in variables) risk.data[[variable]] <- .get_variable_value(variable, risk.data$strata, fit)
  }


  if (!is.null(legend.labs))
    risk.data$strata <- factor(risk.data$strata, labels = legend.labs)

  time <- strata <- label <- n.risk <- pct.risk <- abs_pct.risk <- NULL

  # Adjust risk table labels in case of long strata
  risk.table.text.y <- rev(levels(risk.data$strata))
  n_strata <- length(risk.table.text.y)
#   max_char <- max(nchar(risk.table.text.y))
#   is_long_strata <- max_char > 5
#   if(is_long_strata) risk.table.text.y <- rep("-", n_strata)
   if(!risk.table.y.text) risk.table.text.y <- rep("-", n_strata)

  if(type == "percentage")
    dtp <- ggplot2::ggplot(risk.data,
                           aes(x = time, y = rev(strata), label = pct.risk, shape = rev(strata)))
  else if(type == "abs_pct")
    dtp <- ggplot2::ggplot(risk.data,
                           aes(x = time, y = rev(strata), label = abs_pct.risk, shape = rev(strata)))
  else
  dtp <- ggplot2::ggplot(risk.data,
                         ggplot2::aes(x = time, y = rev(strata), label = n.risk, shape = rev(strata)))

  dtp <- dtp + scale_shape_manual(values = 1:length(levels(risk.data$strata)))+
    # ggplot2::geom_point(size = 0)+
    .geom_exec(ggplot2::geom_text, data = risk.data, size = risk.table.fontsize, color = risk.table.col) +
    ggtheme +
    ggplot2::scale_y_discrete(breaks = as.character(levels(risk.data$strata)),
                              labels = risk.table.text.y ) +
    ggplot2::coord_cartesian(xlim = xlim) +
    ggplot2::scale_x_continuous(breaks = times) +
    # .ggcolor(palette)+
    labs(title = risk.table.title) +
    ggplot2::theme(legend.position = "none")

  # if palette != hue
  if(!("hue" %in% palette)){
    dtp <- dtp + .ggcolor(palette, breaks = strata_names, labels = legend.labs)
  }

  dtp <- .set_ticks(dtp, font.tickslab = font.tickslab)
  if(!risk.table.y.text)
    dtp <- dtp + theme(axis.text.y = element_text(size = 50, vjust = 0.35),
                       axis.ticks.y = element_blank())



  return(dtp)
}


# Check user defined legend labels
.check_legend_labs <- function(fit, legend.labs = NULL){

  if(!is.null(legend.labs) & !inherits(fit, "survfit.cox")){

    if(!is.null(fit$strata)){
      if(length(fit$strata) != length(legend.labs))
        stop("The length of legend.labs should be ", length(fit$strata) )
    }

    else{
      if(length(legend.labs) != 1)
        stop("The length of legend.labs should be 1")
    }

  }
}

# Connect survival data to the origine
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.connect2origin <- function(d, fit){
  base <- d[1, , drop = FALSE]
  base[intersect(c('time', 'n.censor', 'std.err', "n.event"), colnames(base))] <- 0
  base[c('surv', 'upper', 'lower')] <- 1.0
  n.strata <- length(levels(d$strata))

  # Connect each group to the origin
  if (n.strata > 1) {
    strata <- levels(d$strata)
    base <- base[rep(1, n.strata),, drop = FALSE]
    row.names(base) <- 1:nrow(base)
    base$strata <- strata
    base$strata <- factor(strata, levels = strata)
    # update variable values
    if(!inherits(fit, "survfit.cox")){
    variables <- .get_variables(base$strata,  fit)
    for(variable in variables) base[[variable]] <- .get_variable_value(variable, base$strata, fit)
    }
  }
  d <- rbind(base, d)
  d
}

# Ge x axis major breaks
#%%%%%%%%%%%%%%%%%%%%%%%%%%%
.get_x_major_breaks <- function(p){
  vv <- as.character(utils::packageVersion("ggplot2"))
  cc <- utils::compareVersion(vv, "2.1.0") > 0
  if(cc){
    # "v>2.1.0"
    breaks <- ggplot_build(p)$layout$panel_ranges[[1]]$x.major_source
  }
  # v<=2.1.0
  else breaks <- ggplot_build(p)$panel$ranges[[1]]$x.major_source
  breaks
}

# Adjust linetype manually
#%%%%%%%%%%%%%%%%%%%%%%%%%%%
.get_lty <- function(linetype){
  linetype.manual = NULL
  nlty <- length(linetype)
  if(is.numeric(linetype)){
    if(nlty > 1) {
      linetype.manual <-linetype
      linetype <- "strata"
    }
  }
  else (is.character(linetype))
  {
  base_lty <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
  is_base_lty <- all(linetype %in% base_lty)
  if(is_base_lty & nlty > 1){
    linetype.manual <-linetype
    linetype <- "strata"
  }
  }
  list(lty = linetype, lty.manual = linetype.manual)
}


# Parse risk.table argument
#%%%%%%%%%%%%%%%%%%%%%%%
# risk.table a logical value (TRUE/FALSE) or a string ("absolute", "percentage", "abs_pct")
.parse_risk_table_arg <- function(risk.table){
  res <- list(display = risk.table, type = "absolute")
  if(inherits(risk.table, "character") ){
    if(risk.table %in% c("absolute", "percentage", "abs_pct") )
      res <- list(display = TRUE, type = risk.table)
    else stop("Allowed values for risk.table are: TRUE, FALSE, 'absolute', 'percentage'")
  }
  res
}

# Drawing horizontal line at 50% median survival
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.add_surv_median <-function(p, fit, type = "hv", fun = NULL){
  x1 <- x2 <- y1 <- y2 <- NULL

  draw_lines <- TRUE
  med_y = 0.5

  if(is.null(fun)) draw_lines <- TRUE
  else if(fun %in% c("cumhaz", "cloglog")){
    warning("Adding survival median lines is not allowed when fun is: ", fun)
    draw_lines <- FALSE
  }
  else if(fun == "pct") med_y <- 50

  if(draw_lines){
      if(!is.null(fit$strata) | is.matrix(fit$surv)) .table <- as.data.frame(summary(fit)$table)
      else{
        .table <- t(as.data.frame(summary(fit)$table))
        rownames(.table) <- "All"
      }
      surv_median <- as.vector(.table[,"median"])
      df <- data.frame(x1 = surv_median, x2 = surv_median,
                       y1 = rep(0, length(surv_median)),
                       y2 = rep(med_y, length(surv_median)),
                       strata = .clean_strata(rownames(.table)))
      if(!is.null(fit$strata)){
        variables <- .get_variables(df$strata, fit)
        for(variable in variables) df[[variable]] <- .get_variable_value(variable, df$strata, fit)
      }
      df <- stats::na.omit(df)

      if(nrow(df)>0){
        if(type %in% c("hv", "h"))
          p <- p +
          geom_segment(aes(x = 0, y = max(y2), xend = max(x1), yend = max(y2)),
                       data = df, linetype = "dashed", size = 0.5) # horizontal segment

        if(type %in% c("hv", "v"))
          p <- p + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = df,
                                linetype = "dashed", size = 0.5) # vertical segments
      }
      else warning("Median survival not reached.")
  }

  p
}


