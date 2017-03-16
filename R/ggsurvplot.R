#' @include utilities.R theme_classic2.R surv_summary.R
#' @importFrom methods is
#' @importFrom stats pchisq
#' @importFrom survMisc ten comp
#' @importFrom utils capture.output
  NULL
#'Drawing Survival Curves Using ggplot2
#'@description Drawing survival curves using ggplot2
#'@param fit an object of class survfit.
#'@param data a dataset used to fit survival curves. If not supplied then data
#'  will be extracted from 'fit' object.
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
#'  See details section for more information.
#'@param linetype line types. Allowed values includes i) "strata" for changing
#'  linetypes by strata (i.e. groups); ii) a numeric vector (e.g., c(1, 2)) or a
#'  character vector c("solid", "dashed").
#'@param break.time.by numeric value controlling time axis breaks. Default value is NULL.
#'@param break.x.by alias of break.time.by. Numeric value controlling x axis breaks. Default value is NULL.
#'@param break.y.by same as break.x.by but for y axis.
#'@param conf.int logical value. If TRUE, plots confidence interval.
#'@param conf.int.fill fill color to be used for confidence interval.
#'@param conf.int.style confidence interval style. Allowed values include
#'  c("ribbon", "step").
#'@param censor logical value. If TRUE, censors will be drawn.
#'@param pval logical value. If TRUE, the p-value is added on the plot.
#'@param pval.size numeric value specifying the p-value text size. Default is 5.
#'@param pval.coord numeric vector, of length 2, specifying the x and y
#'  coordinates of the p-value. Default values are NULL.
#'@param title,xlab,ylab main title and axis labels
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
#'  \bold{percentage} of subjects at risk by time, respectively. Use i)
#'  "abs_pct" to show both absolute number and percentage. ii) "nrisk_cumcensor"
#'  and "nrisk_cumevents" to show the number at risk and, the cumulative number
#'  of censoring and events, respectively. }
#'
#'@param risk.table.title The title to be used for the risk table.
#'@param risk.table.pos character vector specifying the risk table position.
#'  Allowed options are one of c("out", "in") indicating 'outside' or 'inside'
#'  the main plot, respectively. Default value is "out".
#'@param risk.table.col same as tables.col but for risk table only.
#'@param risk.table.fontsize,fontsize font size to be used for the risk table
#'  and the cumulative events table.
#'@param risk.table.y.text logical. Default is TRUE. If FALSE, risk table y axis
#'  tick labels will be hidden.
#'@param risk.table.y.text.col logical. Default value is FALSE. If TRUE, risk
#'  table tick labels will be colored by strata.
#'@param tables.height numeric value (in [0 - 1]) specifying the general height
#'  of all tables under the main survival plot.
#'@param tables.y.text logical. Default is TRUE. If FALSE, the y axis tick
#'  labels of tables will be hidden.
#'@param tables.col color to be used for all tables under the main plot. Default value is
#'  "black". If you want to color by strata (i.e. groups), use tables.col =
#'  "strata".
#'@param tables.theme function, ggplot2 theme name. Default value is
#'  \link{theme_survminer}. Allowed values include ggplot2 official themes: see
#'  \code{\link[ggplot2]{theme}}.
#'@param risk.table.height the height of the risk table on the grid. Increase
#'  the value when you have many strata. Default is 0.25. Ignored when
#'  risk.table = FALSE.
#'@param surv.plot.height the height of the survival plot on the grid. Default
#'  is 0.75. Ignored when risk.table = FALSE. \code{1-risk.table.height -
#'  ncensor.plot.height} when \code{risk.table = TRUE} and \code{ncensor.plot =
#'  TRUE}
#'@param ncensor.plot logical value. If TRUE, the number of censored subjects at
#'  time t is plotted. Default is FALSE. Ignored when cumcensor = TRUE.
#'@param ncensor.plot.title The title to be used for the censor plot. Used when
#'  \code{ncensor.plot = TRUE}.
#'@param ncensor.plot.height The height of the censor plot. Used when
#'  \code{ncensor.plot = TRUE}.
#'@param cumevents logical value specifying whether to show or not the table of
#'  the cumulative number of events. Default is FALSE.
#'@param cumevents.title The title to be used for the cumulative events table.
#'@param cumevents.col same as tables.col but for the cumulative events table only.
#'@param cumevents.y.text logical. Default is TRUE. If FALSE, the y axis tick
#'  labels of the cumulative events table  will be hidden.
#'@param cumevents.y.text.col logical. Default value is FALSE. If TRUE, the y
#'  tick labels of the cumulative events will be colored by strata.
#'@param cumevents.height the height of the cumulative events table on the grid.
#'  Default is 0.25. Ignored when cumevents = FALSE.
#'@param cumcensor logical value specifying whether to show or not the table of
#'  the cumulative number of censoring. Default is FALSE.
#'@param cumcensor.title The title to be used for the cumcensor table.
#'@param cumcensor.col same as tables.col but for cumcensor table only.
#'@param cumcensor.y.text logical. Default is TRUE. If FALSE, the y axis tick
#'  labels of the cumcensor table will be hidden.
#'@param cumcensor.y.text.col logical. Default value is FALSE. If TRUE, the y
#'  tick labels of the cumcensor will be colored by strata.
#'@param cumcensor.height the height of the cumcensor table on the grid. Default
#'  is 0.25. Ignored when cumcensor = FALSE.
#'@param surv.median.line character vector for drawing a horizontal/vertical
#'  line at median survival. Allowed values include one of c("none", "hv", "h",
#'  "v"). v: vertical, h:horizontal.
#'@param ggtheme function, ggplot2 theme name. Default value is
#'  \link{theme_survminer}. Allowed values include ggplot2 official themes: see
#'  \code{\link[ggplot2]{theme}}.
#'@param ... other arguments to be passed i) to ggplot2 geom_*() functions such
#'  as linetype, size, ii) or to the function ggpubr::ggpar() for customizing
#'  the plots. See details section.
#'@param log.rank.weights The name for the type of weights to be used in
#'  computing the p-value for log-rank test. By default \code{survdiff} is used
#'  to calculate regular log-rank test (with weights == 1). A user can specify
#'  \code{"1", "n", "sqrtN", "S1", "S2", "FH"} to use weights specified in
#'  \link[survMisc]{comp}, so that weight correspond to the test as : 1 -
#'  log-rank, n - Gehan-Breslow (generalized Wilcoxon), sqrtN - Tarone-Ware, S1
#'  - Peto-Peto's modified survival estimate, S2 - modified Peto-Peto (by
#'  Andersen), FH - Fleming-Harrington(p=1, q=1).
#'@param pval.method whether to add a text with the test name used for
#'  calculating the pvalue, that corresponds to survival curves' comparison -
#'  used only when \code{pval=TRUE}
#'@param pval.method.size the same as \code{pval.size} but for displaying
#'  \code{log.rank.weights} name
#'@param pval.method.coord the same as \code{pval.coord} but for displaying
#'  \code{log.rank.weights} name
#'@details \itemize{ \item \strong{legend position}: The argument \strong{legend} can be also a
#'  numeric vector c(x,y). In this case it is possible to position the legend
#'  inside the plotting area. x and y are the coordinates of the legend box.
#'  Their values should be between 0 and 1. c(0,0) corresponds to the "bottom
#'  left" and c(1,1) corresponds to the "top right" position. For instance use
#'  legend = c(0.8, 0.2).\cr \item \strong{Color palettes}: The argument
#'  \strong{palette} can be used to specify the color to be used for each group.
#'  By default, the first color in the palette is used to color the first level
#'  of the factor variable. This default behavior can be changed by assigning
#'  correctly a named vector. That is, the names of colors should match the
#'  strata names as generated by the \code{ggsurvplot()} function in the
#'  legend.\cr \item \strong{Customizing the plots}: The plot can be easily
#'  customized using additional arguments to be passed to the function ggpar().
#'  Read ?ggpubr::ggpar. These arguments include
#'  \emph{font.title, font.subtitle, font.caption, font.x, font.y, font.tickslab and font.legend}:
#'   a vector of length 3 indicating respectively the size (e.g.: 14), the style
#'  (e.g.: "plain", "bold", "italic", "bold.italic") and the color (e.g.: "red")
#'  of main title, subtitle, caption, xlab and ylab, axis tick labels and legend,
#'  respectively. For example \emph{font.x = c(14, "bold", "red")}.  Use font.x
#'  = 14, to change only font size; or use font.x = "bold", to change only font
#'  face.}
#'
#'@return return an object of class ggsurvplot which is list containing the
#'  following components: \itemize{ \item plot: the survival plot (ggplot
#'  object) \item table: the number of subjects at risk table per time (ggplot
#'  object). \item cumevents: the cumulative number of events table (ggplot
#'  object). \item ncensor.plot: the number of censoring (ggplot object). \item
#'  data.survplot: the data used to plot the survival curves (data.frame). \item
#'  data.survtable: the data used to plot the tables under the main survival
#'  curves (data.frame). }
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
#'# Basic survival curves
#'ggsurvplot(fit, data = lung)
#'
#'# Customized survival curves
#'ggsurvplot(fit, data = lung,
#'  surv.median.line = "hv", # Add medians survival
#'
#'  # Change legends: title & labels
#'  legend.title = "Sex",
#'  legend.labs = c("Male", "Female"),
#'  # Add p-value and confidence intervals
#'  pval = TRUE,
#'
#'  conf.int = TRUE,
#'  # Add risk table
#'  risk.table = TRUE,
#'  tables.height = 0.2,
#'  tables.theme = theme_cleantable(),
#'
#'  # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
#'  # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
#'  palette = c("#E7B800", "#2E9FDF"),
#'  ggtheme = theme_bw() # Change ggplot2 theme
#')
#'
#'# Change font size, style and color
#'#++++++++++++++++++++++++++++++++++++
#'\dontrun{
#' # Change font size, style and color at the same time
#' ggsurvplot(fit, data = lung,  main = "Survival curve",
#'    font.main = c(16, "bold", "darkblue"),
#'    font.x = c(14, "bold.italic", "red"),
#'    font.y = c(14, "bold.italic", "darkred"),
#'    font.tickslab = c(12, "plain", "darkgreen"))
#'}
#'
#'
#'
#'#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#'# Example 2: Facet ggsurvplot() output by
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
#' ggsurv <- ggsurvplot(fit3, data = colon,
#'   fun = "cumhaz", conf.int = TRUE,
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
#'}
#'
#'@describeIn ggsurvplot Draws survival curves using ggplot2.
#'@export
ggsurvplot <- function(fit, data = NULL, fun = NULL,
                       color = NULL, palette = NULL, linetype = 1,
                       break.x.by = NULL, break.y.by = NULL,  break.time.by = NULL,
                       surv.scale = c("default", "percent"),
                       conf.int = FALSE, conf.int.fill = "gray", conf.int.style = "ribbon",
                       censor = TRUE,
                       pval = FALSE, pval.size = 5, pval.coord = c(NULL, NULL),
                       pval.method = FALSE, pval.method.size = pval.size, pval.method.coord = c(NULL, NULL),
                       log.rank.weights = c("survdiff", "1", "n", "sqrtN", "S1", "S2", "FH_p=1_q=1"),
                       title = NULL,  xlab = "Time", ylab = "Survival probability",
                        xlim = NULL, ylim = NULL,
                       legend = c("top", "bottom", "left", "right", "none"),
                       legend.title = "Strata", legend.labs = NULL,
                       tables.height = 0.25, tables.y.text = TRUE, tables.col = "black",
                       risk.table = FALSE, risk.table.pos = c("out", "in"), risk.table.title = NULL,
                       risk.table.col = tables.col, risk.table.fontsize = 4.5, fontsize = 4.5,
                       risk.table.y.text = tables.y.text,
                       risk.table.y.text.col = TRUE,
                       risk.table.height = tables.height, surv.plot.height = 0.75,
                       ncensor.plot.height = tables.height, cumevents.height = tables.height,
                       cumcensor.height = tables.height,
                       ncensor.plot = FALSE,
                       ncensor.plot.title = NULL,
                       cumevents = FALSE, cumevents.col = tables.col, cumevents.title = NULL,
                       cumevents.y.text = tables.y.text, cumevents.y.text.col = TRUE,
                       cumcensor = FALSE, cumcensor.col = tables.col, cumcensor.title = NULL,
                       cumcensor.y.text = tables.y.text, cumcensor.y.text.col = TRUE,
                       surv.median.line = c("none", "hv", "h", "v"),
                       ggtheme = theme_survminer(),
                       tables.theme = ggtheme,
                       ...
                       ){

  if(!inherits(fit, "survfit"))
    stop("Can't handle an object of class ", class(fit))
  size <- ifelse(is.null(list(...)$size), 1, list(...)$size)
  if(is.null(xlim)) xlim <- c(0, max(fit$time))
  if(is.null(ylim) & is.null(fun)) ylim <- c(0, 1)
  if(!is(legend, "numeric")) legend <- match.arg(legend)
  surv.median.line <- match.arg(surv.median.line)
  stopifnot(log.rank.weights %in% c("survdiff", "1", "n", "sqrtN", "S1", "S2","FH_p=1_q=1"))
  log.rank.weights <- match.arg(log.rank.weights)

  # Make sure that user can do either ncensor.plot or cumcensor
  # But not both
  if(ncensor.plot & cumcensor){
    warning("Both ncensor.plot and cumsensor are TRUE.",
            "In this case, we consider only cumcensor.", call. = FALSE)
    ncensor.plot <- FALSE
  }
  if(cumcensor) ncensor.plot.height <- cumcensor.height
  if(is.null(ncensor.plot.title))
    ncensor.plot.title <- "Number of censoring"
  if(is.null(cumcensor.title))
    cumcensor.title <- "Cumulative number of censoring"
  if(is.null(cumevents.title))
    cumevents.title <- "Cumulative number of events"

  # Adapt ylab value according to the value of the argument fun
  ylab <- .check_ylab(ylab, fun)
  # Check and get linetypes
  lty <- .get_lty(linetype)
  linetype <- lty$lty
  linetype.manual <- lty$lty.manual
  # Check legend
  .check_legend_labs(fit, legend.labs)
  # risk.table argument
  risk.table.pos <- match.arg(risk.table.pos)
  risktable <- .parse_risk_table_arg(risk.table)
  risk.table <- risktable$display
  risk.table.type <- risktable$type
  extra.params <- list(...)

  # Data
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # data used to compute survfit
  data <- .get_data(fit, data = data)
  # Data for survival plot
  d <- surv_summary(fit, data = data)

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
  d <- .connect2origin(d, fit, data)

  #  Transformation of the survival curve
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  d <- .apply_surv_func(d, fun = fun)

  # Scale transformation
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  surv.scale <- match.arg(surv.scale)
  scale_labels <-  ggplot2::waiver()
  if (surv.scale == "percent") scale_labels <- scales::percent

  y.breaks <- ggplot2::waiver()
  if(!is.null(break.y.by)) y.breaks <- seq(0, 1, by = break.y.by)


  # Drawing survival curves
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  d$strata <- factor(d$strata, levels = strata_names, labels = legend.labs)
  d <- d[order(d$strata), , drop = FALSE]
  surv.color <- ifelse(n.strata > 1, "strata", color)
  p <- ggplot2::ggplot(d, ggplot2::aes_string("time", "surv")) +
       ggpubr::geom_exec(ggplot2::geom_step, data = d, size = size, color = surv.color, linetype = linetype, ...) +
       ggplot2::scale_y_continuous(breaks = y.breaks, labels = scale_labels, limits = ylim) +
       ggplot2::coord_cartesian(xlim = xlim)+
       ggtheme
  p <- ggpubr::ggpar(p, palette = palette, ...)

  if(!is.null(break.x.by)) break.time.by <- break.x.by
  if(is.null(break.time.by)) times <- .get_default_breaks(fit$time)
  else times <- seq(0, max(c(fit$time, xlim)), by = break.time.by)
  p <- p + ggplot2::scale_x_continuous(breaks = times)


  # Add confidence interval
  if(conf.int){
    if(missing(conf.int.fill)) conf.int.fill <- surv.color
    if(conf.int.style == "ribbon"){
      p <- p + ggpubr::geom_exec(.geom_confint, data = d,
                          ymin = "lower", ymax = "upper",
                          fill = conf.int.fill,  alpha = 0.3, na.rm = TRUE)
    }
    else if(conf.int.style == "step"){
      p <- p + ggpubr::geom_exec(ggplot2::geom_step, data = d,
                          y = "lower", linetype = "dashed",
                          color = surv.color, na.rm = TRUE)+
        ggpubr::geom_exec(ggplot2::geom_step, data = d,
                   y = "upper", linetype = "dashed",
                   color = surv.color, na.rm = TRUE)

    }
  }
  # Add cencored
  if (censor & any(d$n.censor >= 1)) {
    p <- p + ggpubr::geom_exec(ggplot2::geom_point, data = d[d$n.censor > 0, , drop = FALSE],
                          colour = surv.color, size = size*4.5, shape = "+")
  }

  # Add pvalue
  if(pval & !is.null(fit$strata)){
    pval <- .get_pvalue(fit, method = log.rank.weights, data = data)
    pvaltxt <- ifelse(pval$val < 1e-04, "p < 0.0001",
                    paste("p =", signif(pval$val, 2)))

    pval.x <- ifelse(is.null(pval.coord[1]), max(fit$time)/50, pval.coord[1])
    pval.y <- ifelse(is.null(pval.coord[2]), 0.2, pval.coord[2])
    p <- p + ggplot2::annotate("text", x = pval.x, y = pval.y,
                               label = pvaltxt, size = pval.size, hjust = 0)
    if(pval.method){
      pvalmethod <- pval$method
      pval.method.x <- ifelse(is.null(pval.method.coord[1]), max(fit$time)/50, pval.method.coord[1])
      pval.method.y <- ifelse(is.null(pval.method.coord[2]), 0.3, pval.method.coord[2])
      p <- p + ggplot2::annotate("text", x = pval.method.x, y = pval.method.y,
                                 label = pvalmethod, size = pval.method.size, hjust = 0)
    }
  }

  # Drawing a horizontal line at 50% survival
  #if(surv.scale == "percent") fun <- "pct"
  if(surv.median.line %in% c("hv", "h", "v"))
    p <- .add_surv_median(p, fit, type = surv.median.line, fun = fun, data = data)

  # Axis limits
  p <- p + ggplot2::expand_limits(x = 0, y = 0)
  # Axis label and legend title
  lty.leg.title <- ifelse(linetype == "strata", legend.title, linetype)
  p <- p + ggplot2::labs(x = xlab, y = ylab, title = title,
                         color = legend.title, fill = legend.title,
                         linetype = lty.leg.title
                         )
  p <-  .set_general_gpar(p, legend = legend, ...) # general graphical parameters
  if(!is.null(linetype.manual)) p <- p + scale_linetype_manual(values = linetype.manual)

  res <- list(plot = p)

  # Extract strata colors used in survival curves
  # Will be used to color the y.text of risk table and cumevents table
  g <- ggplot_build(p)
  scurve_cols <- unlist(unique(g$data[[1]]["colour"]))
  if(length(scurve_cols)==1) scurve_cols <- rep(scurve_cols, length(legend.labs))
  names(scurve_cols) <- legend.labs # Give every color an appropriate name


  # Add risk table
   if(risk.table){
     if(risk.table.pos == "in") risk.table.col = surv.color
     risktable <- ggrisktable(fit, data = data, type = risk.table.type, color = risk.table.col,
                             palette = palette, break.time.by = break.time.by,
                             xlim = xlim, title = risk.table.title,
                             legend = legend, legend.title = legend.title, legend.labs = legend.labs,
                             y.text = risk.table.y.text, y.text.col = risk.table.y.text.col,
                             fontsize = risk.table.fontsize, ggtheme = ggtheme,
                             xlab = xlab, ylab = legend.title,
                             ...)
     # For backward compatibility
     risktable <-  .set_general_gpar(risktable, legend = legend, ...) # general graphical parameters
     risktable <- .set_risktable_gpar(risktable, legend = legend, ...) # specific graphical params
     risktable <- risktable  + tables.theme
     if(!risk.table.y.text) risktable <- .set_large_dash_as_ytext(risktable)
     # color risk.table ticks by strata
     if(risk.table.y.text.col)
       risktable <- risktable + theme(axis.text.y = element_text(colour = rev(scurve_cols)))
     res$table <- risktable
   }

 # Add the cumulative number of events
  if(cumevents){
    res$cumevents <- ggcumevents (fit, data = data, color = cumevents.col,
                                  palette = palette, break.time.by = break.time.by,
                                  xlim = xlim, title = cumevents.title,
                                  legend = legend, legend.title = legend.title, legend.labs = legend.labs,
                                  y.text = cumevents.y.text, y.text.col = cumevents.y.text.col,
                                  fontsize = fontsize, ggtheme = ggtheme, xlab = xlab, ylab = legend.title,
                                  ...)
    res$cumevents <- res$cumevents + tables.theme
    if(!cumevents.y.text) res$cumevents <- .set_large_dash_as_ytext(res$cumevents)
    if(cumevents.y.text.col)
      res$cumevents <- res$cumevents + theme(axis.text.y = element_text(colour = rev(scurve_cols)))
  }

  # Add ncensor.plot or cumcensor plot
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if(ncensor.plot){
  ncensor_plot <- ggplot(d, aes_string("time", "n.censor")) +
    ggpubr::geom_exec(geom_bar, d, color = surv.color, fill = surv.color,
               stat = "identity", position = "dodge")+
    coord_cartesian(xlim = xlim)+
    scale_x_continuous(breaks = times)+
    scale_y_continuous(breaks = sort(unique(d$n.censor))) +
    ggtheme

  ncensor_plot <- ggpubr::ggpar(ncensor_plot, palette = palette)
  ncensor_plot <- ncensor_plot + ggplot2::labs(color = legend.title, fill = legend.title,
                                               x = xlab, y = "n.censor", title = ncensor.plot.title)

  # For backward compatibility
  ncensor_plot <-  .set_general_gpar(ncensor_plot,  ...) # general graphical parameters
  ncensor_plot <- .set_ncensorplot_gpar(ncensor_plot,  ...) # specific graphical params
  ncensor_plot <- ncensor_plot + tables.theme
  }
  else if(cumcensor){
    ncensor_plot <- ggcumcensor (fit, data = data, color = cumcensor.col,
                                  palette = palette, break.time.by = break.time.by,
                                  xlim = xlim, title = cumcensor.title,
                                  legend = legend, legend.title = legend.title, legend.labs = legend.labs,
                                  y.text = cumcensor.y.text, y.text.col = cumcensor.y.text.col,
                                  fontsize = fontsize, ggtheme = ggtheme, xlab = xlab, ylab = legend.title,
                                  ...)
    ncensor_plot <- ncensor_plot + tables.theme
    if(!cumcensor.y.text) ncensor_plot <- .set_large_dash_as_ytext(ncensor_plot)
    if(cumcensor.y.text.col)
      ncensor_plot <- ncensor_plot + theme(axis.text.y = element_text(colour = rev(scurve_cols)))
  }
  if(ncensor.plot | cumcensor)
    res$ncensor.plot <- ncensor_plot

  # Defining attributs for ggsurvplot
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  heights <- list(
    plot =  surv.plot.height,
    table =  ifelse(risk.table, risk.table.height, 0),
    ncensor.plot = ifelse(ncensor.plot | cumcensor, ncensor.plot.height, 0),
    cumevents = ifelse(cumevents, cumevents.height, 0)
  )
  y.text <- list(
    table =  risk.table.y.text,
    cumevents = cumevents.y.text,
    cumcensor = cumcensor.y.text
  )
  y.text.col <- list(
    table =  risk.table.y.text.col,
    cumevents = cumevents.y.text.col,
    cumcensor = cumcensor.y.text.col
  )

  # Returning the data used to generate the survival plots
  res$data.survplot <- d
  res$data.survtable <- .get_timepoints_survsummary(fit, data, times)

  class(res) <- c("ggsurvplot", "ggsurv", "list")
  attr(res, "heights") <- heights
  attr(res, "y.text") <- y.text
  attr(res, "y.text.col") <- y.text.col
  attr(res, "legend.position") <- legend
  attr(res, "legend.labs") <- legend.labs
  attr(res, "cumcensor") <- cumcensor
  attr(res, "risk.table.pos") <- risk.table.pos
  res
}

#' @param x an object of class ggsurvplot
#' @method print ggsurvplot
#' @param newpage open a new page. See \code{\link{grid.arrange}}
#' @rdname ggsurvplot
#' @export
print.ggsurvplot <- function(x, surv.plot.height = NULL, risk.table.height = NULL, ncensor.plot.height = NULL, newpage = TRUE, ...){

  res <- .build_ggsurvplot(x = x, surv.plot.height = surv.plot.height,
                    risk.table.height = risk.table.height,
                    ncensor.plot.height = ncensor.plot.height)
  if(newpage) grid::grid.newpage()
  grid::grid.draw(res)

}


# Build ggsurvplot for printing
.build_ggsurvplot <- function(x, surv.plot.height = NULL,
                              risk.table.height = NULL, ncensor.plot.height = NULL,
                              cumevents.height = NULL, ...)
{
  if(!inherits(x, "ggsurvplot"))
    stop("An object of class ggsurvplot is required.")
  heights <- attr(x, "heights")
  y.text <- attr(x, "y.text")
  y.text.col <- attr(x, "y.text.col")
  cumcensor <- attr(x, "cumcensor")

  risk.table.pos <- attr(x, "risk.table.pos")
  if(risk.table.pos == "in") x <- .put_risktable_in_survplot(x)

  nplot <- .count_ggplots(x)
  # Removing data components from the list and keep only plot objects
  x$data.survplot <- x$data.survtable <-  NULL
  # Extract legend from the survival plot
  legend.position <- attr(x, "legend.position")[1]
  legend.grob <- .get_legend(x$plot)

  # Update heights
  if(!is.null(surv.plot.height))  heights$plot <- surv.plot.height
  if(!is.null(risk.table.height))  heights$table <- risk.table.height
  if(!is.null(ncensor.plot.height))  heights$ncensor.plot <- ncensor.plot.height
  if(!is.null(cumevents.height))  heights$cumevents <- cumevents.height
  heights$plot <- 1 - heights$table - heights$ncensor.plot - heights$cumevents

  # Extract strata colors for survival curves
  legend.labs <- attr(x, "legend.labs")
  g <- ggplot_build(x$plot)
  cols <- unlist(unique(g$data[[1]]["colour"]))
  if(length(cols)==1) cols <- rep(cols, length(legend.labs))
  names(cols) <- legend.labs # Give every color an appropriate name

  if(nplot > 1 & legend.position %in% c("left", "right", "bottom")) x$plot <- .hide_legend(x$plot)

  if(!is.null(x$table)){
    x$table <- .hide_legend(x$table)
    if(!y.text$table) x$table <- .set_large_dash_as_ytext(x$table)
    # Make sure that risk.table.y.text.col will be the same as the plot legend colors
    if(y.text.col$table)
      x$table <- x$table + ggplot2::theme(axis.text.y = ggplot2::element_text(colour = rev(cols)))
  }


  if(!is.null(x$cumevents)){
    x$cumevents <- .hide_legend(x$cumevents)
    if(!y.text$cumevents) x$cumevents <- .set_large_dash_as_ytext(x$cumevents)
    # Make sure that y.text.col will be the same as the plot legend colors
    if(y.text.col$cumevents)
      x$cumevents <- x$cumevents + ggplot2::theme(axis.text.y = ggplot2::element_text(colour = rev(cols)))
  }


  if(!is.null(x$ncensor.plot)){
    x$ncensor.plot <- x$ncensor.plot + theme (legend.position = "none")
    if(cumcensor){
      if(!y.text$cumcensor) x$ncensor.plot <- .set_large_dash_as_ytext(x$ncensor.plot)
      if(y.text.col$cumcensor)
        x$ncensor.plot <- x$ncensor.plot + theme(axis.text.y = ggplot2::element_text(colour = rev(cols)))
    }
  }

  if(is.null(x$table) & is.null(x$ncensor.plot) & is.null(x$cumevents)) return(x$plot)

  heights <- unlist(heights)[names(x)] # get the height of each component in x
  plots <- x
  grobs <- widths <- list()
  for (i in 1:length(plots)) {
    if(is.ggplot(plots[[i]])){
      grobs[[i]] <- ggplotGrob(plots[[i]])
      widths[[i]] <- grobs[[i]]$widths[2:5]
    }
  }
  maxwidth <- do.call(grid::unit.pmax, widths)
  for (i in 1:length(grobs)) {
    grobs[[i]]$widths[2:5] <- as.list(maxwidth)
  }

  ggsurv <- gridExtra::arrangeGrob(grobs = grobs, nrow = nplot, heights = unlist(heights))

  # Set legend
  if(nplot > 1 & legend.position %in% c("left", "right", "bottom") & is.null(legend.grob)){
    ggsurv <- switch(legend.position,
                   bottom = gridExtra::arrangeGrob(grobs = list(ggsurv, legend.grob), nrow = 2, heights = c(0.9, 0.1)),
                   top = gridExtra::arrangeGrob(grobs = list(legend.grob, ggsurv), nrow = 2, heights = c(0.1, 0.9)),
                   right = gridExtra::arrangeGrob(grobs = list(ggsurv, legend.grob), ncol = 2, widths = c(0.75, 0.25)),
                   left = gridExtra::arrangeGrob(grobs = list(legend.grob, ggsurv), ncol = 2, widths = c(0.25, 0.75)),
                   ggsurv
                  )
  }

  return(ggsurv)
}

.hide_legend <- function(p){
p <- p + theme(legend.position = "none")
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
.get_pvalue <- function(fit, method, data = NULL){

  data <- .get_data(fit, data)

  # One group
  if(length(levels(summary(fit)$strata)) == 0)  return(list(val = NULL, method = NULL))

  if(method == "survdiff") {
    ssubset <- fit$call$subset
    if(is.null(ssubset))
      sdiff <- survival::survdiff(eval(fit$call$formula), data = data)
    else
      sdiff <- survival::survdiff(eval(fit$call$formula), data = data,
                                     subset = eval(fit$call$subset))
    pvalue <- stats::pchisq(sdiff$chisq, length(sdiff$n) - 1, lower.tail = FALSE)
    return(list(val = pvalue, method = "Log-rank"))
  } else {
    tenfit <- ten(eval(fit$call$formula), data = data)
    capture.output(comp(tenfit)) -> null_dev
    # comp modifies tenfit object (ten class: ?survMisc::ten)
    # and adds attributes with tests
    attributes(tenfit)$lrt -> tests
    # check str(tests) -> W:weights / pNorm:p-values
    pvalue <- round(tests$pNorm[tests$W == method], 4)
    test_name <- c("Log-rank", "Gehan-Breslow",
                   "Tarone-Ware", "Peto-Peto",
                   "modified Peto-Peto", "Fleming-Harrington (p=1, q=1)")
    # taken from ?survMisc::comp
    return(list(val = pvalue, method = test_name[tests$W == method]))
  }
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
.connect2origin <- function(d, fit, data = NULL){
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
    variables <- .get_variables(base$strata,  fit, data)
    for(variable in variables) base[[variable]] <- .get_variable_value(variable, base$strata, fit, data)
    }
  }
  d <- rbind(base, d)
  d
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
    if(risk.table %in% c("absolute", "percentage", "abs_pct", "nrisk_cumcensor", "nrisk_cumevents") )
      res <- list(display = TRUE, type = risk.table)
    else stop("Allowed values for risk.table are: TRUE, FALSE, 'absolute', 'percentage', 'nrisk_cumcensor', 'nrisk_cumevents' ")
  }
  res
}

# Drawing horizontal line at 50% median survival
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.add_surv_median <-function(p, fit, type = "hv", fun = NULL, data = NULL){
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
        variables <- .get_variables(df$strata, fit, data)
        for(variable in variables) df[[variable]] <- .get_variable_value(variable, df$strata, fit, data)
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


# Graphical parameters
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# general graphical parameters to be applied to
# survival curves, risk.table, ncensor.plot
.set_general_gpar <- function(p, legend = "top", ...){
  extra.params <- list(...)
  ggpubr::ggpar(p = p, font.main = extra.params$font.main, font.x = extra.params$font.x,
                font.y = extra.params$font.y, font.submain = extra.params$font.submain,
                font.caption = extra.params$font.caption,
                font.tickslab = extra.params$font.tickslab,
                legend = legend, font.legend = extra.params$font.legend)
}

# Specific graphical params to risk.table
.set_risktable_gpar <- function(p, legend = "none",  ...){
  extra.params <- list(...)
  ggpubr::ggpar(p,
                 subtitle = extra.params$risk.table.subtitle,
                 caption = extra.params$risk.table.caption,
                 font.title = extra.params$font.risk.table.title,
                 font.x = extra.params$font.risk.table.x,
                 font.y = extra.params$font.risk.table.y,
                 font.submain = extra.params$font.risk.table.subtitle,
                 font.caption = extra.params$font.risk.table.caption,
                 legend = legend
  )
}

# Specific graphical params to ncensor_plot
.set_ncensorplot_gpar <- function(p, legend = "none", ...){
  extra.params <- list(...)
  ggpubr::ggpar(p,
             subtitle = extra.params$ncensor.plot.subtitle,
             caption = extra.params$ncensor.plot.caption,
             font.main = extra.params$font.ncensor.plot.title,
             font.submain = extra.params$font.ncensor.plot.subtitle,
             font.caption = extra.params$font.ncensor.plot.caption,
             font.tickslab = extra.params$font.ncensor.plot.tickslab,
             font.x = extra.params$font.ncensor.plot.x,
             font.y = extra.params$font.ncensor.plot.y,
             legend = legend)

}

# Put risk table inside main plot
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.put_risktable_in_survplot <- function(ggsurv){

  if(is.null(ggsurv$table)) return(ggsurv)

  if(is.null(ggsurv$table))
    stop("You can't put risk table inside the main plot because risk.table = FALSE. Use risk.table = TRUE")

  # Create a transparent theme object
  theme_transparent<- function() {
    theme(
    title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    plot.margin=unit(c(0,0,0,0),"mm"),
    panel.border = element_blank(),
    legend.position = "none")
  }

  survplot <- ggsurv$plot
  risktable <- ggsurv$table + theme_transparent()
  nstrata <- length(levels(survplot$data$strata))
  .time <- survplot$data$time
  ymax <- nstrata*0.05
  risktable_grob = ggplotGrob(risktable)
  survplot <- survplot + annotation_custom(grob = risktable_grob, xmin = -max(.time)/20,
                                           ymin = -0.05, ymax = ymax)
  ggsurv$plot <- survplot
  ggsurv$table <- NULL
  ggsurv
}
