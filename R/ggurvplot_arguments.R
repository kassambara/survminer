#'ggsurvplot Argument Descriptions
#'
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
#'@param xscale numeric or character value specifying x-axis scale. \itemize{
#'  \item If numeric, the value is used to divide the labels on the x axis. For
#'  example, a value of 365.25 will give labels in years instead of the original
#'  days. \item If character, allowed options include one of c("d_m", "d_y",
#'  "m_d", "m_y", "y_d", "y_m"), where d = days, m = months and y = years. For
#'  example, xscale = "d_m" will transform labels from days to months; xscale =
#'  "m_y", will transform labels from months to years.}
#'@param color color to be used for the survival curves. \itemize{ \item If the
#'  number of strata/group (n.strata) = 1, the expected value is the color name.
#'  For example color = "blue". \item If n.strata > 1, the expected value is the
#'  grouping variable name. By default, survival curves are colored by strata
#'  using the argument color = "strata",  but you can also color survival curves
#'  by any other grouping variables used to fit the survival curves. In this
#'  case, it's possible to specify a custom color palette by using the argument
#'  palette.}
#'@param palette the color palette to be used. Allowed values include "hue" for
#'  the default hue color scale; "grey" for grey color palettes; brewer palettes
#'  e.g. "RdBu", "Blues", ...; or custom color palette e.g. c("blue", "red"); and scientific journal palettes from ggsci R package, e.g.: "npg",
#'   "aaas", "lancet", "jco", "ucscgb", "uchicago", "simpsons" and
#'   "rickandmorty".
#'  See details section for more information.  Can be also a numeric vector of
#'  length(groups); in this case a basic color palette is created using the
#'  function \link[grDevices]{palette}.
#'@param linetype line types. Allowed values includes i) "strata" for changing
#'  linetypes by strata (i.e. groups); ii) a numeric vector (e.g., c(1, 2)) or a
#'  character vector c("solid", "dashed").
#'@param break.time.by numeric value controlling time axis breaks. Default value
#'  is NULL.
#'@param break.x.by alias of break.time.by. Numeric value controlling x axis
#'  breaks. Default value is NULL.
#'@param break.y.by same as break.x.by but for y axis.
#'@param conf.int logical value. If TRUE, plots confidence interval.
#'@param conf.int.fill fill color to be used for confidence interval.
#'@param conf.int.style confidence interval style. Allowed values include
#'  c("ribbon", "step").
#'@param conf.int.alpha numeric value specifying fill color transparency. Value
#'  should be in [0, 1], where 0 is full transparency and 1 is no transparency.
#'@param censor logical value. If TRUE, censors will be drawn.
#'@param censor.shape character or numeric value specifying the point shape of
#'  censors. Default value is "+" (3), a sensible choice is "|" (124).
#'@param censor.size numveric value specifying the point size of censors.
#'  Default is 4.5.
#'@param pval logical value, a numeric or a string. If logical and TRUE, the
#'  p-value is added on the plot. If numeric, than the computet p-value is
#'  substituted with the one passed with this parameter. If character, then the
#'  customized string appears on the plot. See examples - Example 3.
#'@param pval.size numeric value specifying the p-value text size. Default is 5.
#'@param pval.coord numeric vector, of length 2, specifying the x and y
#'  coordinates of the p-value. Default values are NULL.
#'@param title,xlab,ylab main title and axis labels
#'@param xlim,ylim x and y axis limits e.g. xlim = c(0, 1000), ylim = c(0, 1).
#'@param axes.offset logical value. Default is TRUE. If FALSE, set the plot axes
#'  to start at the origin.
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
#'  "absolute" or "percentage". Shows the \bold{absolute number} and the
#'  \bold{percentage} of subjects at risk by time, respectively. \item "abs_pct"
#'  to show both absolute number and percentage. \item "nrisk_cumcensor" and
#'  "nrisk_cumevents". Show the number at risk and, the cumulative number of
#'  censoring and events, respectively. }
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
#'@param tables.y.text.col logical. Default value is FALSE. If TRUE, tables tick
#'  labels will be colored by strata.
#'@param tables.col color to be used for all tables under the main plot. Default
#'  value is "black". If you want to color by strata (i.e. groups), use
#'  tables.col = "strata".
#'@param tables.theme function, ggplot2 theme name. Default value is
#'  \link{theme_survminer}. Allowed values include ggplot2 official themes: see
#'  \code{\link[ggplot2]{theme}}. Note that, \code{tables.theme} is incremental to \code{ggtheme}.
#'@param risk.table.height the height of the risk table on the grid. Increase
#'  the value when you have many strata. Default is 0.25. Ignored when
#'  risk.table = FALSE.
#'@param surv.plot.height the height of the survival plot on the grid. Default
#'  is 0.75. Ignored when risk.table = FALSE.
#'@param ncensor.plot logical value. If TRUE, the number of censored subjects at
#'  time t is plotted. Default is FALSE. Ignored when cumcensor = TRUE.
#'@param ncensor.plot.title The title to be used for the censor plot. Used when
#'  \code{ncensor.plot = TRUE}.
#'@param ncensor.plot.height The height of the censor plot. Used when
#'  \code{ncensor.plot = TRUE}.
#'@param cumevents logical value specifying whether to show or not the table of
#'  the cumulative number of events. Default is FALSE.
#'@param cumevents.title The title to be used for the cumulative events table.
#'@param cumevents.col same as tables.col but for the cumulative events table
#'  only.
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
#'  as linetype, size, ii) or to the function \link[ggpubr]{ggpar}() for
#'  customizing the plots. See details section.
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
#'@name ggsurvplot_arguments
NULL
