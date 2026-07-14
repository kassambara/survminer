#' @include utilities.R
#' @importFrom ggplot2 geom_step
NULL
#'Plot Survival Curves from Survival Summary Data Frame
#'
#'@description An extension to \link{ggsurvplot}() to plot survival curves from
#'  any data frame containing the summary of survival curves as returned the
#'  \link{surv_summary}() function.
#'
#'  Might be useful for a user who wants
#'  to use \link{ggsurvplot} for visualizing survival curves computed by another
#'  method than the standard \link[survival]{survfit.formula} function. In this
#'  case, the user has just to provide the data frame containing the summary of
#'  the survival analysis.
#'
#'@inheritParams ggsurvplot_arguments
#'@param fit a data frame as returned by surv_summary. Should contains at least
#'  the following columns: \itemize{ \item time: survival time \item surv:
#'  survival probability \item strata: grouping variables \item n.censor: number
#'  of censors \item upper: upper end of confidence interval \item lower: lower
#'  end of confidence interval }
#' @param surv.geom survival curve style. Is the survival curve entered a step
#'  function (\link[ggplot2]{geom_step}) or a smooth function (\link[ggplot2]{geom_line}).
#' @param linejoin line join style for the survival curve, passed to the survival
#'  geom. Default is \code{"round"} (unchanged). Use \code{"mitre"} for sharp,
#'  precisely-marked corners at event times (requires a \pkg{ggplot2} version that
#'  passes \code{linejoin} through \code{\link[ggplot2]{geom_step}}).
#'
#' @examples
#' library(survival)
#'
#'# Fit survival curves
#'#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'fit1 <- survfit( Surv(time, status) ~ 1, data = colon)
#'fit2 <- survfit( Surv(time, status) ~ adhere, data = colon)
#'
#'# Summary
#'#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'head(surv_summary(fit1, colon))
#'
#'head(surv_summary(fit2, colon))
#'
#'# Visualize
#'#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'ggsurvplot_df(surv_summary(fit1, colon))
#'
#'ggsurvplot_df(surv_summary(fit2, colon), conf.int = TRUE,
#'              legend.title = "Adhere", legend.labs = c("0", "1"))
#'
#'# Kaplan-Meier estimate
#'#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'out_km <- survfit(Surv(time, status) ~ 1, data = lung)
#'
#'# Weibull model
#'#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'wb <- survreg(Surv(time, status) ~ 1, data = lung)
#'s <- seq(.01, .99, by = .01)
#'t <- predict(wb, type = "quantile", p = s, newdata = lung[1, ])
#'out_wb <- data.frame(time = t, surv = 1 - s, upper = NA, lower = NA, std.err = NA)
#'
#'# plot both
#'#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'p_km <- ggsurvplot(out_km, conf.int = FALSE)
#'p_wb <- ggsurvplot(out_wb, conf.int = FALSE, surv.geom = geom_line)
#'
#'p_km
#'p_wb
#'p_km$plot + geom_line(data = out_wb, aes(x = time, y = surv))
#'
#'@export
ggsurvplot_df <- function(fit, fun = NULL,
                          color = NULL, palette = NULL, linetype = 1,
                          break.x.by = NULL, break.time.by = NULL, break.y.by = NULL,
                          surv.scale = c("default", "percent"), surv.geom = geom_step,
                          linejoin = "round",
                          xscale = 1,
                          conf.int = FALSE, conf.int.fill = "gray", conf.int.style = "ribbon",
                          conf.int.alpha = 0.3,
                          censor = TRUE, censor.shape = "+", censor.size = 4.5,
                          title = NULL,  xlab = "Time", ylab = "Survival probability",
                          xlim = NULL, ylim = NULL, axes.offset = TRUE,
                          legend = c("top", "bottom", "left", "right", "none"),
                          legend.title = "Strata", legend.labs = NULL,
                          ggtheme = theme_survminer(),
                          ...)
{

  .dots <- list(...)

  if(!inherits(fit, "data.frame"))
    stop("fit should be a data frame.")
  df <- fit

  # Negative survival times are not meaningful for a Kaplan-Meier estimate and
  # make the curve appear to increase. survfit()/survminer plot them as-is, so
  # warn the user rather than silently drawing a misleading (up-ticking) curve
  # (#523). This is the common draw point for all entry points (ggsurvplot(),
  # ggsurvplot_combine(), ggsurvplot_facet(), ...), so the warning fires once
  # per plot regardless of route. The plot itself is unchanged.
  if(any(df$time < 0, na.rm = TRUE))
    warning("Negative survival times are present in the data. ",
            "The survival curve can appear to increase, which is not meaningful ",
            "for a Kaplan-Meier estimate. Consider removing observations with ",
            "negative times.", call. = FALSE)

  # if(!is.null(.dots$risk.table))
  #   warning("Can't extract risk.table from a data frame.", call. = FALSE)
  # if(!is.null(.dots$pval))
  #   warning("Can't compute pvalue from a data frame.", call. = FALSE)

  size <- ifelse(is.null(list(...)$size), 1, list(...)$size) # point and line size
  if(!is(legend, "numeric")) legend <- match.arg(legend)
  if(!is.numeric(xscale) & !(xscale %in% c("d_m", "d_y", "m_d", "m_y", "y_d", "y_m")))
    stop('xscale should be numeric or one of c("d_m", "d_y", "m_d", "m_y", "y_d", "y_m").')

  # Adapt ylab value according to the value of the argument fun
  ylab <- .check_ylab(ylab, fun)
  # Check and get linetypes
  lty <- .get_lty(linetype)
  linetype <- lty$lty
  linetype.manual <- lty$lty.manual

  # Data preparation
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # one group
  if(is.null(df$strata)){
    df$strata <- as.factor(rep("All", nrow(df)))
    if(missing(conf.int)){
      conf.int = TRUE
      conf.int.fill = "strata"
    }
  }


  # Coloring variable
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  if(is.null(color))
    color <- .strata.var <- "strata"
  else if(color %in% colnames(df)){
    .strata.var <- color
  }
  else {
   warning("Now, to change color palette, use the argument palette= '", color, "' ",
            "instead of color = '", color, "'", call. = FALSE)
    palette <- color
    .strata.var <- "strata"
  }

  # Number of strata and strata names
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .strata <- df[, .strata.var]
  strata_names <- .levels(.strata)
  n.strata <- length(strata_names)

  # Check legend labels and title
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  if(!is.null(legend.labs)){
    if(n.strata != length(legend.labs))
      stop("The length of legend.labs should be ", n.strata )
  }
  else if(is.null(legend.labs))
    legend.labs <- strata_names

  # Native plotmath in legend.labs (#350): an expression() or a list of language
  # objects renders as math. Keep a character version for all internal logic
  # (factor levels, colour extraction, tables) so the character path is
  # byte-identical; the expressions are overlaid on the legend at the end.
  legend.labs.math <- NULL
  if(.is_plotmath(legend.labs)){
    legend.labs.math <- legend.labs
    legend.labs <- .plotmath_to_char(legend.labs)
  }

  if(is.null(legend.title)) legend.title <- .strata.var

  # Connect surv data to the origin for plotting: time = 0, surv = 1
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  if(!.is_cloglog(fun)) df <- .connect2origin(df)

  #  Transformation of the survival curve
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  df <- .apply_surv_func(df, fun = fun)

  # Scale transformation
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  surv.scale <- match.arg(surv.scale)
  scale_labels <-  ggplot2::waiver()
  if (surv.scale == "percent") scale_labels <- scales::percent
  xlog <- .is_cloglog(fun)

  y.breaks <- ggplot2::waiver()
  if(!is.null(break.y.by)){
    # Derive the break range from the displayed y-range instead of the hardcoded
    # 0..1: for transformed curves (fun = "cloglog"/"event"/"cumhaz", or a custom
    # ylim) the y-values fall outside [0, 1], so seq(0, 1, ...) produced too few
    # (or no) breaks (#378, #442). The default survival plot (fun = NULL, no
    # ylim) still uses 0..1, so its breaks are unchanged.
    y.range <- ylim
    if(is.null(y.range) && is.null(fun)) y.range <- c(0, 1)
    if(is.null(y.range)) y.range <- range(df$surv, na.rm = TRUE)
    # round the endpoints out to multiples of break.y.by so the breaks are on a
    # clean grid; breaks that fall outside the axis limits are dropped by ggplot2
    y.breaks <- seq(floor(min(y.range) / break.y.by) * break.y.by,
                    ceiling(max(y.range) / break.y.by) * break.y.by,
                    by = break.y.by)
  }
  # Axis limits
  xmin <- ifelse(.is_cloglog(fun), min(c(1, df$time)), min(c(0, df$time), na.rm = TRUE))
  if(is.null(xlim)) xlim <- c(xmin, max(df$time))
  if(is.null(ylim) & is.null(fun)) ylim <- c(0, 1)

  # Axis offset
  .expand <- ggplot2::waiver()
  if(!axes.offset) .expand <- c(0, 0)

  # Drawing survival curves
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  df[, .strata.var] <- factor( df[, .strata.var], levels = .levels(.strata), labels = legend.labs)

  # `size` is captured above and forwarded as `linewidth`; drop it from the extra
  # dots so ggpubr::geom_exec() does not also set the deprecated `size` line
  # aesthetic (deprecated in ggplot2 3.4.0). `linewidth` already carries the value,
  # so the drawn width is unchanged.
  .extra <- list(...)
  .extra$size <- NULL
  surv.layer <- do.call(ggpubr::geom_exec,
                        c(list(surv.geom, data = df, linewidth = size,
                               color = color, linetype = linetype), .extra))
  # ggpubr::geom_exec() does not forward `linejoin` to the step geom, so set it
  # directly on the built layer. The default "round" reproduces geom_step()'s own
  # default, so the curve is byte-identical unless the user opts into another join
  # such as "mitre", which marks event-time corners precisely (#653). Guarded so
  # the default path never touches the layer, and so a custom surv.geom without a
  # linejoin parameter is left untouched.
  if (!identical(linejoin, "round") &&
      !is.null(surv.layer$geom_params) &&
      "linejoin" %in% names(surv.layer$geom_params)) {
    surv.layer$geom_params$linejoin <- linejoin
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = !!sym("time"), y = !!sym("surv"))) +
    surv.layer +
    ggplot2::scale_y_continuous(breaks = y.breaks, labels = scale_labels, limits = ylim, expand = .expand) +
    ggplot2::coord_cartesian(xlim = xlim)+
    ggtheme
  p <- ggpubr::ggpar(p, palette = palette, ...)

  # Format axis ticks
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  if(!is.null(break.x.by))
    break.time.by <- break.x.by

  times <- .get_default_breaks(df$time, .log = xlog)

  if(!is.null(break.time.by) & !xlog)
    times <- .time_breaks(break.time.by, max(c(df$time, xlim)))

  xticklabels <- .format_xticklabels(labels = times, xscale = xscale)

  if(!.is_cloglog(fun)) {
    p <- p + ggplot2::scale_x_continuous(breaks = times, labels = xticklabels, expand = .expand) +
      ggplot2::expand_limits(x = 0, y = 0)
  }
  else p <- p + .scale_x_log10(breaks = times, labels = xticklabels)

  # Add confidence interval
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  if(conf.int){
    if(missing(conf.int.fill)) conf.int.fill <- color
    else if(length(conf.int.fill) == 1 & conf.int.fill[1] == "gray" & n.strata > 1 )
      conf.int.fill <- color
    if(conf.int.style == "ribbon"){
      p <- p + ggpubr::geom_exec(.geom_confint, data = df,
                                 ymin = "lower", ymax = "upper",
                                 fill = conf.int.fill,  alpha = conf.int.alpha, na.rm = TRUE)
    }
    else if(conf.int.style == "step"){
      p <- p + ggpubr::geom_exec(surv.geom, data = df,
                                 y = "lower", linetype = "dashed",
                                 color = color, na.rm = TRUE)+
        ggpubr::geom_exec(surv.geom, data = df,
                          y = "upper", linetype = "dashed",
                          color = color, na.rm = TRUE)
    }
  }

  # Add cencored
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  if (censor & any(df$n.censor >= 1)) {
    p <- p + ggpubr::geom_exec(ggplot2::geom_point, data = df[df$n.censor > 0, , drop = FALSE],
                               colour = color, size = censor.size, shape = censor.shape)
  }

  # Axis label and legend title
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  lty.leg.title <- ifelse(linetype == "strata", legend.title, linetype)
  
  # Build labs() arguments - only include aesthetics that are actually used
  labs_args <- list(x = xlab, y = ylab, title = title, color = legend.title)
  
  # Add fill legend only if confidence interval ribbon is used
  if(conf.int && conf.int.style == "ribbon") {
    labs_args$fill <- legend.title
  }
  
  # Add linetype legend only if linetype is used as an aesthetic mapping
  if(linetype == "strata") {
    labs_args$linetype <- lty.leg.title
  }
  
  p <- p + do.call(ggplot2::labs, labs_args)
  p <-  .set_general_gpar(p, legend = legend, ...) # general graphical parameters
  if(!is.null(linetype.manual)) p <- p + scale_linetype_manual(values = linetype.manual)

  # Overlay plotmath legend labels (#350). Gated: character labels leave
  # legend.labs.math NULL and never reach here, so the plot is byte-identical.
  if(!is.null(legend.labs.math))
    p <- .apply_plotmath_legend(p, legend.labs.math, grp.levels = legend.labs,
                                linetype = linetype, linetype.manual = linetype.manual)

  pms <- list(
    data = df, color = color, palette = palette,
    break.time.by =  break.time.by,
    xlim = xlim,
    legend = legend, legend.title = legend.title,
    legend.labs = legend.labs, legend.labs.math = legend.labs.math, xlog = xlog,
    time.breaks = times,
    xlab = xlab, ylab = ylab,
    xscale = xscale, xticklabels = xticklabels
  )
  attr(p, "parameters" ) <- pms

  p

}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Helper functions
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Data-frame input path WITH number-at-risk / cumulative tables (#409).
# ggsurvplot_df() itself only draws the curves (it is also the shared plot
# builder for the survfit paths, so it must keep returning a bare ggplot). When
# the user passes a surv_summary() data frame to ggsurvplot() and requests
# risk.table / cumevents / cumcensor, this builds the curve plot with
# ggsurvplot_df() and the tables from the data frame (which carries n.risk /
# n.event / n.censor), assembling the same compound "ggsurvplot" object the
# survfit path returns. Modeled on ggsurvplot_combine(); the tables are computed
# by .get_timepoints_from_df(), which matches the survfit risk table exactly.
.ggsurvplot_df_tables <- function(fit, risk.table = FALSE, cumevents = FALSE,
                                  cumcensor = FALSE, risk.table.pos = c("out", "in"),
                                  axes.offset = TRUE,
                                  fontsize = 4.5,
                                  tables.height = 0.25, tables.col = "black",
                                  tables.y.text = TRUE, tables.y.text.col = TRUE,
                                  risk.table.title = NULL, risk.table.col = tables.col,
                                  risk.table.fontsize = fontsize,
                                  risk.table.y.text = tables.y.text,
                                  risk.table.y.text.col = tables.y.text.col,
                                  risk.table.height = tables.height,
                                  surv.plot.height = 0.75,
                                  ncensor.plot.height = tables.height,
                                  cumevents.height = tables.height,
                                  cumcensor.height = tables.height,
                                  cumevents.col = tables.col, cumevents.title = NULL,
                                  cumevents.y.text = tables.y.text,
                                  cumevents.y.text.col = tables.y.text.col,
                                  cumcensor.col = tables.col, cumcensor.title = NULL,
                                  cumcensor.y.text = tables.y.text,
                                  cumcensor.y.text.col = tables.y.text.col,
                                  ggtheme = theme_survminer(), tables.theme = ggtheme, ...)
{
  risk.table.pos <- match.arg(risk.table.pos)
  risktable  <- .parse_risk_table_arg(risk.table);  risk.table <- risktable$display
  cumev      <- .parse_risk_table_arg(cumevents);   cumevents  <- cumev$display
  cumcens    <- .parse_risk_table_arg(cumcensor);   cumcensor  <- cumcens$display

  # Curve plot (bare ggplot). axes.offset is forwarded (as the old direct
  # ggsurvplot_df() call did via ...) so it is not silently dropped.
  p <- ggsurvplot_df(fit, ggtheme = ggtheme, axes.offset = axes.offset, ...)

  # No table requested -> behave exactly like ggsurvplot_df() (byte-identical).
  if(!(risk.table | cumevents | cumcensor))
    return(p)

  # A risk table can only be built from a surv_summary()-style data frame.
  fit <- as.data.frame(fit)
  needed <- c("time", "n.risk", "n.event", "n.censor")
  miss <- setdiff(needed, colnames(fit))
  if(length(miss) > 0)
    stop("Can't build a risk/events/censor table from this data frame: missing ",
         "the column(s) ", .collapse(miss, sep = ", "),
         ". Pass a surv_summary() data frame (or a survfit object).", call. = FALSE)

  res <- list(plot = p)
  scurve_cols <- .extract_ggplot_colors(p, grp.levels = attr(p, "parameters")$legend.labs)

  # Shared table params, taken from the plot then extended (mirrors ggsurvplot_core).
  pms <- attr(p, "parameters")
  surv.color <- pms$color   # the plot's colour aesthetic (usually "strata")
  pms$ggtheme <- ggtheme
  pms$tables.theme <- tables.theme
  pms$ylab <- pms$legend.title
  pms$axes.offset <- axes.offset
  # Time-point table from the data frame, wrapped in the (table, time) shape
  # ggsurvtable() accepts; matches the survfit path exactly.
  survtable <- .get_timepoints_from_df(fit, times = pms$time.breaks)
  pms$fit <- list(table = survtable, time = fit$time)

  # Each table is built with its OWN display type / colour / y-text, exactly as
  # ggsurvplot_core does (a single shared call would apply one type to all).
  if(risk.table){
    pms$survtable <- "risk.table"
    pms$risk.table.type <- risktable$type
    pms$title <- risk.table.title
    pms$fontsize <- risk.table.fontsize
    pms$y.text <- risk.table.y.text
    # In-plot table: colour the numbers by strata (the curve colours), as the
    # survfit path does -- surv.color is the plot's colour aesthetic ("strata").
    pms$color <- if(risk.table.pos == "in") surv.color else risk.table.col
    if(risk.table.y.text.col) pms$y.text.col <- scurve_cols
    pms$origin.align <- (risk.table.pos != "in")
    res$table <- do.call(ggsurvtable, pms)
  }
  if(cumevents){
    pms$survtable <- "cumevents"
    pms$risk.table.type <- cumev$type
    pms$title <- cumevents.title
    pms$fontsize <- fontsize
    pms$color <- cumevents.col
    pms$y.text <- cumevents.y.text
    if(cumevents.y.text.col) pms$y.text.col <- scurve_cols
    pms$origin.align <- TRUE
    res$cumevents <- do.call(ggsurvtable, pms)
  }
  if(cumcensor){
    pms$survtable <- "cumcensor"
    pms$risk.table.type <- cumcens$type
    pms$title <- cumcensor.title
    pms$fontsize <- fontsize
    pms$color <- cumcensor.col
    pms$y.text <- cumcensor.y.text
    if(cumcensor.y.text.col) pms$y.text.col <- scurve_cols
    pms$origin.align <- TRUE
    res$ncensor.plot <- do.call(ggsurvtable, pms)
  }

  # Component heights / y-text attributes (per table, as ggsurvplot_core).
  heights <- list(
    plot         = surv.plot.height,
    table        = ifelse(risk.table, risk.table.height, 0),
    ncensor.plot = ifelse(cumcensor, cumcensor.height, 0),
    cumevents    = ifelse(cumevents, cumevents.height, 0)
  )
  y.text     <- list(table = risk.table.y.text, cumevents = cumevents.y.text,
                     cumcensor = cumcensor.y.text)
  y.text.col <- list(table = risk.table.y.text.col, cumevents = cumevents.y.text.col,
                     cumcensor = cumcensor.y.text.col)

  res$data.survplot  <- fit
  res$data.survtable <- survtable

  class(res) <- c("ggsurvplot", "ggsurv", "list")
  attr(res, "heights") <- heights
  attr(res, "y.text") <- y.text
  attr(res, "y.text.col") <- y.text.col
  attr(res, "legend.position") <- pms$legend
  attr(res, "legend.labs") <- pms$legend.labs
  attr(res, "cumcensor") <- cumcensor
  attr(res, "risk.table.pos") <- risk.table.pos
  attr(res, "axes.offset") <- axes.offset
  res
}

# Adapt ylab according to the value of the argument fun
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.check_ylab <- function(ylab, fun){
  if(!is.null(fun) & is.character(fun)){
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

# Check user defined legend labels
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.check_legend_labs <- function(fit, legend.labs = NULL){

  if(!is.null(legend.labs) & !inherits(fit, c("survfit.cox", "survfitcox"))){

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
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.connect2origin <- function(d, ...){
  n.risk <- strata <- NULL
  # if("n.risk" %in% colnames(d)){d <- dplyr::arrange(d, dplyr::desc(n.risk))}
  origin <- d %>% dplyr::distinct(strata, .keep_all = TRUE)
  origin[intersect(c('n.censor', 'std.err', "n.event"), colnames(origin))] <- 0
  origin[c('surv', 'upper', 'lower')] <- 1.0
  # Origin x-position: 0 for ordinary data (byte-identical), or the smallest
  # observed time when negative, so each curve starts at the first time like
  # base plot.survfit() instead of injecting a spurious (0, 1) point that would
  # sort into the middle of negative-time data (#389).
  origin['time'] <- min(c(0, d$time), na.rm = TRUE)
  dplyr::bind_rows(origin, d)
}



# Adjust linetype manually
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.get_lty <- function(linetype){
  linetype.manual = NULL
  nlty <- length(linetype)
  # A length > 1 linetype is a per-strata vector: numeric codes, base line-type
  # names ("solid"/"dashed"/...), hex dash patterns ("F1", "4C1C"), or a mix.
  # Apply it manually across strata (scale_linetype_manual). Previously only an
  # all-base-name or all-numeric vector was recognised, so a vector containing a
  # hex pattern fell through and later crashed with "the condition has length > 1"
  # (#344). A single value (including "strata" or a lone hex pattern) is
  # unchanged: it is passed through as-is.
  if(nlty > 1){
    linetype.manual <- linetype
    linetype <- "strata"
  }
  list(lty = linetype, lty.manual = linetype.manual)
}



# Graphical parameters
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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

