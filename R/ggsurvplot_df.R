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
  if(!is.null(break.y.by)) y.breaks <- seq(0, 1, by = break.y.by)
  # Axis limits
  xmin <- ifelse(.is_cloglog(fun), min(c(1, df$time)), 0)
  if(is.null(xlim)) xlim <- c(xmin, max(df$time))
  if(is.null(ylim) & is.null(fun)) ylim <- c(0, 1)

  # Axis offset
  .expand <- ggplot2::waiver()
  if(!axes.offset) .expand <- c(0, 0)

  # Drawing survival curves
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  df[, .strata.var] <- factor( df[, .strata.var], levels = .levels(.strata), labels = legend.labs)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = !!sym("time"), y = !!sym("surv"))) +
    ggpubr::geom_exec(surv.geom, data = df, size = size, color = color, linetype = linetype, ...) +
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
    times <- seq(0, max(c(df$time, xlim)), by = break.time.by)

  xticklabels <- .format_xticklabels(labels = times, xscale = xscale)

  if(!.is_cloglog(fun)) {
    p <- p + ggplot2::scale_x_continuous(breaks = times, labels = xticklabels, expand = .expand) +
      ggplot2::expand_limits(x = 0, y = 0)
  }
  else p <- p + ggplot2::scale_x_continuous(breaks = times, trans = "log10", labels = xticklabels)

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

  pms <- list(
    data = df, color = color, palette = palette,
    break.time.by =  break.time.by,
    xlim = xlim,
    legend = legend, legend.title = legend.title,
    legend.labs = legend.labs, xlog = xlog,
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
  origin[intersect(c('time', 'n.censor', 'std.err', "n.event"), colnames(origin))] <- 0
  origin[c('surv', 'upper', 'lower')] <- 1.0
  dplyr::bind_rows(origin, d)
}



# Adjust linetype manually
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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

