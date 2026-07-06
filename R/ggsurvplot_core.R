#' @include utilities.R surv_summary.R ggsurvplot_df.R surv_pvalue.R ggsurvtable.R
#
# Core function to plot survival curves using ggplot2.
# Accepts only one survfit object. Internally called by the other \code{ggsurvplot_*()} family functions.
# The documentation of arguments are described at ?ggsurvplot
ggsurvplot_core <- function(fit, data = NULL, fun = NULL,
                            color = NULL, palette = NULL, linetype = 1,
                            break.x.by = NULL, break.y.by = NULL,  break.time.by = NULL,
                            surv.scale = c("default", "percent"), xscale = 1,
                            conf.int = FALSE, conf.int.fill = "gray", conf.int.style = "ribbon",
                            conf.int.alpha = 0.3,
                            censor = TRUE, censor.shape = "+", censor.size = 4.5,
                            pval = FALSE, pval.size = 5, pval.coord = c(NULL, NULL),
                            pval.parse = FALSE,
                            test.for.trend = FALSE,
                            pval.method = FALSE, pval.method.size = pval.size, pval.method.coord = c(NULL, NULL),
                            log.rank.weights = c("survdiff", "1", "n", "sqrtN", "S1", "S2", "FH_p=1_q=1"),
                            title = NULL,  xlab = "Time", ylab = "Survival probability",
                            xlim = NULL, ylim = NULL, axes.offset = TRUE,
                            legend = c("top", "bottom", "left", "right", "none"),
                            legend.title = "Strata", legend.labs = NULL,
                            fontsize = 4.5, font.family = "",
                            tables.height = 0.25, tables.y.text = TRUE, tables.col = "black",
                            tables.y.text.col = TRUE,
                            risk.table = FALSE, risk.table.pos = c("out", "in"), risk.table.title = NULL,
                            risk.table.col = tables.col, risk.table.fontsize = fontsize,
                            risk.table.y.text = tables.y.text,
                            risk.table.y.text.col = tables.y.text.col,
                            risk.table.height = tables.height, surv.plot.height = 0.75,
                            ncensor.plot.height = tables.height, cumevents.height = tables.height,
                            cumcensor.height = tables.height,
                            ncensor.plot = FALSE,
                            ncensor.plot.title = NULL,
                            cumevents = FALSE, cumevents.col = tables.col, cumevents.title = NULL,
                            cumevents.y.text = tables.y.text, cumevents.y.text.col = tables.y.text.col,
                            cumcensor = FALSE, cumcensor.col = tables.col, cumcensor.title = NULL,
                            cumcensor.y.text = tables.y.text, cumcensor.y.text.col = tables.y.text.col,
                            surv.median.line = c("none", "hv", "h", "v"),
                            ggtheme = theme_survminer(),
                            tables.theme = ggtheme,
                            ...
){

  if(!inherits(fit, "survfit"))
    stop("Can't handle an object of class ", class(fit))
  surv.median.line <- match.arg(surv.median.line)
  stopifnot(log.rank.weights %in% c("survdiff", "1", "n", "sqrtN", "S1", "S2","FH_p=1_q=1"))
  log.rank.weights <- match.arg(log.rank.weights)

  # cumevents / cumcensor accept the same values as risk.table: TRUE/FALSE, or a
  # character ("absolute", "percentage", "abs_pct") selecting how the cumulative
  # count is displayed (#499). Parse to a logical display flag + a type; logical
  # inputs keep type "absolute" (the raw count), so existing calls are unchanged.
  cumev.parsed <- .parse_risk_table_arg(cumevents)
  cumevents <- cumev.parsed$display
  cumevents.type <- cumev.parsed$type
  cumcens.parsed <- .parse_risk_table_arg(cumcensor)
  cumcensor <- cumcens.parsed$display
  cumcensor.type <- cumcens.parsed$type

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
  # Default table titles reflect the display type so a percentage table is not
  # labelled "number of ..." (#499). A user-supplied title is kept as-is.
  if(is.null(cumcensor.title))
    cumcensor.title <- switch(cumcensor.type,
      percentage = "Cumulative censoring (%)",
      abs_pct    = "Cumulative censoring: n (%)",
      "Cumulative number of censoring")
  if(is.null(cumevents.title))
    cumevents.title <- switch(cumevents.type,
      percentage = "Cumulative events (%)",
      abs_pct    = "Cumulative events: n (%)",
      "Cumulative number of events")

  # risk.table argument
  risk.table.pos <- match.arg(risk.table.pos)
  risktable <- .parse_risk_table_arg(risk.table)
  risk.table <- risktable$display
  risk.table.type <- risktable$type
  extra.params <- list(...)

  # Axes offset
  .expand <- ggplot2::waiver()
  if(!axes.offset)
    .expand <- c(0, 0)


  # Data
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # data used to compute survfit
  data <- .get_data(fit, data = data, complain = FALSE)
  # Data for survival plot
  d <- surv_summary(fit, data = data)
  if(!is.null(fit$start.time)) d <- subset(d, d$time >= fit$start.time )

  # Axis limits
   xmin <- ifelse(.is_cloglog(fun), min(c(1, d$time)), min(c(0, d$time), na.rm = TRUE))
   if(!is.null(fit$start.time)) xmin <- fit$start.time
   # Extend the upper x-limit to cover the largest event/censoring time, not
   # just the largest "nice" axis break. scales::extended_breaks() can return a
   # last break below max(d$time), which previously clipped events beyond it
   # from the default plot (#655). This is a no-op when the data already fit
   # within the breaks; the axis tick breaks are unchanged (computed separately
   # in ggsurvplot_df()), and a user-supplied xlim is unaffected (gated below).
   xmax <- max(c(max(d$time, na.rm = TRUE),
                 .get_default_breaks(d$time, .log = .is_cloglog(fun)) %>% max()),
               na.rm = TRUE)
   if(is.null(xlim)) xlim <- c(xmin, xmax)

  # Main survival curves
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  p <- ggsurvplot_df(d, fun = fun,
                     color = color, palette = palette, linetype = linetype,
                     break.x.by = break.x.by, break.time.by = break.time.by, break.y.by = break.y.by,
                     surv.scale = surv.scale, xscale = xscale,
                     conf.int = conf.int, conf.int.fill = conf.int.fill, conf.int.style = conf.int.style,
                     conf.int.alpha = conf.int.alpha,
                     censor = censor, censor.shape = censor.shape, censor.size = censor.size,
                     title = title,  xlab = xlab, ylab = ylab,
                     xlim = xlim, ylim = ylim, axes.offset = axes.offset,
                     legend = legend, legend.title = legend.title, legend.labs = legend.labs,
                     ggtheme = ggtheme, ...)


  # The main plot parameters, will be used to plot survival tables
  pms <- attr(p, "parameters")
  color <- surv.color <- pms$color

  # Add pvalue
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # Compute pvalue or parse it if provided by the user
  pval <- surv_pvalue(fit, method = log.rank.weights, data = data,
                      pval = pval, pval.coord = pval.coord,
                      pval.method.coord = pval.method.coord,
                      test.for.trend = test.for.trend)

  if(pval$pval.txt != ""){
    p <- p + ggplot2::annotate("text", x = pval$pval.x, y = pval$pval.y,
                               label = pval$pval.txt, size = pval.size, hjust = 0,
                               family = font.family, parse = pval.parse)
    if(pval.method)
      # The method name is package-generated literal text (e.g. "Log-rank",
      # or "Log-rank, tft" with test.for.trend); never plotmath-parse it, or
      # pval.parse = TRUE would crash / mis-render the hyphen. pval.parse only
      # affects the p-value text above.
      p <- p + ggplot2::annotate("text", x = pval$method.x, y = pval$method.y,
                                 label = pval$method, size = pval.method.size, hjust = 0,
                                 family = font.family, parse = FALSE)
  }


  # Drawing a horizontal line at 50% survival
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #if(surv.scale == "percent") fun <- "pct"
  if(surv.median.line %in% c("hv", "h", "v"))
    p <- .add_surv_median(p, fit, type = surv.median.line, fun = fun, data = data)

  res <- list(plot = p)

  # Extract strata colors used in survival curves
  # Will be used to color the y.text of risk table and cumevents table
  if(risk.table | cumevents | cumcensor | ncensor.plot){
    scurve_cols <- .extract_ggplot_colors (p, grp.levels = pms$legend.labs)
  }


  # The main plot parameters, will be used to plot survival tables
  pms <- attr(p, "parameters")
  surv.color <- pms$color

  pms$fit <- fit
  pms$data <- data
  pms$risk.table.type <- risk.table.type
  pms$risk.table.title <- risk.table.title
  pms$cumevents.title <- cumevents.title
  pms$cumcensor.title <- cumcensor.title
  pms$fontsize <- fontsize
  pms$ggtheme <- ggtheme
  pms$ylab <- pms$legend.title
  pms$tables.theme <- tables.theme
  pms$y.text <- tables.y.text
  pms$color <- tables.col
  pms$font.family <- font.family
  pms$axes.offset <- axes.offset


  # Add risk table
  if(risk.table){
    if(risk.table.pos == "in") risk.table.col = surv.color
    pms$color <- risk.table.col
    pms$title <- risk.table.title
    pms$y.text <- risk.table.y.text
    pms$y.text.col <- risk.table.y.text.col
    pms$fontsize <- risk.table.fontsize
    pms$survtable <- "risk.table"
    # Left-align the t=0 numbers-at-risk to the curve origin for the standard
    # (out) table; keep the inset ("in") table's historical geometry, whose
    # placement heuristic in .put_risktable_in_survplot depends on it (#645).
    pms$origin.align <- (risk.table.pos != "in")
    # color risk.table ticks by strata
    if(risk.table.y.text.col) pms$y.text.col <- scurve_cols
    res$table <- risktable <- do.call(ggsurvtable, pms)
  }

  # Add the cumulative number of events
  if(cumevents){
    pms$color <- cumevents.col
    pms$title <- cumevents.title
    pms$y.text <- cumevents.y.text
    if(cumevents.y.text.col) pms$y.text.col <- scurve_cols
    pms$fontsize <- fontsize
    pms$survtable <- "cumevents"
    pms$risk.table.type <- cumevents.type   # absolute (default) / percentage / abs_pct (#499)
    pms$origin.align <- TRUE   # always laid out "out"; align t=0 to the origin
    res$cumevents <- do.call(ggsurvtable, pms)
  }

  # Add ncensor.plot or cumcensor plot
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if(ncensor.plot){
    # A single-group fit (e.g. ~ 1) has no 'strata' column, so the default
    # color = "strata" would be passed to geom_bar as a literal colour and
    # error ("Unknown colour name: strata"). For that sentinel only, use the
    # survival curve's own colour (so the bars match the curve, as grouped
    # bars do), falling back to black if it can't be resolved. A real strata
    # column (grouped) or a user-supplied colour is left untouched.
    ncensor.color <- surv.color
    if(identical(surv.color, "strata") && !("strata" %in% colnames(d))){
      ncensor.color <- unname(scurve_cols)[1]
      if(length(ncensor.color) == 0 || is.na(ncensor.color))
        ncensor.color <- "black"
    }
    ncensor_plot <- ggplot(d, ggplot2::aes(x = !!sym("time"), y = !!sym("n.censor"))) +
      ggpubr::geom_exec(geom_bar, d, color = ncensor.color, fill = ncensor.color,
                        stat = "identity", position = "dodge")+
      coord_cartesian(xlim = xlim)+
      scale_y_continuous(breaks = .ncensor_y_breaks(d$n.censor)) +
      ggtheme

    ncensor_plot <- ggpubr::ggpar(ncensor_plot, palette = pms$palette)
    ncensor_plot <- ncensor_plot + ggplot2::labs(color = pms$legend.title, fill = pms$legend.title,
                                                 x = xlab, y = "n.censor", title = ncensor.plot.title)

    # For backward compatibility
    ncensor_plot <-  .set_general_gpar(ncensor_plot,  ...) # general graphical parameters
    ncensor_plot <- .set_ncensorplot_gpar(ncensor_plot,  ...) # specific graphical params
    ncensor_plot <- ncensor_plot + tables.theme

    if(!pms$xlog) ncensor_plot <- ncensor_plot + scale_x_continuous(breaks = pms$time.breaks,
                                                                    labels = pms$xticklabels, expand = .expand)
    else ncensor_plot <- ncensor_plot + ggplot2::scale_x_continuous(breaks = pms$time.breaks, trans = "log10", labels = pms$xticklabels)

  }
  else if(cumcensor){
    pms$color <- cumcensor.col
    pms$title <- cumcensor.title
    if(cumcensor.y.text.col) pms$y.text.col <- scurve_cols
    #pms$y.text.col <- cumcensor.y.text.col
    pms$fontsize <- fontsize
    pms$survtable <- "cumcensor"
    pms$risk.table.type <- cumcensor.type   # absolute (default) / percentage / abs_pct (#499)
    pms$origin.align <- TRUE   # always laid out "out"; align t=0 to the origin
    ncensor_plot  <- do.call(ggsurvtable, pms)
  }
  if(ncensor.plot | cumcensor)
    res$ncensor.plot <- ncensor_plot


  # Defining attributs for ggsurvplot
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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
  res$data.survtable <- .get_timepoints_survsummary(fit, data, pms$time.breaks)

  class(res) <- c("ggsurvplot", "ggsurv", "list")
  attr(res, "heights") <- heights
  attr(res, "y.text") <- y.text
  attr(res, "y.text.col") <- y.text.col
  attr(res, "legend.position") <- legend
  attr(res, "legend.labs") <- legend.labs
  attr(res, "cumcensor") <- cumcensor
  attr(res, "risk.table.pos") <- risk.table.pos
  attr(res, "axes.offset") <- axes.offset
  res
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
  axes.offset <- attr(x, "axes.offset")


  risk.table.pos <- attr(x, "risk.table.pos")
  if(risk.table.pos == "in") x <- .put_risktable_in_survplot(x, axes.offset = axes.offset)

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
  if(!is.null(x$table) | !is.null(x$ncensor.plot) | !is.null(x$cumevents)){
    g <- ggplot_build(x$plot)
    cols <- unlist(unique(g$data[[1]]["colour"]))
    if(length(cols)==1) cols <- rep(cols, length(legend.labs))
    names(cols) <- legend.labs # Give every color an appropriate name
  }

  if(nplot > 1 & legend.position %in% c("left", "right", "bottom")) x$plot <- .hide_legend(x$plot)

  if(!is.null(x$table)){
    x$table <- .hide_legend(x$table)
    if(!y.text$table) x$table <- .set_large_dash_as_ytext(x$table)
    # Make sure that risk.table.y.text.col will be the same as the plot legend colors
   # if(y.text.col$table)
   #    x$table <- x$table + ggplot2::theme(axis.text.y = ggplot2::element_text(colour = rev(cols)))
  }

  if(!is.null(x$cumevents)){
    x$cumevents <- .hide_legend(x$cumevents)
    if(!y.text$cumevents) x$cumevents <- .set_large_dash_as_ytext(x$cumevents)
    # Make sure that y.text.col will be the same as the plot legend colors
    #if(y.text.col$cumevents)
    #  x$cumevents <- x$cumevents + ggplot2::theme(axis.text.y = ggplot2::element_text(colour = rev(cols)))
  }


  if(!is.null(x$ncensor.plot)){
    x$ncensor.plot <- x$ncensor.plot + theme (legend.position = "none")
    if(cumcensor){
      if(!y.text$cumcensor) x$ncensor.plot <- .set_large_dash_as_ytext(x$ncensor.plot)
      #if(y.text.col$cumcensor)
       # x$ncensor.plot <- x$ncensor.plot + theme(axis.text.y = ggplot2::element_text(colour = rev(cols)))
    }
  }

  if(is.null(x$table) & is.null(x$ncensor.plot) & is.null(x$cumevents)) return(ggplotGrob(x$plot))

  heights <- unlist(heights)[names(x)] # get the height of each component in x
  plots <- x
  grobs <- widths <- list()
  for (i in 1:length(plots)) {
    if(ggplot2::is_ggplot(plots[[i]])){
      grobs[[i]] <- ggplotGrob(plots[[i]])
      # Find panel columns dynamically instead of hardcoding [2:5]
      panel_cols <- which(grepl("panel", grobs[[i]]$layout$name))
      if(length(panel_cols) == 0) {
        # Fallback to traditional approach if no panel found
        panel_range <- 2:min(5, ncol(grobs[[i]]))
      } else {
        # Use actual panel column range
        panel_range <- min(panel_cols):max(panel_cols)
      }
      widths[[i]] <- grobs[[i]]$widths[panel_range]
    }
  }
  maxwidth <- do.call(grid::unit.pmax, widths)
  for (i in 1:length(grobs)) {
    if(!is.null(grobs[[i]])){
      # Apply same panel range logic for setting widths
      panel_cols <- which(grepl("panel", grobs[[i]]$layout$name))
      if(length(panel_cols) == 0) {
        panel_range <- 2:min(5, ncol(grobs[[i]]))
      } else {
        panel_range <- min(panel_cols):max(panel_cols)
      }
      grobs[[i]]$widths[panel_range] <- as.list(maxwidth)
    }
  }


  ggsurv <- gridExtra::arrangeGrob(grobs = grobs, nrow = nplot, heights = unlist(heights))

  # Set legend
  if(nplot > 1 & legend.position %in% c("left", "right", "bottom") & !is.null(legend.grob)){
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
.add_surv_median <-function(p, fit, type = "hv", fun = NULL, data = NULL,
                            color = "black", linetype = "dashed", size = 0.5){
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
                     strata = .clean_strata(rownames(.table), fit))
    if(!is.null(fit$strata)){
      variables <- .get_variables(df$strata, fit, data)
      for(variable in variables) df[[variable]] <- .get_variable_value(variable, df$strata, fit, data)
    }
    df <- stats::na.omit(df)

    if(nrow(df)>0){
      if(type %in% c("hv", "h")){
        # Create single-row dataframe for horizontal line to avoid aesthetic length warning
        h_line_data <- data.frame(x = 0, y = max(df$y2), xend = max(df$x1), yend = max(df$y2))
        p <- p +
          geom_segment(aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
                       data = h_line_data, linetype = linetype, linewidth = size, color = color) # horizontal segment
      }

      if(type %in% c("hv", "v"))
        p <- p + geom_segment(aes(x = .data$x1, y = .data$y1, xend = .data$x2, yend = .data$y2), data = df,
                              linetype = linetype, linewidth = size, color = color) # vertical segments
    }
    else warning("Median survival not reached.")
  }

  p
}


# Draw median survival lines from a precomputed vector of median survival
# times. Used by ggsurvplot_combine(), which works from a list of fits (combined
# into one data frame) rather than a single survfit object, so it cannot use
# .add_surv_median() above. NA medians (median not reached) are dropped.
.add_median_lines <- function(p, medians, type = "hv", med_y = 0.5,
                              color = "black", linetype = "dashed", size = 0.5){
  x <- y <- xend <- yend <- x1 <- x2 <- y1 <- y2 <- NULL
  medians <- medians[!is.na(medians)]
  if(length(medians) == 0){
    warning("Median survival not reached.")
    return(p)
  }
  if(type %in% c("hv", "h")){
    h_line_data <- data.frame(x = 0, y = med_y, xend = max(medians), yend = med_y)
    p <- p +
      geom_segment(aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
                   data = h_line_data, linetype = linetype, linewidth = size, color = color)
  }
  if(type %in% c("hv", "v")){
    v_line_data <- data.frame(x1 = medians, x2 = medians,
                              y1 = rep(0, length(medians)),
                              y2 = rep(med_y, length(medians)))
    p <- p +
      geom_segment(aes(x = .data$x1, y = .data$y1, xend = .data$x2, yend = .data$y2),
                   data = v_line_data, linetype = linetype, linewidth = size, color = color)
  }
  p
}



# Put risk table inside main plot
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.put_risktable_in_survplot <- function(ggsurv, axes.offset = TRUE){

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
  ymin <- -0.05
  xmin <- -max(.time)/20

  if(!axes.offset){
    ymin <- -0.02
    xmin <- -max(.time)/50
  }
  risktable_grob = ggplotGrob(risktable)
  survplot <- survplot + annotation_custom(grob = risktable_grob, xmin = xmin,
                                           ymin = ymin, ymax = ymax)
  ggsurv$plot <- survplot
  ggsurv$table <- NULL
  ggsurv
}

# Check if fun is cloglog
.is_cloglog <- function(fun){
  res <- FALSE
  if(is.character(fun)){
    res <- fun == "cloglog"
  }
  res
}


