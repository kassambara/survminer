#' @include utilities.R
#' @importFrom methods is
#' @importFrom stats pchisq
  NULL
#'Drawing survival curves using ggplot2
#'@description Drawing survival curves using ggplot2
#'@param fit an object of class survfit.
#'@param fun an arbitrary function defining a transformation of the
#'  survival curve. For example use function(y){y*100}. Often used transformations
#'  can be specified with a character argument instead: "event" plots cumulative
#'  events (f(y) = 1-y), "cumhaz" plots the cumulative hazard function (f(y) =
#'  -log(y)), and "cloglog" creates a complimentary log-log survival plot (f(y)
#'  = log(-log(y)) along with log scale for the x-axis).
#'@param surv.scale scale transformation of survival curves. Allowed values are
#'  "default" or "percent".
#'@param color color to be used for the survival curves. This argument is
#'  ignored when the number of strata (groups > 1). In this case, use the
#'  argument palette.
#'@param palette the color palette to be used. Allowed values include "grey" for
#'  grey color palettes; brewer palettes e.g. "RdBu", "Blues", ...; or custom
#'  color palette e.g. c("blue", "red").
#'@param break.time.by numeric value controlling time axis breaks. Default value
#'  is NULL.
#'@param conf.int logical value. If TRUE, plots confidence interval.
#'@param conf.int.fill fill color to be used for confidence interval.
#'@param censor logical value. If TRUE, censors will be drawn.
#'@param pval logical value. If TRUE, the p-value is added on the plot.
#'@param pval.size numeric value specifying the p-value text size. Default is 5.
#'@param pval.coord numeric vector, of length 2, specifying the x and y
#'  coordinates of the p-value. Default values are NULL.
#'@param main,xlab,ylab main title and axis labels
#'@param xlim,ylim x and y axis limits e.g. xlim = c(0, 1000), ylim = c(0, 1).
#'@param legend character specifying legend position. Allowed values are one of
#'  c("top", "bottom", "left", "right", "none"). Default is "top" side
#'  position. to remove the legend use legend = "none". Legend position can be
#'  also specified using a numeric vector c(x, y); see details section.
#'@param legend.title legend title.
#'@param legend.labs character vector specifying legend labels.
#'@param risk.table logical value specifying whether to show risk table. Default
#'  is FALSE.
#'@param risk.table.col color to be used for risk table. Default value is
#'  "black". If you want to color by strata (i.e. groups), use risk.table.col =
#'  "strata".
#'@param risk.table.adj numeric value, used to adjust the location of the risk
#'  table. Negative value will shift the table to the left and positive value
#'  will shift the table to the right side. Ignored when risk.table = FALSE.
#'@param risk.table.height the height of the risk table on the grid. Increase
#'  the value when you have many strata. Default is 0.25. Ignored when
#'  risk.table = FALSE.
#'@param surv.plot.adj numeric value, used to adjust survival plot (like
#'  risk.table.adj). Ignored when risk.table = FALSE.
#'@param surv.plot.height the height of the survival plot on the grid. Default
#'  is 2. Ignored when risk.table = FALSE.
#'@param ggtheme function, ggplot2 theme name. Default value is theme_classic().
#'  Allowed values include ggplot2 official themes: theme_gray(), theme_bw(),
#'  theme_minimal(), theme_classic(), theme_void(), ....
#'@param ... other arguments to be passed to ggplot2 geom_*() functions such as
#'  linetype, size, ...
#'@details \strong{legend position}: The argument \strong{legend} can be also a
#'  numeric vector c(x,y). In this case it is possible to position the legend
#'  inside the plotting area. x and y are the coordinates of the legend box.
#'  Their values should be between 0 and 1. c(0,0) corresponds to the "bottom
#'  left" and c(1,1) corresponds to the "top right" position. For instance use
#'  legend = c(0.8, 0.2).
#'@return return a ggplot2 (when risk.table = FALSE).
#'
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
#'ggsurvplot(fit, linetype = "strata",
#'           conf.int = TRUE, pval = TRUE,
#'           palette = "Dark2")
#'
#'
#'# Add risk table
#'#++++++++++++++++++++++++++++++++++++
#'
#'# Add Risk table
#'ggsurvplot(fit, pval = TRUE, conf.int = TRUE,
#'           risk.table = TRUE)
#'
#'# Change color, linetype by strata, risk.table color by strata
#'ggsurvplot(fit,
#'           pval = TRUE, conf.int = TRUE,
#'           risk.table = TRUE, # Add risk table
#'           risk.table.col = "strata", # Change risk table color by groups
#'           lienetype = "strata", # Change line type by groups
#'           ggtheme = theme_bw(), # Change ggplot2 theme
#'           palette = c("#E7B800", "#2E9FDF"))
#'
#'# Survival curve transformation
#'#++++++++++++++++++++++++++++++++++++
#'# Plot cumulative events
#'ggsurvplot(fit, conf.int = TRUE,
#'           palette = c("#FF9E29", "#86AA00"),
#'           risk.table = TRUE, risk.table.col = "strata",
#'           fun = "event")
#'
#'
#'# Arbitrary function
#'ggsurvplot(fit, conf.int = TRUE,
#'           palette = c("#FF9E29", "#86AA00"),
#'           risk.table = TRUE, risk.table.col = "strata",
#'           pval = TRUE,
#'           fun = function(y) y*100)
#'
#'
#'#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#'# Example 3: Survival curve with multiple group
#'#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#'
#'# Fit (complexe) survival curves
#'#++++++++++++++++++++++++++++++++++++
#'
#'require("survival")
#'fit2 <- survfit( Surv(time, status) ~ rx + adhere,
#'                 data = colon )
#'
#'# Visualize
#'#++++++++++++++++++++++++++++++++++++
#'
#'# Visualize: add p-value, chang y limits
#'# change color using brewer palette
#'ggsurvplot(fit2, pval = TRUE,
#'           break.time.by = 400,
#'           risk.table = TRUE)
#'
#'# Adjust risk table and survival plot locations
#'# ++++++++++++++++++++++++++++++++++++
#'# Adjust risk table location, shift to the left
#'
#'ggsurvplot(fit2, pval = TRUE,
#'           break.time.by = 400,
#'           risk.table = TRUE,
#'           risk.table.col = "strata",
#'           risk.table.adj = -2, # risk table location adj
#'           palette = "Dark2")
#'
#'# Adjust survival plot location, shift to the right
#'# Change Risk table height
#'ggsurvplot(fit2, pval = TRUE,
#'           break.time.by = 400,
#'           risk.table = TRUE,
#'           risk.table.col = "strata",
#'           risk.table.height = 0.5, # Useful when you have multiple groups
#'           surv.plot.adj = 4.9, # surv plot location adj
#'           palette = "Dark2")
#'
#'
#'# Change legend labels
#'# ++++++++++++++++++++++++++++++++++++
#' \dontrun{
#'ggsurvplot(fit2, pval = TRUE,
#'           break.time.by = 400,
#'           risk.table = TRUE,
#'           risk.table.col = "strata",
#'           ggtheme = theme_bw(),
#'           legend.labs = c("A", "B", "C", "D", "E", "F"))
#' }
#'
#'@export
ggsurvplot <- function(fit, fun = NULL,
                       color = NULL, palette = NULL, break.time.by = NULL,
                       surv.scale = c("default", "percent"),
                       conf.int = FALSE, conf.int.fill = "gray",
                       censor = TRUE,
                       pval = FALSE, pval.size = 5, pval.coord = c(NULL, NULL),
                       main = NULL, xlab = "Time", ylab = "Survival probability",
                       xlim = NULL, ylim = NULL,
                       legend = c("top", "bottom", "left", "right", "none"),
                       legend.title = "strata", legend.labs = NULL,
                       risk.table = FALSE, risk.table.col = "black", risk.table.adj = NULL,
                       risk.table.height = 0.25,
                       surv.plot.adj = NULL, surv.plot.height = 2,
                       ggtheme = ggplot2::theme_classic(),
                       ...
                       ){

  if(!methods::is(fit, "survfit"))
    stop("Can't handle an object of class ", class(fit))
  size <- ifelse(is.null(list(...)$size), 1, list(...)$size)
  if(is.null(xlim)) xlim <- c(0, max(fit$time))
  if(is.null(ylim) & is.null(fun)) ylim <- c(0, 1)
  if(!is(legend, "numeric")) legend <- match.arg(legend)

  n.strata <- ifelse(is.null(fit$strata) == TRUE, 1, length(fit$strata))

  break.time.by <- ifelse(is.null(break.time.by),
                          round(max(fit$time)/10), break.time.by)
  times <- seq(0, max(fit$time), by = break.time.by)


  # data for survival plot
  d <- data.frame(time = fit$time,
                  n.risk = fit$n.risk,
                  n.event = fit$n.event,
                  n.censor = fit$n.censor,
                  surv = fit$surv,
                  std.err = fit$std.err,
                  upper = fit$upper,
                  lower = fit$lower
                  )

  if ('strata' %in% names(fit)){
    d <- cbind.data.frame(d, strata =  rep(names(fit$strata), fit$strata))
    if(missing(color)) color <- "strata"
    if(is.null(legend.labs)) legend.labs <-  sort(names(fit$strata))
    else{
      if(length(levels(d$strata))!=length(legend.labs))
        stop("The length of legend.labs must be ", length(levels(d$strata)) )
       levels(d$strata) <- legend.labs
    }
  }else{
    d <- cbind.data.frame(d, strata =  factor(rep('All', nrow(d))))
    if(missing(conf.int)) conf.int = TRUE
    if(missing(color)) color <- "black"
    if(is.null(legend.labs)) legend.labs <- "All"
  }

  # connect to the origin for plotting
    base <- d[1, , drop = FALSE]
    base[intersect(c('time', 'n.censor', 'std.err'), colnames(base))] <- 0
    base[c('surv', 'upper', 'lower')] <- 1.0
    if ('strata' %in% names(fit)) {
      strata <- levels(d$strata)
      base <- as.data.frame(sapply(base, rep.int, times = length(strata)))
      base$strata <- strata
      base$strata <- as.factor(base$strata)
    }
    d <- rbind(base, d)

  #  transformation of the survival curve
  d <- .apply_surv_func(d, fun = fun)


  # Scale transformation
  surv.scale <- match.arg(surv.scale)
  scale_labels <-  ggplot2::waiver()
  if (surv.scale == "percent")
    scale_labels <- scales::percent

  # Drawing survival curves
  surv.color <- ifelse(n.strata > 1, "strata", color)
  p <- ggplot2::ggplot(d, ggplot2::aes_string("time", "surv")) +
      .geom_exec(ggplot2::geom_step, data = d, size = size, color = surv.color, ...) +
       ggplot2::scale_y_continuous(labels = scale_labels, limits = ylim) +
       ggplot2::scale_x_continuous(breaks = times, limits = xlim) +
       .ggcolor(palette) +
       .ggfill(palette) + ggtheme

  # Add confidence interval
  if(conf.int){
    if(missing(conf.int.fill)) conf.int.fill <- surv.color
    p <- p + .geom_exec(ggplot2::geom_ribbon, data = d,
                        ymin = "lower", ymax = "upper",
                        fill = conf.int.fill, alpha = 0.3, na.rm = TRUE)
  }
  # Add cencored
  if (censor) {
    p <- p + .geom_exec(ggplot2::geom_point, data = d[d$n.censor > 0, , drop = FALSE],
                          colour = surv.color, size = size*4.5, shape = "+")
  }
   # print(d[d$n.censor > 0, , drop = FALSE])

  # Add pvalue
  if(pval){
    pval <- .get_pvalue(fit)
    pval.x <- ifelse(is.null(pval.coord[1]), 0.1*max(fit$time), pval.coord[1])
    pval.y <- ifelse(is.null(pval.coord[2]), 0.2, pval.coord[2])
    p <- p + ggplot2::annotate("text", x = pval.x, y = pval.y,
                               label = paste0("p = ", signif(pval, 3)),
                               size = pval.size)
  }

  # Axis limits
  p <- p + ggplot2::expand_limits(x = 0, y = 0)
  # Label
  p <- p + ggplot2::labs(x = xlab, y = ylab, title = main,
                         color = legend.title, fill = legend.title,
                         linetype = legend.title
                         )
  p <- p + ggplot2::theme(legend.position = legend)

  # Add risk table
   if(risk.table){
     blankp <- .blank_plot(d, "time", "strata")
     risktable <- .risk_table_plot(fit, times = times,
                                   legend.labs = legend.labs,
                                   xlim = xlim, ylim = ylim, risk.table.adj = risk.table.adj,
                                   risk.table.col = risk.table.col, palette = palette)
     m <- max(nchar(legend.labs))
    if(is.null(surv.plot.adj)) surv.plot.adj <- ifelse(m < 10, 1.5, 2.5)
    p <- p + ggplot2::theme(legend.position = "top") +
    ggplot2::theme(plot.margin = grid::unit(c(0, 1, .5, surv.plot.adj),"lines"))
    gridExtra::grid.arrange(p, blankp, risktable, clip = FALSE, nrow = 3,
                 ncol = 1, heights = grid::unit(c(surv.plot.height, .1, risk.table.height),
                                                c("null", "null", "null")))

   #  p <- gridExtra::arrangeGrob(p, blankp, risktable, clip = FALSE, nrow = 3,
   #                  ncol = 1, heights = unit(c(2, .1, .25), c("null", "null", "null")))
   #  invisible(p)
   }
  else return(p)
}



# Function defining a transformation of the survival curve
# ++++++++++++++++++++++++++++++++++++++++++++++
# see ?survival::plot.survfit
# d: data frame containing the column surv, upper and lower
# fun the function
.apply_surv_func <- function(d, fun = NULL){

  if (!is.null(fun)) {
    if (is.character(fun)) {
      fun <- switch(fun, log = function(y) y,
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




# get survdiff pvalue
.get_pvalue <- function(fit){
  # One group
  if(length(levels(summary(fit)$strata)) == 0)  return(NULL)
    sdiff <- survival::survdiff(eval(fit$call$formula), data = eval(fit$call$data))
    pvalue <- stats::pchisq(sdiff$chisq, length(sdiff$n) - 1, lower.tail = FALSE)
    return (pvalue)
}

# Draw risk table
# risk.table.adj adjustement of risk table location. Used to shift the table on the right or
# on the left
.risk_table_plot <- function(fit, times, legend.labs = NULL,
                        xlim = c(0, max(fit$time)), ylim = c(0,1),
                        risk.table.adj = NULL, risk.table.col = "black",
                        palette = NULL){

  ntimes <- length(summary(fit, times = times, extend = TRUE)$time)

  if (!('strata' %in% names(fit))){
    strata <- factor(rep("All", ntimes))
    if(is.null(legend.labs)) legend.labs <- "All"
  } else{
    strata<- factor(summary(fit, times = times, extend = TRUE)$strata)
    if(is.null(legend.labs)) legend.labs <-  sort(names(fit$strata))
    else {
      if(length(levels(strata))!=length(legend.labs))
        stop("The length of legend.labs must be ", length(levels(strata)) )
       levels(strata) <- legend.labs
    }
  }

    # if(is.null(ystrataname)) ystrataname <- "Strata"

    risk.data <- data.frame(
      strata = factor(strata, levels = levels(strata)),
      time = summary(fit,times = times, extend = TRUE)$time,
      n.risk = summary(fit,times = times,extend = TRUE)$n.risk
    )

    .blank <- ggplot2::element_blank()
    dtp <- ggplot2::ggplot(risk.data,
           ggplot2::aes_string(x = 'time', y = 'strata', label = "n.risk")) +
          .geom_exec(ggplot2::geom_text, data = risk.data, size = 3.5, color = risk.table.col) +
           ggplot2::theme_bw() +
           ggplot2::scale_y_discrete(breaks = levels(risk.data$strata),
                       labels = legend.labs, limits = rev(legend.labs)) +
          ggplot2::scale_x_continuous("Numbers at risk", limits = xlim) +
          .ggcolor(palette) +
          ggplot2:: theme(axis.title.x = ggplot2::element_text(size = 10, vjust = 1),
            panel.grid.major = .blank, panel.grid.minor = .blank,
            panel.border = .blank, axis.text.x = .blank,
            axis.ticks = .blank, axis.text.y = ggplot2::element_text(face = "bold", hjust = 1 ))+
          ggplot2::theme(legend.position = "none") +
          ggplot2::labs(x = NULL, y = NULL)

   # Adjust position for table at risk
    m <- max(nchar(legend.labs))
    if(is.null(risk.table.adj)) {
      risk.table.adj <- ifelse(m < 10, 2.5, 3.5) - 0.15 * m
    }
    dtp <- dtp +
      ggplot2::theme(plot.margin = grid::unit(c(-1.5, 1, 0.1, risk.table.adj), "lines"))
    return(dtp)
}



# Draw a blank plot
.blank_plot <- function(d, x, y){
  .blank <- ggplot2::element_blank()
  blank_plot <- ggplot2::ggplot(d, ggplot2::aes_string(x, y)) +
    ggplot2::geom_blank() +  ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = .blank ,axis.text.y = .blank ,
          axis.title.x = .blank , axis.title.y = .blank ,
          axis.ticks = .blank ,
          panel.grid.major = .blank, panel.border = .blank)
  blank_plot
}






