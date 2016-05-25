#' @include utilities.R theme_classic2.R
#' @importFrom methods is
#' @importFrom stats pchisq
  NULL
#'Drawing survival curves using ggplot2
#'@description Drawing survival curves using ggplot2
#'@param fit an object of class survfit.
#'@param fun an arbitrary function defining a transformation of the survival
#'  curve. For example use function(y){y*100}. Often used transformations can be
#'  specified with a character argument instead: "event" plots cumulative events
#'  (f(y) = 1-y), "cumhaz" plots the cumulative hazard function (f(y) =
#'  -log(y)), and "cloglog" creates a complimentary log-log survival plot (f(y)
#'  = log(-log(y)) along with log scale for the x-axis).
#'@param surv.scale scale transformation of survival curves. Allowed values are
#'  "default" or "percent".
#'@param color color to be used for the survival curves. This argument is
#'  ignored when the number of strata (groups > 1). In this case, use the
#'  argument palette.
#'@param palette the color palette to be used. Allowed values include "hue" for
#'  the default hue color scale; "grey" for grey color palettes; brewer palettes
#'  e.g. "RdBu", "Blues", ...; or custom color palette e.g. c("blue", "red").
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
#'@param risk.table logical value specifying whether to show risk table. Default
#'  is FALSE.
#'@param risk.table.title Title to be used for risk table.
#'@param risk.table.col color to be used for risk table. Default value is
#'  "black". If you want to color by strata (i.e. groups), use risk.table.col =
#'  "strata".
#'@param risk.table.fontsize font size to be used for the risk table.
#'@param risk.table.y.text logical. Default is TRUE. If FALSE, risk table y axis tick labels will be hidden.
#'@param risk.table.y.text.col logical. Default value is FALSE. If TRUE, risk
#'  table tick labels will be colored by strata.
#'@param risk.table.height the height of the risk table on the grid. Increase
#'  the value when you have many strata. Default is 0.25. Ignored when
#'  risk.table = FALSE.
#'@param surv.plot.height the height of the survival plot on the grid. Default
#'  is 0.75. Ignored when risk.table = FALSE.
#'@param ggtheme function, ggplot2 theme name. Default value is survminer::theme_classic2().
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
#'@return return an object of class ggsurvplot which is list containing two
#'  ggplot objects, including: \itemize{ \item plot: the survival plot \item
#'  table: the number at risk table per time }
#'
#' @author
#' Alboukadel Kassambara, \email{alboukadel.kassambara@@gmail.com}
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
#'           lienetype = "strata", # Change line type by groups
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
#'@describeIn ggsurvplot Draws survival curves using ggplot2.
#'@export
ggsurvplot <- function(fit, fun = NULL,
                       color = NULL, palette = "hue", break.time.by = NULL,
                       surv.scale = c("default", "percent"),
                       conf.int = FALSE, conf.int.fill = "gray",
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
                       risk.table = FALSE, risk.table.title = "Number at risk by time",
                       risk.table.col = "black", risk.table.fontsize = 4.5,
                       risk.table.y.text = TRUE,
                       risk.table.y.text.col = TRUE,
                       risk.table.height = 0.25, surv.plot.height = 0.75,
                       ggtheme = theme_classic2(),
                       ...
                       ){

  if(!methods::is(fit, "survfit"))
    stop("Can't handle an object of class ", class(fit))
  size <- ifelse(is.null(list(...)$size), 1, list(...)$size)
  if(is.null(xlim)) xlim <- c(0, max(fit$time))
  if(is.null(ylim) & is.null(fun)) ylim <- c(0, 1)
  if(!is(legend, "numeric")) legend <- match.arg(legend)

  .check_legend_labs(fit, legend.labs)

  # Number of strata and strata names
  n.strata <- ifelse(is.null(fit$strata) == TRUE, 1, length(fit$strata))

  .strata <- NULL
  # Multiple groups
  if (!is.null(fit$strata)){
    .strata <- rep(names(fit$strata), fit$strata)
    strata_names <- names(fit$strata)
    if(is.null(legend.labs)) legend.labs <- names(fit$strata)
    if(missing(color)) color <- "strata"
  }

  # One group
  else{

    if (is.null(legend.labs)) {
      .strata <- as.factor(rep("All", length(fit$time)))
      legend.labs <- strata_names <- "All"
    }

    else {
      .strata <- as.factor(rep(legend.labs, length(fit$time)))
      strata_names <- legend.labs
    }

    if(missing(conf.int)) conf.int = TRUE
    if(missing(color)) color <- "black"
  }


  # data for survival plot
  d <- data.frame(time = fit$time,
                  n.risk = fit$n.risk,
                  n.event = fit$n.event,
                  n.censor = fit$n.censor,
                  surv = fit$surv,
                  std.err = fit$std.err,
                  upper = fit$upper,
                  lower = fit$lower,
                  strata = as.factor(.strata)
                  )


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
  if (surv.scale == "percent") scale_labels <- scales::percent


  # Drawing survival curves
  d$strata <- factor(d$strata, levels = strata_names, labels = legend.labs)
  d <- d[order(d$strata), , drop = FALSE]
  surv.color <- ifelse(n.strata > 1, "strata", color)
  p <- ggplot2::ggplot(d, ggplot2::aes_string("time", "surv")) +
      .geom_exec(ggplot2::geom_step, data = d, size = size, color = surv.color, ...) +
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


  if(is.null(break.time.by))
    times <- ggplot_build(p)$panel$ranges[[1]]$x.major_source
  else times <- seq(0, max(fit$time), by = break.time.by)

  p <- p + ggplot2::scale_x_continuous(breaks = times)


  # Add confidence interval
  if(conf.int){
    if(missing(conf.int.fill)) conf.int.fill <- surv.color
    p <- p + .geom_exec(ggplot2::geom_ribbon, data = d,
                        ymin = "lower", ymax = "upper",
                        fill = conf.int.fill, alpha = 0.3, na.rm = TRUE)
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

  # Axis limits
  p <- p + ggplot2::expand_limits(x = 0, y = 0)
  # Label
  p <- p + ggplot2::labs(x = xlab, y = ylab, title = main,
                         color = legend.title, fill = legend.title,
                         linetype = legend.title
                         )
  p <-.labs(p = p, font.main = font.main, font.x = font.x, font.y = font.y)
  p <- .set_ticks(p, font.tickslab = font.tickslab)
  p <- p + ggplot2::theme(legend.position = legend)
  p <- .set_legend_font(p, font.legend)


  # Add risk table
   if(risk.table){
     risktable <- .risk_table_plot(fit, times = times,
                                   xlim = xlim, legend.labs = legend.labs,
                                   risk.table.col = risk.table.col, palette = palette,
                                   ggtheme = ggtheme, risk.table.fontsize = risk.table.fontsize,
                                   risk.table.title = risk.table.title,
                                   risk.table.y.text = risk.table.y.text,
                                   font.tickslab = font.tickslab
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
       names(cols) <- legend.labs # Give every color an appropriate name
       risktable <- risktable + ggplot2::theme(axis.text.y = ggplot2::element_text(colour = rev(cols)))
     }

    res <- list(table = risktable, plot = p)
   }
  else res <- list(plot = p)
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
  if(is.null(x$table)) print(x$plot)
  else{
  surv.plot.height <- ifelse(is.null(surv.plot.height), attr(x, "surv.plot.height"), surv.plot.height)
  risk.table.height <- ifelse(is.null(risk.table.height), attr(x, "risk.table.height"), risk.table.height)
  surv.plot.height <- ifelse(is.null(surv.plot.height), 0.75, surv.plot.height)
  risk.table.height <- ifelse(is.null(risk.table.height), 0.25, risk.table.height)
  # Hide legende: don't use  theme(legend.position = "none") because awkward legend when position = "left"
  # x$table <- x$table + theme(legend.position = "none")
  x$table <- x$table + theme(legend.key.height = NULL, legend.key.width = NULL,
                              legend.key = element_rect(colour = NA, fill = NA),
                              legend.text = element_text(colour = NA),
                              legend.title = element_text(colour = NA)) +
    guides(color = FALSE)

  # Make sure that risk.table.y.text.col will be the same as the plot legend colors
  risk.table.y.text.col <- attr(x, 'risk.table.y.text.col')
  if(risk.table.y.text.col){
    g <- ggplot2::ggplot_build(x$plot)
    cols <- unlist(unique(g$data[[1]]["colour"]))
    legend.labs <- levels(g$plot$data$strata)
    names(cols) <- legend.labs # Give every color an appropriate name
    x$table <- x$table + ggplot2::theme(axis.text.y = ggplot2::element_text(colour = rev(cols)))
  }



  plots <- rev(x)
  grobs <- widths <- list()
  for (i in 1:length(plots)) {
    grobs[[i]] <- ggplotGrob(plots[[i]])
    widths[[i]] <- grobs[[i]]$widths[2:5]
  }
  maxwidth <- do.call(grid::unit.pmax, widths)
  for (i in 1:length(grobs)) {
    grobs[[i]]$widths[2:5] <- as.list(maxwidth)
  }
  do.call(gridExtra::grid.arrange, c(grobs, nrow = 2, heights = list(c(surv.plot.height, risk.table.height))))
  }
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
# on the left
.risk_table_plot <- function(fit, times, legend.labs = NULL,
                             xlim = c(0, max(fit$time)),
                             risk.table.col = "black",
                             palette = NULL, ggtheme = ggplot2::theme_classic(),
                             risk.table.fontsize = 5, risk.table.title = "Number at risk by time",
                             risk.table.y.text = TRUE,
                             font.tickslab = c(12, "plain", "black")
)
  {

  ntimes <- length(summary(fit, times = times, extend = TRUE)$time)

  if (is.null(fit$strata)) {
    .strata <- factor(rep("All", length(times)))
    strata_names <- "All"
  }
  else {
    .strata <- factor(summary(fit, times = times, extend = TRUE)$strata)
    strata_names <- names(fit$strata)
  }
  risk.data <- data.frame(
    strata = as.factor(.strata),
    time = summary(fit, times = times, extend = TRUE)$time,
    n.risk = round(summary(fit, times = times, extend = TRUE)$n.risk)
  )


  if (!is.null(legend.labs))
    risk.data$strata <- factor(risk.data$strata, labels = legend.labs)

  time <- strata <- label <- n.risk <- NULL

  # Adjust risk table labels in case of long strata
  risk.table.text.y <- rev(levels(risk.data$strata))
  n_strata <- length(risk.table.text.y)
#   max_char <- max(nchar(risk.table.text.y))
#   is_long_strata <- max_char > 5
#   if(is_long_strata) risk.table.text.y <- rep("-", n_strata)
   if(!risk.table.y.text) risk.table.text.y <- rep("-", n_strata)


  dtp <- ggplot2::ggplot(risk.data,
                         ggplot2::aes(x = time, y = rev(strata), label = n.risk, shape = rev(strata))) +
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

  if(!is.null(legend.labs)){

    if(!is.null(fit$strata)){
      if(length(fit$strata) != length(legend.labs))
        stop("The length of legend.labs must should be ", length(fit$strata) )
    }

    else{
      if(length(legend.labs) != 1)
        stop("The length of legend.labs should be 1")
    }

  }
}






