#' @import ggplot2
#' @import ggpubr


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Execute a geom_* function from ggplot2
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# geomfunc : gem_*() functions
# data data for mapping
# ... argument accepeted by the function
.geom_exec <- function (geomfunc, data = NULL, ...) {
  params <- list(...)

  mapping <-
    list() # option to pass to mapping aes() or aes_string()
  option <- list() # option to the geom_*()

  allowed_options <- c(
    # general
    "color", "colour", "linetype", "fill", "size", "shape",
    "alpha", "na.rm",
    "lwd", "pch", "cex",
    "stat", "position"
  )

  columns <- colnames(data)
  for (key in names(params)) {
    value <- params[[key]]
    if (is.null(value)) {

    }
    else if (value %in% columns) {
      mapping[[key]] <- value
    }
    else if (key %in% allowed_options) {
      option[[key]] <- value
    }
    # else stop("Don't know '", key, "'")
  }
  option[["data"]] <- data
  option[["mapping"]] <- do.call(ggplot2::aes_string, mapping)
  return(do.call(geomfunc, option))
}


# Change color manually
# possible value for palette: brewer palette, "grey" or a vector of colors
.ggcolor <- function(palette = NULL, ...) {
  brewerpal <- c(
    # sequential
    'Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
    'YlGn', 'YlGnBu YlOrBr', 'YlOrRd',
    #Divergent
    'BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral',
    # Qualitative
    'Accent', 'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3'
  )

  res <- NULL
  if (is.null(palette))
    palette <- ""
  if(length(palette) == 1){
    if (palette %in% brewerpal)
      ggplot2::scale_color_brewer( ..., palette = palette)
    else if (palette == "grey")
      ggplot2::scale_color_grey(..., start = 0.8, end = 0.2)
    else if (palette == "hue")
      ggplot2::scale_color_hue(...)
  }
  else if (palette[1] != "")
    ggplot2::scale_color_manual(..., values = palette)
}

# Change fill color manually
# possible value for palette: brewer palette, "grey" or a vector of colors
.ggfill <- function(palette = NULL, ...) {
  brewerpal <- c(
    # sequential
    'Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
    'YlGn', 'YlGnBu YlOrBr', 'YlOrRd',
    #Divergent
    'BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral',
    # Qualitative
    'Accent', 'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3'
  )

  res <- NULL
  if (is.null(palette))
    palette <- ""
  if(length(palette) == 1){
    if (palette %in% brewerpal)
      ggplot2::scale_fill_brewer( ..., palette = palette)
    else if (palette == "grey")
      ggplot2::scale_fill_grey(..., start = 0.8, end = 0.2)
    else if (palette == "hue")
      ggplot2::scale_fill_hue(...)
  }
  else if (palette[1] != "")
    ggplot2::scale_fill_manual(..., values = palette)
}


# Change title and labels
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.labs <- function(p, main = NULL, submain = NULL, caption = NULL, xlab = NULL, ylab = NULL,
                  font.submain = NULL, font.caption = NULL,
                  font.main = NULL, font.x = NULL, font.y = NULL)
{
  font.main <- .parse_font(font.main)
  font.x <- .parse_font(font.x)
  font.y <- .parse_font(font.y)
  font.submain <- .parse_font(font.submain)
  font.caption <- .parse_font(font.caption)

  if (!is.null(main)) {
    if (main != FALSE)
      p <- p + labs(title = main)
  }

  if (!is.null(submain)) {
    if (submain != FALSE)
      p <- p + labs(subtitle = submain)
  }

  if (!is.null(caption)) {
    if (caption != FALSE)
      p <- p + labs(caption = caption)
  }

  if (!is.null(xlab)) {
    if (xlab == FALSE)
      p <- p + theme(axis.title.x = element_blank())
    else
      p <- p + labs(x = xlab)
  }

  if (!is.null(ylab)) {
    if (ylab == FALSE)
      p <- p + theme(axis.title.y = element_blank())
    else
      p <- p + labs(y = ylab)
  }

  if (!is.null(font.main))
    p <-
      p + theme(
        plot.title = element_text(
          size = font.main$size,
          lineheight = 1.0, face = font.main$face, colour = font.main$color
        )
      )
  if (!is.null(font.submain))
    p <-
      p + theme(
        plot.subtitle = element_text(
          size = font.submain$size,
          lineheight = 1.0, face = font.submain$face, colour = font.submain$color
        )
  )
  if (!is.null(font.caption))
    p <-
      p + theme(
        plot.caption = element_text(
          size = font.caption$size,
          lineheight = 1.0, face = font.caption$face, colour = font.caption$color
        )
  )
  if (!is.null(font.x))
    p <-
      p + theme(axis.title.x = element_text(
        size = font.x$size,
        face = font.x$face, colour = font.x$color
      ))
  if (!is.null(font.y))
    p <-
      p + theme(axis.title.y = element_text(
        size = font.y$size,
        face = font.y$face, colour = font.y$color
      ))
  p
}

# ticks
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.set_ticks <-
  function(p, font.tickslab = NULL)
  {
    if(!is.null(font.tickslab)){
     font <- .parse_font(font.tickslab)
      xtickslab <-
        element_text(
          size = font$size, face = font$face,
          colour = font$color, angle = 0
        )
      ytickslab <-
        element_text(
          size = font$size, face = font$face,
          colour = font$color, angle = 0
        )
      p <- p+theme(axis.text.x = xtickslab, axis.text.y = ytickslab)
    }
    p
  }

# Legends
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.set_legend_font <- function(p, font.legend = NULL){

  font <- .parse_font(font.legend)
  if(!is.null(font)){
    p <- p + theme(
      legend.text = element_text(size = font$size,
                                            face = font$face, colour = font$color),
      legend.title = element_text(size = font$size,
                                 face = font$face, colour = font$color)
      )
  }
  p
}


# parse font
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.parse_font <- function(font){
  if(is.null(font)) res <- NULL
  else{
   # matching size and face
  size <- grep("^[0-9]+$", font, perl = TRUE)
  face <- grep("plain|bold|italic|bold.italic", font, perl = TRUE)
  if(length(size) == 0) size <- NULL else size <- as.numeric(font[size])
  if(length(face) == 0) face <- NULL else face <- font[face]
  color <- setdiff(font, c(size, face))
  if(length(color) == 0) color <- NULL
  res <- list(size=size, face = face, color = color)
  }
  res
}


# Connect observations by stairs.
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Connect observations by stairs.
#
# mapping the aesthetic mapping
# data a layer specific dataset
# stat the statistical transformation to use on the data for this layer
# position the position adjustment to use for overlapping points on this layer
# na.rm logical frag whether silently remove missing values
#  ... other arguments passed to methods
.geom_confint <- function (mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, ...) {
  ggplot2::layer(mapping = mapping,
                 data = data,
                 stat = stat,
                 geom = GeomConfint,
                 position = position,
                 params = list(na.rm = na.rm, ...))
}

GeomConfint <- ggplot2::ggproto('GeomConfint', ggplot2::GeomRibbon,
                                required_aes = c("x", "ymin", "ymax"),
                                draw_group = function(data, panel_scales, coord, na.rm = FALSE) {
                                  if (na.rm) data <- data[complete.cases(data[c("x", "ymin", "ymax")]), ]
                                  data <- rbind(data, data)
                                  data <- data[order(data$x), ]
                                  data$x <- c(data$x[2:nrow(data)], NA)
                                  data <- data[complete.cases(data["x"]), ]
                                  GeomRibbon$draw_group(data, panel_scales, coord, na.rm = FALSE)
                                }
#                                 draw_group = function(self, data, panel_scales, coord, na.rm = FALSE) {
#                                   if (na.rm) data <- data[stats::complete.cases(self$required_aes), ]
#                                   data <- data[order(data$group, data$x), ]
#                                   data <- self$stairstep_confint(data)
#                                   ggplot2:::GeomRibbon$draw_group(data, panel_scales, coord, na.rm = FALSE)
#                                 },
#                                 stairstep_confint = function (data) {
#                                   data <- as.data.frame(data)[order(data$x), ]
#                                   n <- nrow(data)
#                                   ys <- rep(1:n, each = 2)[-2 * n]
#                                   xs <- c(1, rep(2:n, each = 2))
#                                   data.frame(x = data$x[xs], ymin = data$ymin[ys], ymax = data$ymax[ys],
#                                              data[xs, setdiff(names(data), c("x", "ymin", "ymax"))])
#                                 }
)


# Remove NULL items in a vector or list
#
# x a vector or list
.compact <- function(x){Filter(Negate(is.null), x)}

# remove white space at the head and the tail of a string
.trim <- function(x){gsub("^\\s+|\\s+$", "", x)}

# Take a data frame and return a flatten value
.flat <- function(x){
  x <- as.data.frame(x)
  x <- tidyr::gather_(x,
                      key_col = "key", value_col = "value",
                      gather_cols = colnames(x))
  x$value
}

# extract dataset if not provided
.get_data <- function(fit, data = NULL, complain = TRUE) {
  if(is.null(data)){
    if (complain)
      warning ("The `data` argument is not provided. Data will be extracted from model fit.")
    data <- eval(fit$call$data)
  }
  data
}

# Compute default axis breaks as ggplot2
#-------------------------------------
# Return a vector of axis labels
.get_default_breaks <- function(x){
  scales::extended_breaks()(x)
}


# Get survival summary for a specified time points
#------------------------------------------------
# fit: survfit object
# data: data used for survfit
# times: a vector of timepoints
#
# Return a data frame with the following components:
#   - strata: stratification of curve estimation
#   - time: the timepoints on the curve
#   - n.risk: the number of subjects at risk at time t-0
#   - n.event: the cumulative number of events that have occurred since the last time listed until time t+0
#   - n.censor: number of censored subjects
#   - strata_size: number of subject in the strata
.get_timepoints_survsummary <- function(fit, data, times)
{
  survsummary <- summary(fit, times = times, extend = TRUE)

  if (is.null(fit$strata)) {
    .strata <- factor(rep("All", length(times)))
    strata_names <- "All"
    strata_size <- rep(fit$n, length(.strata))
  }
  else {
    .strata <- factor(survsummary$strata)
    strata_names <- names(fit$strata)
    nstrata <- length(strata_names)
    strata_size <- rep(fit$n, each = length(.strata)/nstrata)
  }

  res <- data.frame(
    strata = .clean_strata(.strata),
    time = survsummary$time,
    n.risk = survsummary$n.risk,
    n.event = survsummary$n.event,
    n.censor = survsummary$n.censor,
    strata_size = strata_size
  )

  if(!is.null(fit$strata)){
    variables <- .get_variables(res$strata, fit, data)
    for(variable in variables) res[[variable]] <- .get_variable_value(variable, res$strata, fit, data)
  }
  res
}

# Get variable names in strata
# -----------------------------------------
# strata: a vector
# fit: survfit object
# data: data used to fit survival curves
.get_variables <- function(strata, fit, data = NULL){
  variables <- sapply(as.vector(strata),
                      function(x){
                        x <- unlist(strsplit(x, "=|,\\s+", perl=TRUE))
                        x[seq(1, length(x), 2)]
                      })
  variables <- unique(as.vector(variables))
  variables <- intersect(variables, colnames(.get_data(fit, data) ))
  variables
}

# levels of a given variable used in survfit formula
# ----------------------------
# variable: variable name
.get_variable_value <- function(variable, strata, fit, data = NULL){
  res <- sapply(as.vector(strata), function(x){
    x <- unlist(strsplit(x, "=|(\\s+)?,\\s+", perl=TRUE))
    index <- grep(paste0("^", variable, "$"), x)
    .trim(x[index+1])
  })
  res <- as.vector(res)
  var_levels <- levels(.get_data(fit, data)[, variable])
  if(!is.null(var_levels)) res <- factor(res, levels = var_levels)
  else res <- as.factor(res)
  res
}


# remove dollar sign ($) in strata
# ---------------------------------
# remove dollar sign ($) in strata, in the situation, where
# the user uses data$variable to fit survival curves
.clean_strata <- function(strata, fit){
  is_dollar_sign <- grepl("$", as.character(strata)[1], fixed=TRUE)
  if(is_dollar_sign) {
    strata <- as.character(strata)
    data_name <- unlist(strsplit(strata[1], "$", fixed =TRUE))[1]
    strata <- gsub(paste0(data_name, "$"), "", strata, fixed=TRUE)
    strata <- as.factor(strata)
  }
  else if(!missing(fit)) strata <- factor(strata, levels = names(fit$strata))
  return(strata)
}


# Build ggsurvplot for printing
# Old version. To be removed
.build_ggsurvplot2 <- function(x, surv.plot.height = NULL,
                               risk.table.height = NULL, ncensor.plot.height = NULL, ...)
{
  if(!inherits(x, "ggsurvplot"))
    stop("An object of class ggsurvplot is required.")

  surv.plot.height <- ifelse(is.null(surv.plot.height), attr(x, "surv.plot.height"), surv.plot.height)
  risk.table.height <- ifelse(is.null(risk.table.height), attr(x, "risk.table.height"), risk.table.height)
  ncensor.plot.height <- ifelse(is.null(ncensor.plot.height), attr(x, "ncensor.plot.height"), ncensor.plot.height)

  if(is.null(risk.table.height)) risk.table.height <- 0.25
  ncensor.plot.height <- ifelse(is.null(x$ncensor.plot), 0, ncensor.plot.height)
  if(is.null(surv.plot.height)) surv.plot.height <- 1-risk.table.height-ncensor.plot.height

  if(!is.null(x$table)){
    # Hide legende: don't use  theme(legend.position = "none") because awkward legend when position = "left"
    x$table <- .hide_legend(x$table)
    risk.table.y.text <- attr(x, 'risk.table.y.text')

    if(!risk.table.y.text)
      x$table <- x$table + theme(axis.text.y = element_text(size = 50, vjust = 0.35),
                                 axis.ticks.y = element_blank())

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
  if(!is.null(x$ncensor.plot)) x$ncensor.plot <- x$ncensor.plot + theme (legend.position = "none")

  res <- NULL
  if(is.null(x$table) & is.null(x$ncensor.plot)) res <- x$plot
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

    res <- gridExtra::arrangeGrob(grobs = grobs, nrow = nplot, heights = unlist(heights))
    # res <- do.call(gridExtra::grid.arrange, c(grobs, nrow = nplot, heights = heights, newpage = newpage))
  }
  return(res)
}


