#' @import ggplot2


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
.labs <- function(p, main = NULL, xlab = NULL, ylab = NULL,
                  font.main = NULL, font.x = NULL, font.y = NULL)
{
  font.main <- .parse_font(font.main)
  font.x <- .parse_font(font.x)
  font.y <- .parse_font(font.y)

  if (!is.null(main)) {
    if (main != FALSE)
      p <- p + labs(title = main)
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


