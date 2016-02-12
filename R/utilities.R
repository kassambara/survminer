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
    "lwd", "pch", "cex"
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
      ggplot2::scale_color_brewer(palette = palette)
    else if (palette == "grey")
      ggplot2::scale_color_grey()
    else if (palette == "hue")
      ggplot2::scale_color_hue(...)
  }
  else if (palette[1] != "")
    ggplot2::scale_color_manual(values = palette, ...)
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
      ggplot2::scale_fill_brewer(palette = palette)
    else if (palette == "grey")
      ggplot2::scale_fill_grey()
    else if (palette == "hue")
      ggplot2::scale_fill_hue(...)
  }
  else if (palette[1] != "")
    ggplot2::scale_fill_manual(values = palette, ...)
}


# Change title and labels
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.labs <- function(p, main = NULL, xlab = NULL, ylab = NULL,
                  font.main = NULL, font.x = NULL, font.y = NULL)
{
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
          size = as.numeric(font.main[1]),
          lineheight = 1.0, face = font.main[2], colour = font.main[3]
        )
      )
  if (!is.null(font.x))
    p <-
      p + theme(axis.title.x = element_text(
        size = as.numeric(font.x[1]),
        face = font.x[2], colour = font.x[3]
      ))
  if (!is.null(font.y))
    p <-
      p + theme(axis.title.y = element_text(
        size = as.numeric(font.y[1]),
        face = font.y[2], colour = font.y[3]
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
          size = as.numeric(font[1]), face = font[2],
          colour = font[3]
        )
      ytickslab <-
        element_text(
          size = as.numeric(font[1]), face = font[2],
          colour = font[3]
        )
      p <- p+theme(axis.text.x = xtickslab, axis.text.y = ytickslab)
    }
    p
  }


# parse font
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.parse_font <- function(font){
   # matching size and face
  size <- grep("[0-9]+", font, perl = TRUE)
  face <- grep("plain|bold|italic|bold.italic", font, perl = TRUE)
  if(length(size) == 0) size <- NULL else size <- font[size]
  if(length(face) == 0) face <- NULL else face <- font[face]
  color <- setdiff(font, c(size, face))
  if(length(color) == 0) color <- NULL
  c(size, face, color)
}


