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



