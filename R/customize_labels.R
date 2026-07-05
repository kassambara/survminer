#' Customize the Font of ggsurvplot Labels
#'
#' @description Set the font (size, style and color) of the plot main title,
#'  subtitle, caption, axis titles and axis tick labels of a \code{ggsurvplot}
#'  object or of any \code{ggplot}. Works on the whole \code{\link{ggsurvplot}}
#'  object at once (survival curve, risk table and censor plot) or on an
#'  individual component (e.g. \code{ggsurv$table}).
#'
#'  Compared to \code{\link[ggpubr]{ggpar}()}, \code{customize_labels()} sets the
#'  labels using \code{\link[ggtext]{element_markdown}()}, so it preserves (and
#'  can style) the markdown-formatted risk-table strata labels produced by
#'  \code{ggsurvplot()}.
#'
#' @param p a \code{ggsurvplot} object, a \code{ggplot} or a list of
#'  \code{ggplot}s.
#' @param font.title,font.subtitle,font.caption a vector of length 3 indicating
#'  respectively the size (e.g.: 14), the style (e.g.: "plain", "bold", "italic",
#'  "bold.italic") and the color (e.g.: "red") of the plot main title, subtitle
#'  and caption, respectively. For example \code{font.title = c(14, "bold",
#'  "red")}. Use \code{NULL} (default) to leave the corresponding label
#'  unchanged.
#' @param font.x,font.y a vector of length 3 indicating respectively the size,
#'  the style and the color of the x and y axis titles.
#' @param font.xtickslab,font.ytickslab a vector of length 3 indicating
#'  respectively the size, the style and the color of the x and y axis tick
#'  labels.
#'
#' @return Returns an object of the same type as the input \code{p} (a single
#'  \code{ggplot} when a \code{ggplot} is supplied, otherwise the list/
#'  \code{ggsurvplot} with its \code{ggplot} components updated).
#'
#' @examples
#' \dontrun{
#' library(survival)
#' fit <- survfit(Surv(time, status) ~ sex, data = lung)
#' ggsurv <- ggsurvplot(fit, data = lung, risk.table = TRUE)
#'
#' # Apply the same font style to all the components of ggsurv
#' # (survival curve, risk table and censor part)
#' ggsurv <- customize_labels(
#'   ggsurv,
#'   font.title     = c(16, "bold", "darkblue"),
#'   font.subtitle  = c(15, "bold.italic", "purple"),
#'   font.caption   = c(14, "plain", "orange"),
#'   font.x         = c(14, "bold.italic", "red"),
#'   font.y         = c(14, "bold.italic", "darkred"),
#'   font.xtickslab = c(12, "plain", "darkgreen")
#' )
#' ggsurv
#'
#' # Customize only the risk table
#' ggsurv$table <- customize_labels(
#'   ggsurv$table,
#'   font.title = c(13, "bold.italic", "green")
#' )
#' print(ggsurv)
#' }
#'
#' @export
customize_labels <- function(p, font.title = NULL,
                             font.subtitle = NULL, font.caption = NULL,
                             font.x = NULL, font.y = NULL,
                             font.xtickslab = NULL, font.ytickslab = NULL) {
  original.p <- p
  if (ggplot2::is_ggplot(original.p)) {
    list.plots <- list(original.p)
  } else if (is.list(original.p)) {
    list.plots <- original.p
  } else {
    stop("Can't handle an object of class ", class(original.p))
  }

  .set_font <- function(font) {
    font <- .parse_customize_font(font)
    ggtext::element_markdown(
      size = font$size, face = font$face, colour = font$color
    )
  }

  for (i in seq_along(list.plots)) {
    p <- list.plots[[i]]
    if (ggplot2::is_ggplot(p)) {
      if (!is.null(font.title))
        p <- p + theme(plot.title = .set_font(font.title))
      if (!is.null(font.subtitle))
        p <- p + theme(plot.subtitle = .set_font(font.subtitle))
      if (!is.null(font.caption))
        p <- p + theme(plot.caption = .set_font(font.caption))
      if (!is.null(font.x))
        p <- p + theme(axis.title.x = .set_font(font.x))
      if (!is.null(font.y))
        p <- p + theme(axis.title.y = .set_font(font.y))
      if (!is.null(font.xtickslab))
        p <- p + theme(axis.text.x = .set_font(font.xtickslab))
      if (!is.null(font.ytickslab))
        p <- p + theme(axis.text.y = .set_font(font.ytickslab))
      list.plots[[i]] <- p
    }
  }

  if (ggplot2::is_ggplot(original.p)) list.plots[[1]] else list.plots
}


# Parse a font specification c(size, face, color) into a named list.
# Local copy so we don't reach into ggpubr internals via ':::'.
.parse_customize_font <- function(font) {
  if (is.null(font)) {
    return(NULL)
  }
  if (inherits(font, "list")) {
    return(font)
  }
  size_idx <- grep("^[0-9]+([\\.,][0-9]+)?$", font, perl = TRUE)
  face_idx <- grep("plain|bold|italic|bold.italic", font, perl = TRUE)
  if (length(size_idx) == 0) {
    size <- NULL
  } else {
    size <- as.numeric(gsub(",", ".", font[size_idx], fixed = TRUE))
  }
  face <- if (length(face_idx) == 0) NULL else font[face_idx]
  color_idx <- setdiff(seq_along(font), c(size_idx, face_idx))
  color <- if (length(color_idx) == 0) NULL else font[color_idx]
  list(size = size, face = face, color = color)
}
