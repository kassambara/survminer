#' @include utilities.R
NULL

#' Theme for Survminer Plots
#'
#' @description
#' Default theme for plots generated with survminer.
#' @param base_size base font size
#' @param base_family base font family
#' @param font.main,font.submain,font.caption,font.x,font.y,font.tickslab,font.legend a vector of length 3
#'  indicating respectively the size (e.g.: 14), the style (e.g.: "plain",
#'  "bold", "italic", "bold.italic") and the color (e.g.: "red") of main title, subtitle, caption,
#'  xlab and ylab, axis tick labels and legend, respectively. For example \emph{font.x =
#'  c(14, "bold", "red")}.  Use font.x = 14, to change only font size; or use
#'  font.x = "bold", to change only font face.
#'@param legend character specifying legend position. Allowed values are one of
#'  c("top", "bottom", "left", "right", "none"). Default is "top" side position.
#'  to remove the legend use legend = "none". Legend position can be also
#'  specified using a numeric vector c(x, y); see details section.
#'
#' @param ... additional arguments passed to the function theme_survminer().
#'@author Alboukadel Kassambara, \email{alboukadel.kassambara@@gmail.com}
#' @examples
#'
#'# Fit survival curves
#'#++++++++++++++++++++++++++++++++++++
#'require("survival")
#'fit<- survfit(Surv(time, status) ~ sex, data = lung)
#'
#'# Basic survival curves
#'#++++++++++++++++++++++++++++++++++++
#'ggsurv <- ggsurvplot(fit, data = lung, risk.table = TRUE,
#'    main = "Survival curves",
#'    submain = "Based on Kaplan-Meier estimates",
#'    caption = "created with survminer"
#'    )
#'
#'# Change font size, style and color
#'#++++++++++++++++++++++++++++++++++++
#' # Change font size, style and color at the same time
#' # Use font.x = 14, to change only font size; or use
#' # font.x = "bold", to change only font face.
#'ggsurv %+% theme_survminer(
#'      font.main = c(16, "bold", "darkblue"),
#'      font.submain = c(15, "bold.italic", "purple"),
#'      font.caption = c(14, "plain", "orange"),
#'      font.x = c(14, "bold.italic", "red"),
#'      font.y = c(14, "bold.italic", "darkred"),
#'      font.tickslab = c(12, "plain", "darkgreen")
#'    )
#'
#' # Clean risk table
#' # +++++++++++++++++++++++++++++
#' ggsurv$table <- ggsurv$table + theme_cleantable()
#' ggsurv
#'
#' @describeIn ggsurvtheme Default theme for survminer plots. A theme similar to theme_classic() with large font size.
#' @export
theme_survminer <-
  function (base_size = 12, base_family = "",
            font.main = c(16, "plain", "black"), font.submain = c(15, "plain", "black"),
            font.x = c(14, "plain", "black"), font.y = c(14, "plain", "black"),
            font.caption = c(15, "plain", "black"),
            font.tickslab = c(12, "plain", "black"),
            legend = c("top", "bottom", "left", "right", "none"),
            font.legend = c(10, "plain", "black"),
            ...
            )
  {

    font.main <- ggpubr:::.parse_font(font.main)
    font.x <- ggpubr:::.parse_font(font.x)
    font.y <- ggpubr:::.parse_font(font.y)
    font.submain <- ggpubr:::.parse_font(font.submain)
    font.caption <- ggpubr:::.parse_font(font.caption)
    font.tickslab <- ggpubr:::.parse_font(font.tickslab)
    font.legend <- ggpubr:::.parse_font(font.legend)
    if(!is(legend, "numeric")) legend <- match.arg(legend)

    tickslab <-
      element_text(
        size = font.tickslab$size, face = font.tickslab$face,
        colour = font.tickslab$color, angle = 0)

    legend.text <- element_text(size = font.legend$size,
                 face = font.legend$face, colour = font.legend$color)

    result <- theme_classic(base_size = base_size, base_family = base_family) + theme(
      plot.title = element_text(
        size = font.main$size,
        lineheight = 1.0,
        face = font.main$face,
        colour = font.main$color
      ),
      plot.subtitle = element_text(
        size = font.submain$size,
        lineheight = 1.0,
        face = font.submain$face,
        colour = font.submain$color
      ),
      axis.title.x = element_text(
        size = font.x$size,
        face = font.x$face,
        colour = font.x$color
      ),
      axis.title.y = element_text(
        angle = 90,
        size = font.y$size,
        face = font.y$face,
        colour = font.y$color
      ),
      plot.caption = element_text(
        size = font.caption$size,
        lineheight = 1.0,
        face = font.caption$face,
        colour = font.caption$color
      ),
      axis.text.x = tickslab,
      axis.text.y = tickslab,
      legend.position = legend,
      legend.text = legend.text,
      legend.title = legend.text
    )
    class(result) <- "theme"
    result
  }


#' @export
#' @describeIn ggsurvtheme theme for drawing a clean risk table and cumulative
#'   number of events table. A theme similar to theme_survminer() without i)
#'   axis lines and, ii) x axis ticks and title.
theme_cleantable <- function(base_size = 12, base_family = "", ...)
{
  #theme_survminer(base_size = base_size, base_family = base_family, ...) %+replace%
    theme(
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
}
