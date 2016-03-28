#' @include utilities.R
NULL

#' ggplot2 classic theme with axis lines
#'
#' @description
#' Create a ggplot2 classic theme with axis lines.
#' @param base_size base font size
#' @param base_family base font family
#' @examples
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'    geom_point(aes(color = gear))
#'
#' # Default plot
#' p
#'
#' # Use theme_classic()
#' p + theme_classic()
#'
#' # Use theme_classic2()
#' p + theme_classic2()
#' @export
theme_classic2 <-
  function (base_size = 12, base_family = "")
  {
    theme_classic(base_size = base_size, base_family = base_family) %+replace%
      theme(
        axis.line.x = element_line(),
        axis.line.y = element_line()
      )
  }
