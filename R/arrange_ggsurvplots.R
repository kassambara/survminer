#' @include ggsurvplot.R
  NULL
#'Arranging Multiple ggsurvplots
#'@description Arranging multiple ggsurvplots on the same page.
#'@param x a list of ggsurvplots.
#'@param print logical value. If TRUE, the arranged plots are displayed.
#'@param title character vector specifying page title. Default is NA.
#'@param ncol,nrow the number of columns and rows, respectively.
#'@param risk.table.height the height of the risk table on the grid. Increase
#'  the value when you have many strata. Default is 0.25. Ignored when
#'  risk.table = FALSE.
#'@param surv.plot.height the height of the survival plot on the grid. Default
#'  is 0.75. Ignored when risk.table = FALSE. \code{1-risk.table.height -
#'  ncensor.plot.height} when \code{risk.table = TRUE} and \code{ncensor.plot =
#'  TRUE}
#'@param ncensor.plot.height The height of the censor plot. Used when
#'  \code{ncensor.plot = TRUE}.
#'@param ... not used
#'
#'@return returns an invisible object of class arrangelist (see
#'  \link[gridExtra]{marrangeGrob}), which can be saved into a pdf file using
#'  the function \link[ggplot2]{ggsave}.
#'
#'@author Alboukadel Kassambara, \email{alboukadel.kassambara@@gmail.com}
#' @examples
#'
#'# Fit survival curves
#'require("survival")
#'fit<- survfit(Surv(time, status) ~ sex, data = lung)
#'
#'# List of ggsurvplots
#'require("survminer")
#'splots <- list()
#'splots[[1]] <- ggsurvplot(fit, data = lung, risk.table = TRUE, ggtheme = theme_minimal())
#'splots[[2]] <- ggsurvplot(fit, data = lung, risk.table = TRUE, ggtheme = theme_grey())
#'
#'# Arrange multiple ggsurvplots and print the output
#'arrange_ggsurvplots(splots, print = TRUE,
#'   ncol = 2, nrow = 1, risk.table.height = 0.4)
#'
#'\dontrun{
#'# Arrange and save into pdf file
#'res <- arrange_ggsurvplots(splots, print = FALSE)
#'ggsave("myfile.pdf", res)
#'}
#'
#'
#'@rdname arrange_ggsurvplots
#'@export
arrange_ggsurvplots <- function(x,  print = TRUE, title = NA, ncol = 2, nrow = 1, surv.plot.height = NULL,
                                         risk.table.height = NULL, ncensor.plot.height = NULL, ...)
{

  # Build each ggsurvplot in the list
  survs <- lapply(x, .build_ggsurvplot,
                         surv.plot.height = surv.plot.height,
                         risk.table.height = risk.table.height,
                         ncensor.plot.height = ncensor.plot.height)

  # Arrange multiple ggsurvplots
  survs <- do.call(gridExtra::marrangeGrob,
                  list(grobs = survs, ncol = ncol, nrow = nrow, top = title))

  if(print) print(survs)

  invisible(survs)
}

