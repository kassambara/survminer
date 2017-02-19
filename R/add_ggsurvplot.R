#' Add Components to a ggsurvplot
#'@description Allows to add ggplot theme function to an object of class ggsurvplot.
#'@param e1 an object of class \code{\link{ggsurvplot}}.
#'@param e2 a theme function.
#'@examples
#'# Fit survival curves
#'require("survival")
#'fit<- survfit(Surv(time, status) ~ sex, data = lung)
#'
#'# Basicsurvival curves
#'p <- ggsurvplot(fit, data = lung, risk.table = TRUE)
#'p
#'
#'# Customizing the plots
#'p %+% theme_survminer(
#'      font.main = c(16, "bold", "darkblue"),
#'      font.submain = c(15, "bold.italic", "purple"),
#'      font.caption = c(14, "plain", "orange"),
#'      font.x = c(14, "bold.italic", "red"),
#'      font.y = c(14, "bold.italic", "darkred"),
#'      font.tickslab = c(12, "plain", "darkgreen")
#')
#'
#'@rdname add-ggsurvplot
#'@method + ggsurvplot
#'@export
"+.ggsurvplot" <- function (e1, e2)
{
  if (!is.theme(e2)) stop("e2 should be a theme function.")
  if(inherits(e1, "ggsurvplot")){
    e1$plot <- e1$plot + e2
    if(!is.null(e1$table)) e1$table <- e1$table + e2
    if(!is.null(e1$ncensor.plot)) e1$ncensor.plot <- e1$ncensor.plot + e2
  }
  e1
}

#' @rdname add-ggsurvplot
#' @export
"%+%" <- `+.ggsurvplot`
