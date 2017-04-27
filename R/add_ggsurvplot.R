#'Add Components to a ggsurvplot
#'@description Allows to add ggplot components - theme(), labs(), ... - to an
#'  object of class ggsurv, which is a list of ggplots.
#'@param e1 an object of class ggsurv.
#'@param e2 a plot component such as theme and labs.
#'@seealso \code{\link{theme_survminer}} and \code{\link{ggsurvplot}}
#'@examples
#'# Fit survival curves
#'require("survival")
#'fit<- survfit(Surv(time, status) ~ sex, data = lung)
#'
#'# Basic survival curves
#'p <- ggsurvplot(fit, data = lung, risk.table = TRUE,
#'    main = "Survival curve",
#'    submain = "Based on Kaplan-Meier estimates",
#'    caption = "created with survminer"
#'    )
#'p
#'
#'# Customizing the plots
#'p + theme_survminer(
#'      font.main = c(16, "bold", "darkblue"),
#'      font.submain = c(15, "bold.italic", "purple"),
#'      font.caption = c(14, "plain", "orange"),
#'      font.x = c(14, "bold.italic", "red"),
#'      font.y = c(14, "bold.italic", "darkred"),
#'      font.tickslab = c(12, "plain", "darkgreen")
#')
#'@name add_ggsurvplot
#'@rdname add_ggsurvplot
#'@method + ggsurv
#'@export
"+.ggsurv" <- function (e1, e2)
{

  original.p <- e1
  if(is.ggplot(original.p)) list.plots <- list(original.p)
  else if(is.list(original.p)) list.plots <- original.p
  else stop("Can't handle an object of class ", class (original.p))

  # if (!is.theme(e2)) stop("e2 should be a theme function.")

  for(i in 1:length(list.plots)){
    p <- list.plots[[i]]
    if(is.ggplot(p)) list.plots[[i]] <- p + e2
  }

  if(is.ggplot(original.p)) list.plots[[1]]
  else list.plots
}


#' @rdname add_ggsurvplot
#' @export
"%++%" <- `+.ggsurv`
