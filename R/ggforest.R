#' Drawing Forest Plot for CoxPH model
#' @description Drawing Forest Plot for CoxPH model
#' @param fit an object of class coxph.
#' @param alpha significance level for coloring.
#' @param ggtheme function, ggplot2 theme name. Default value is theme_classic2. Allowed values include ggplot2 official themes: see theme.
#' @param legend.title legend title.
#' @param font.main,font.submain,font.caption,font.x,font.y,font.tickslab a vector of length 3
#'  indicating respectively the size (e.g.: 14), the style (e.g.: "plain",
#'  "bold", "italic", "bold.italic") and the color (e.g.: "red") of main title, subtitle, caption,
#'  xlab and ylab and axis tick labels, respectively. For example \emph{font.x =
#'  c(14, "bold", "red")}.  Use font.x = 14, to change only font size; or use
#'  font.x = "bold", to change only font face.
#'
#' @return return an object of class ggplot
#'
#' @examples
#' require("survival")
#' model <- coxph( Surv(time, status) ~ sex + rx + adhere,
#'                 data = colon )
#' ggforest(model)
#'
#' @export
#' @import broom

ggforest <- function(model, alpha = 0.05,
                     plot.title = "Forest plot for coxph model",
                     ggtheme = theme_classic2(),
                     palette = c("black", "red4"),
                     font.main = c(16, "plain", "black"), font.submain = c(15, "plain", "black"),
                     font.caption = c(15, "plain", "black"),
                     font.x = c(14, "plain", "black"), font.y = c(14, "plain", "black"),
                     font.tickslab = c(12, "plain", "black")) {
  coef <- tidy(model)
  coef$p.val <- sapply(coef$p.value, function(x) as.character(signif(x, digits = 2)))
  coef$est <- sapply(exp(coef$estimate), function(x) as.character(signif(x, digits = 2)))
  stars <- paste0(ifelse(coef$p.value < 0.05, "*",""),
                  ifelse(coef$p.value < 0.01, "*",""),
                  ifelse(coef$p.value < 0.001, "*",""),
                  xlab = "Hazard ratio",
                  font.main = c(16, "bold", "darkblue"),          # font for titles of the plot, the table and censor part
                  font.submain = c(15, "bold.italic", "purple"),  # font for subtitles in the plot, the table and censor part
                  font.caption = c(14, "plain", "orange"),        # font for captions in the plot, the table and censor part
                  font.x = c(14, "bold.italic", "red"),           # font for x axises in the plot, the table and censor part
                  font.y = c(14, "bold.italic", "darkred"),       # font for y axises in the plot, the table and censor part
                  font.tickslab = c(12, "plain", "darkgreen")    # font for ticklabs in the plot, the table and censor part
  )
  coef$p.val <- paste0(coef$est, " (p.value ", coef$p.val, stars,")")
  coef$issig <- coef$p.value < alpha

  p <- ggplot(coef, aes(term, exp(estimate), color=issig)) +
    geom_point() +
    geom_errorbar(aes(ymin=exp(conf.low), ymax=exp(conf.high)), width=0.15) +
    geom_text(aes(label=p.val), vjust=-0.25, hjust=0) +
    geom_hline(yintercept=1, linetype=3) +
    coord_flip() +
    scale_y_log10(
      name = xlab,
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    scale_color_manual(values = palette) +
    ggtheme +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), legend.position = "none") +
    xlab("") +
    ggtitle(plot.title)
  p <-.labs(p = p, font.main = font.main, font.x = font.x, font.y = font.y, font.submain = font.submain, font.caption = font.caption)
  p
}
