#' Forest Plot for Cox Proportional Hazards Model
#'
#' @description Drawing Forest Plot for Cox proportional hazards model. In two panels the model structure is presented.
#' @param model an object of class coxph.
#' @param data a dataset used to fit survival curves. If not supplied then data
#'  will be extracted from 'fit' object.
#' @param alpha significance level for coloring.
#' @param ggtheme function, ggplot2 theme name. Default value is theme_classic2. Allowed values include ggplot2 official themes: see theme.
#' @param plot.title legend title.
#' @param palette the color palette to be used for coloring of significant variables.
#' @param xlab Label in OX axis.
#'@param ... further arguments passed to the function \code{\link[ggpubr]{ggpar}} for customizing the plot.
#'
#' @return return an object of class ggplot
#'
#' @author Przemyslaw Biecek, \email{przemyslaw.biecek@@gmail.com}
#'
#' @examples
#' require("survival")
#' model <- coxph( Surv(time, status) ~ sex + rx + adhere,
#'                 data = colon )
#' ggforest(model)
#'
#' @export
#' @import broom
#' @import grid
#' @import gridExtra

ggforest <- function(model, data = NULL, alpha = 0.05,
                     plot.title = "Forest plot for coxph model",
                     ggtheme = theme_survminer(),
                     palette = c("black", "red4"),
                     xlab = "Hazard ratio", ...) {
  coef <- tidy(model)
  coef$p.val <- sapply(coef$p.value, function(x) as.character(signif(x, digits = 2)))
  coef$est <- sapply(exp(coef$estimate), function(x) as.character(signif(x, digits = 2)))
  stars <- paste0(ifelse(coef$p.value < 0.05, "*",""),
                  ifelse(coef$p.value < 0.01, "*",""),
                  ifelse(coef$p.value < 0.001, "*",""))
  coef$p.val <- paste0(coef$est, " (p.value ", coef$p.val, stars,")")
  coef$issig <- coef$p.value < alpha

  ##
  data <- .get_data(model, data = data)
  terms <- attr(model$terms, "dataClasses")[-1]
  terms <- terms[intersect(names(terms),
                           gsub(rownames(anova(model))[-1], pattern = "`", replacement = ""))]

  allTerms <- lapply(seq_along(terms), function(i){
    var <- names(terms)[i]
    if (terms[i] == "factor") {
      adf <- as.data.frame(table(data[,var]))
      cbind(var=var, adf, pos=1:nrow(adf))
    } else {
      data.frame(var=var, Var1 = "", Freq = nrow(data), pos=1)
    }
  })
  allTermsDF <- do.call(rbind, allTerms)
  colnames(allTermsDF) <- c("var", "level", "N", "pos")
  inds <- apply(allTermsDF[,1:2], 1, paste0, collapse="")

  coef <- tidy(model)
  rownames(coef) <- gsub(coef$term, pattern = "`", replacement = "")
  toShow <- cbind(allTermsDF, coef[inds,])[,c("var", "level", "N", "p.value", "estimate", "conf.low", "conf.high", "pos")]
  toShowExp <- toShow[,5:7]
  toShowExp[is.na(toShowExp)] <- 0
  toShowExp <- format(exp(toShowExp), digits=2)
  toShowExpClean <- data.frame(toShow,
                               pvalue = signif(toShow[,4],3),
                               toShowExp)
  toShowExpClean$stars <- paste0(round(toShowExpClean$p.value, 3), " ",
                  ifelse(toShowExpClean$p.value < 0.05, "*",""),
                  ifelse(toShowExpClean$p.value < 0.01, "*",""),
                  ifelse(toShowExpClean$p.value < 0.001, "*",""))
  toShowExpClean$ci <- paste0("(",toShowExpClean[,"conf.low.1"]," - ",toShowExpClean[,"conf.high.1"],")")
  toShowExpClean$estimate.1[is.na(toShowExpClean$estimate)] = "reference"
  toShowExpClean$stars[which(toShowExpClean$p.value < 0.001)] = "<0.001 ***"
  toShowExpClean$stars[is.na(toShowExpClean$estimate)] = ""
  toShowExpClean$ci[is.na(toShowExpClean$estimate)] = ""
  toShowExpClean$estimate[is.na(toShowExpClean$estimate)] = 0

  # revert
  toShowExpClean <- toShowExpClean[nrow(toShowExpClean):1,]

  breaks <- axisTicks(range(c(toShowExpClean$conf.high, toShowExpClean$conf.low), na.rm = TRUE),
            log=TRUE, nint=11)

  p2 <- ggplot(toShowExpClean, aes(seq_along(var), exp(estimate))) +
    geom_point(pch=15, size=4) +
    geom_errorbar(aes(ymin=exp(conf.low), ymax=exp(conf.high)), width=0.15) +
    geom_hline(yintercept=1, linetype=3) +
    coord_flip() +
    scale_y_log10(
      name = "Hazard Ratio",
      labels = sprintf("%g", breaks),
      expand = c(0.02, 0.02),
      breaks = breaks) +
    theme_light() +
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "none",
          panel.border=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    xlab("")

  r <- nrow(toShowExpClean)

  # hazard plots
  grid.arrange(grid.points(1, 1, gp = gpar(col="white")),
               p2, ncol=2)
  # vnames
  coord1 <- 0.02
  coord2 <- 0.16
  coord3 <- 0.4
  for(i in 1:r) {
    if (toShowExpClean[i,"pos"] == 1)
      grid.text(toShowExpClean[i,"var"], coord1, 0.083 + 0.91*(i-0.5)/r, just = c(0,0), gp=gpar(cex=0.7, fontface="bold"))
    grid.text(toShowExpClean[i,"level"], coord2, 0.083 + 0.91*(i-0.5)/r, just = c(0,0), gp=gpar(cex=0.7))
    grid.text(toShowExpClean[i,"estimate.1"], coord3, 0.083 + 0.91*(i-0.5)/r, just = c(0.5,-0.5), gp=gpar(cex=0.7))
    grid.text(toShowExpClean[i,"ci"],
              coord3, 0.083 + 0.91*(i-0.5)/r, just = c(0.5,1), gp=gpar(cex=0.7, fontface = "italic"))
    grid.text(toShowExpClean[i,"stars"],
              0.99, 0.083 + 0.91*(i-0.5)/r, just = c(1,-0.5), gp=gpar(cex=0.7, fontface = "italic"))
  }
  # grey bands
  for(i in seq(1,r,2))
    grid.rect(0.5, 0.083 + 0.91*(i-0.5)/r, 1, 0.91/r,
              gp = gpar(fill="black", col=NA, alpha=0.1))

}
