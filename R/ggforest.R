#' Forest Plot for Cox Proportional Hazards Model
#'
#' @description Drawing Forest Plot for Cox proportional hazards model. In two panels the model structure is presented.
#' @param model an object of class coxph.
#' @param data a dataset used to fit survival curves. If not supplied then data
#'  will be extracted from 'fit' object.
#' @param main title of the plot.
#' @param cpositions relative positions of first three columns in the OX scale.
#' @param fontsize relative size of annotations in the plot. Default value: 0.7.
#' @param refLabel label for reference levels of factor variables.
#' @param noDigits number of digits for estimates and p-values in the plot.
#'
#' @return returns a ggplot2 object (invisibly)
#'
#' @author Przemyslaw Biecek (\email{przemyslaw.biecek@@gmail.com}),
#'   Fabian Scheipl (\email{fabian.scheipl@@gmail.com})
#'
#' @examples
#' require("survival")
#' model <- coxph( Surv(time, status) ~ sex + rx + adhere,
#'                 data = colon )
#' ggforest(model)
#'
#' colon <- within(colon, {
#'   sex <- factor(sex, labels = c("female", "male"))
#'   differ <- factor(differ, labels = c("well", "moderate", "poor"))
#'   extent <- factor(extent, labels = c("submuc.", "muscle", "serosa", "contig."))
#' })
#' bigmodel <-
#'   coxph(Surv(time, status) ~ sex + rx + adhere + differ + extent + node4,
#'     data = colon )
#' ggforest(bigmodel)
#'
#' @export
#' @import broom
#' @import grid
#' @import gridExtra
#' @importFrom grDevices axisTicks
#' @importFrom stats anova var

ggforest <- function(model, data = NULL,
  main = "Hazard ratio", cpositions=c(0.02, 0.22, 0.4),
  fontsize = 0.7, refLabel = "reference", noDigits=2) {
  conf.high <- conf.low <- estimate <- NULL
  stopifnot(inherits(model, "coxph"))

  # get data and variables/terms from cox model
  data  <- .get_data(model, data = data)
  terms <- attr(model$terms, "dataClasses")[-1]
# removed as requested in #388
#  terms <- terms[intersect(names(terms),
#    gsub(rownames(anova(model))[-1], pattern = "`", replacement = ""))]

  # use broom to get some required statistics
  coef <- as.data.frame(tidy(model, conf.int = TRUE))
  gmodel <- glance(model)

  # extract statistics for every variable
  allTerms <- lapply(seq_along(terms), function(i){
    var <- names(terms)[i]
    if (terms[i] %in% c("factor", "character")) {
      adf <- as.data.frame(table(data[, var]))
      cbind(var = var, adf, pos = 1:nrow(adf))
    }
    else if (terms[i] == "numeric") {
      data.frame(var = var, Var1 = "", Freq = nrow(data),
                 pos = 1)
    }
    else {
      vars = grep(paste0("^", var, "*."), coef$term, value=TRUE)
      data.frame(var = vars, Var1 = "", Freq = nrow(data),
                 pos = seq_along(vars))
    }
  })
  allTermsDF <- do.call(rbind, allTerms)
  colnames(allTermsDF) <- c("var", "level", "N", "pos")
  inds <- apply(allTermsDF[,1:2], 1, paste0, collapse="")

  # use broom again to get remaining required statistics
  rownames(coef) <- gsub(coef$term, pattern = "`", replacement = "")
  toShow <- cbind(allTermsDF, coef[inds,])[,c("var", "level", "N", "p.value", "estimate", "conf.low", "conf.high", "pos")]
  toShowExp <- toShow[,5:7]
  toShowExp[is.na(toShowExp)] <- 0
  toShowExp <- format(exp(toShowExp), digits=noDigits)
  toShowExpClean <- data.frame(toShow,
    pvalue = signif(toShow[,4],noDigits+1),
    toShowExp)
  toShowExpClean$stars <- paste0(round(toShowExpClean$p.value, noDigits+1), " ",
    ifelse(toShowExpClean$p.value < 0.05, "*",""),
    ifelse(toShowExpClean$p.value < 0.01, "*",""),
    ifelse(toShowExpClean$p.value < 0.001, "*",""))
  toShowExpClean$ci <- paste0("(",toShowExpClean[,"conf.low.1"]," - ",toShowExpClean[,"conf.high.1"],")")
  toShowExpClean$estimate.1[is.na(toShowExpClean$estimate)] = refLabel
  toShowExpClean$stars[which(toShowExpClean$p.value < 0.001)] = "<0.001 ***"
  toShowExpClean$stars[is.na(toShowExpClean$estimate)] = ""
  toShowExpClean$ci[is.na(toShowExpClean$estimate)] = ""
  toShowExpClean$estimate[is.na(toShowExpClean$estimate)] = 0
  toShowExpClean$var = as.character(toShowExpClean$var)
  toShowExpClean$var[duplicated(toShowExpClean$var)] = ""
  # make label strings:
  toShowExpClean$N <- paste0("(N=",toShowExpClean$N,")")

  #flip order
  toShowExpClean <- toShowExpClean[nrow(toShowExpClean):1, ]

  rangeb <- range(toShowExpClean$conf.low, toShowExpClean$conf.high, na.rm = TRUE)
  breaks <- axisTicks(rangeb/2, log = TRUE, nint = 7)
  rangeplot <- rangeb
  # make plot twice as wide as needed to create space for annotations
  rangeplot[1] <- rangeplot[1] - diff(rangeb)
  # increase white space on right for p-vals:
  rangeplot[2] <- rangeplot[2] + .15 * diff(rangeb)

  width <- diff(rangeplot)
  # y-coordinates for labels:
  y_variable <- rangeplot[1] +  cpositions[1] * width
  y_nlevel <- rangeplot[1]  +  cpositions[2] * width
  y_cistring <- rangeplot[1]  +  cpositions[3] * width
  y_stars <- rangeb[2]
  x_annotate <- seq_len(nrow(toShowExpClean))

  # geom_text fontsize is in mm (https://github.com/tidyverse/ggplot2/issues/1828)
  annot_size_mm <- fontsize *
    as.numeric(convertX(unit(theme_get()$text$size, "pt"), "mm"))

  p <- ggplot(toShowExpClean, aes(seq_along(var), exp(estimate))) +
    geom_rect(aes(xmin = seq_along(var) - .5, xmax = seq_along(var) + .5,
      ymin = exp(rangeplot[1]), ymax = exp(rangeplot[2]),
      fill = ordered(seq_along(var) %% 2 + 1))) +
    scale_fill_manual(values = c("#FFFFFF33", "#00000033"), guide = "none") +
    geom_point(pch = 15, size = 4) +
    geom_errorbar(aes(ymin = exp(conf.low), ymax = exp(conf.high)), width = 0.15) +
    geom_hline(yintercept = 1, linetype = 3) +
    coord_flip(ylim = exp(rangeplot)) +
    ggtitle(main) +
    scale_y_log10(
      name = "",
      labels = sprintf("%g", breaks),
      expand = c(0.02, 0.02),
      breaks = breaks) +
    theme_light() +
    theme(panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "none",
      panel.border=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      plot.title = element_text(hjust = 0.5)) +
    xlab("") +
    annotate(geom = "text", x = x_annotate, y = exp(y_variable),
      label = toShowExpClean$var, fontface = "bold", hjust = 0,
      size = annot_size_mm) +
    annotate(geom = "text", x = x_annotate, y = exp(y_nlevel), hjust = 0,
      label = toShowExpClean$level, vjust = -0.1, size = annot_size_mm) +
    annotate(geom = "text", x = x_annotate, y = exp(y_nlevel),
      label = toShowExpClean$N, fontface = "italic", hjust = 0,
      vjust = ifelse(toShowExpClean$level == "", .5, 1.1),
      size = annot_size_mm) +
    annotate(geom = "text", x = x_annotate, y = exp(y_cistring),
      label = toShowExpClean$estimate.1, size = annot_size_mm,
      vjust = ifelse(toShowExpClean$estimate.1 == "reference", .5, -0.1)) +
    annotate(geom = "text", x = x_annotate, y = exp(y_cistring),
      label = toShowExpClean$ci, size = annot_size_mm,
      vjust = 1.1,  fontface = "italic") +
    annotate(geom = "text", x = x_annotate, y = exp(y_stars),
      label = toShowExpClean$stars, size = annot_size_mm,
      hjust = -0.2,  fontface = "italic") +
    annotate(geom = "text", x = 0.5, y = exp(y_variable),
      label = paste0("# Events: ", gmodel$nevent, "; Global p-value (Log-Rank): ",
        format.pval(gmodel$p.value.log, eps = ".001"), " \nAIC: ", round(gmodel$AIC,2),
        "; Concordance Index: ", round(gmodel$concordance,2)),
      size = annot_size_mm, hjust = 0, vjust = 1.2,  fontface = "italic")
  # switch off clipping for p-vals, bottom annotation:
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  # grid.draw(gt)
  # invisible(p)
  ggpubr::as_ggplot(gt)
}
