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
#' @param global.stats logical value. Default is TRUE. If FALSE, the bottom
#'   caption reporting the global statistics (number of events, global
#'   likelihood-ratio-test p-value, AIC and concordance index) is omitted. Useful
#'   when arranging several forest plots in a panel.
#' @param ref.display logical value. Default is TRUE. If FALSE, the reference
#'   level rows of factor variables (the baselines, which have no hazard ratio)
#'   are omitted from both the plot and the table, keeping only the compared
#'   levels. Default TRUE shows every level (unchanged).
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
  fontsize = 0.7, refLabel = "reference", noDigits=2,
  global.stats = TRUE, ref.display = TRUE) {
  conf.high <- conf.low <- estimate <- .row <- NULL
  stopifnot(inherits(model, "coxph"))

  # get data and variables/terms from cox model
  data  <- .get_data(model, data = data)

  # coxph() drops rows with missing values in any model variable
  # (na.action = na.omit by default), so counting all rows of `data` would
  # overstate the sample size reported per term/level. Restrict `data` to the
  # complete cases the model actually used, matching model$n. Models fit on data
  # with no missing values are unaffected (the subset is a no-op) (#597).
  .model.vars <- intersect(all.vars(stats::terms(model)), colnames(data))
  if (length(.model.vars) > 0) {
    .complete <- stats::complete.cases(data[, .model.vars, drop = FALSE])
    if (any(!.complete)) data <- data[.complete, , drop = FALSE]
  }

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
      # A plain column name is extracted directly with data[[var]] (this also
      # handles non-syntactic names such as "risk group"/"a-b", which parse()
      # would choke on, and sidesteps the tibble one-column-data-frame gotcha);
      # only an in-formula transformation (e.g. as.factor(rx), which is not a
      # column) is evaluated. The lookup is kept inline so the column keeps its
      # own name for the later rbind (#240).
      adf <- as.data.frame(table(
        if (var %in% colnames(data)) data[[var]]
        else eval(parse(text = var), envir = data)))
      cbind(var = var, adf, pos = 1:nrow(adf))
    }
    else if (terms[i] == "numeric") {
      data.frame(var = var, Var1 = "", Freq = nrow(data),
                 pos = 1)
    }
    else {
      # Map coefficients to this term via model$assign, which is reliable,
      # rather than a regex on the coefficient names. The old pattern
      # "^var*." treated trailing digits of the name as a regex quantifier, so
      # e.g. term "add11" matched coefficient "add17TRUE" and vice-versa,
      # producing duplicated/wrong rows for prefix-colliding names (#689).
      idx <- model$assign[[var]]
      if (is.null(idx)) idx <- which(startsWith(coef$term, var)) # literal fallback
      vars = coef$term[idx]
      data.frame(var = vars, Var1 = "", Freq = nrow(data),
                 pos = seq_along(vars))
    }
  })
  allTermsDF <- do.call(rbind, allTerms)
  colnames(allTermsDF) <- c("var", "level", "N", "pos")
  inds <- apply(allTermsDF[,1:2], 1, paste0, collapse="")

  # use broom again to get remaining required statistics
  rownames(coef) <- gsub(coef$term, pattern = "`", replacement = "")
  # Match term rows EXACTLY: character row indexing (coef[inds, ]) partial-
  # matches, so a reference level (e.g. "grpBar", absent from coef) would wrongly
  # inherit a prefix-colliding level's statistics (e.g. "grpBarb") instead of
  # being left as the reference (NA). match() gives an exact lookup (#312).
  toShow <- cbind(allTermsDF, coef[match(inds, rownames(coef)),])[,c("var", "level", "N", "p.value", "estimate", "conf.low", "conf.high", "pos")]
  toShowExp <- toShow[,5:7]
  toShowExp[is.na(toShowExp)] <- 0
  toShowExp <- format(exp(toShowExp), digits=noDigits)
  toShowExpClean <- data.frame(toShow,
    pvalue = signif(toShow[,4],noDigits+1),
    toShowExp)
  # ref.display = FALSE drops the reference-level rows (factor baselines) from the
  # forest -- both the drawn point and the table row -- keeping only the compared
  # levels. A reference level is exactly a row with an NA estimate (a baseline has
  # no hazard ratio); this is a precise marker, since a non-converged coefficient
  # is Inf/huge rather than NA, so no genuine estimate is dropped. Default TRUE
  # keeps every row, so existing plots are unchanged (#563).
  if (!ref.display)
    toShowExpClean <- toShowExpClean[!is.na(toShowExpClean$estimate), , drop = FALSE]
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
  # Rows whose hazard ratio or a confidence limit is non-finite on the drawn
  # (exp / log10) scale come from a Cox model that did not converge -- usually
  # complete/quasi-complete separation. The stored coefficient may be a large but
  # finite log value whose exp() overflows to Inf (upper limit) or underflows to
  # 0 (lower limit); either way it cannot be placed on the log axis, and drawing
  # it yields a misleading full-width interval plus a "log-10 transformation
  # introduced infinite values" warning. Flag these rows (tested on the exp'd
  # values) so the point/error-bar layers skip them; the row still appears in the
  # table with its numeric labels. NA (reference) levels are NOT flagged -- their
  # exp'd coordinates are NA, guarded below -- so they are still drawn at HR = 1.
  # Converged models have no such rows -> no-op, output unchanged (#406).
  .undrawable <- function(v){ v <- exp(v); !is.na(v) & (!is.finite(v) | v <= 0) }
  toShowExpClean$.nonfinite <- .undrawable(toShowExpClean$estimate) |
    .undrawable(toShowExpClean$conf.low) | .undrawable(toShowExpClean$conf.high)
  nonfinite_terms <- unique(as.character(toShowExpClean$var)[toShowExpClean$.nonfinite])
  toShowExpClean$var = as.character(toShowExpClean$var)
  toShowExpClean$var[duplicated(toShowExpClean$var)] = ""
  # make label strings:
  toShowExpClean$N <- paste0("(N=",toShowExpClean$N,")")

  #flip order
  toShowExpClean <- toShowExpClean[nrow(toShowExpClean):1, ]
  # Stable row index for the drawn layers, so that subsetting the point/error-bar
  # data to finite rows does not renumber x (the geoms otherwise use
  # seq_along(var), which would misalign a subset from the rects/annotations).
  toShowExpClean$.row <- seq_len(nrow(toShowExpClean))
  if (length(nonfinite_terms) > 0)
    warning("ggforest: the hazard ratio / confidence interval for term(s) ",
            paste(nonfinite_terms, collapse = ", "), " is non-finite -- the Cox ",
            "model likely did not converge (e.g. complete/quasi-complete ",
            "separation). Those rows are listed in the table but not drawn as a ",
            "point/interval.", call. = FALSE)

  # Axis range from the DRAWABLE rows only (a non-finite row is not plotted, so it
  # must not stretch the axis). For converged models no row is dropped, so this is
  # identical to the previous range over all rows. Fall back to the full range (the
  # clamp below then bounds it to a finite window) when the drawable rows give no
  # finite range -- e.g. a single factor covariate whose only drawable row is the
  # NA-CI reference level, or every row non-finite -- so axisTicks() never receives
  # an empty/inverted range.
  .drawable <- !toShowExpClean$.nonfinite
  # suppressWarnings: when the drawable subset is empty / all-NA (e.g. a single
  # separated factor covariate whose only drawable row is the NA-CI reference),
  # range(na.rm=TRUE) warns "no non-missing arguments to min"; the fallback below
  # then supplies a finite range, so that warning is just noise.
  rangeb <- suppressWarnings(range(toShowExpClean$conf.low[.drawable],
                                   toShowExpClean$conf.high[.drawable], na.rm = TRUE))
  if (!all(is.finite(rangeb)))
    rangeb <- range(toShowExpClean$conf.low, toShowExpClean$conf.high, na.rm = TRUE)
  # (Quasi-)complete separation in the Cox model yields near-infinite
  # coefficients, whose exp()-ed confidence limits overflow and make
  # axisTicks() error with "'at' creation, _LARGE_ range". Clamp the (log-scale)
  # axis range to a finite window and warn, so the plot still renders (#570,
  # #590). This is a no-op for ordinary models (their range sits well inside).
  finite.range <- log(c(1e-6, 1e6))
  if (any(!is.finite(rangeb)) || rangeb[1] < finite.range[1] || rangeb[2] > finite.range[2]) {
    warning("Some hazard ratios or confidence limits are extreme or non-finite ",
            "(possible complete/quasi-complete separation in the Cox model); ",
            "the x-axis range has been clamped so the plot can be drawn.",
            call. = FALSE)
    rangeb <- c(max(rangeb[1], finite.range[1], na.rm = TRUE),
                min(rangeb[2], finite.range[2], na.rm = TRUE))
  }
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
    geom_point(data = toShowExpClean[!toShowExpClean$.nonfinite, , drop = FALSE],
      aes(x = .row, y = exp(estimate)), pch = 15, size = 4) +
    geom_errorbar(data = toShowExpClean[!toShowExpClean$.nonfinite, , drop = FALSE],
      aes(x = .row, ymin = exp(conf.low), ymax = exp(conf.high)), width = 0.15) +
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
      hjust = -0.2,  fontface = "italic")
  # Global statistics caption (# events, global p-value, AIC, concordance).
  # Optional: set global.stats = FALSE to omit it (e.g. panels of forest plots).
  # `broom::glance()$p.value.log` is the p-value of the *likelihood ratio test*
  # (survival stores it in `logtest`); the score/log-rank test is `p.value.sc`
  # (`sctest`). The caption previously mislabeled this value as "Log-Rank"; it now
  # names the test correctly. The value shown is unchanged (#640).
  if (global.stats)
    p <- p +
    annotate(geom = "text", x = 0.5, y = exp(y_variable),
      label = paste0("# Events: ", gmodel$nevent, "; Global p-value (Likelihood ratio test): ",
        format.pval(gmodel$p.value.log, eps = ".001"), " \nAIC: ", round(gmodel$AIC,2),
        "; Concordance Index: ", round(gmodel$concordance,2)),
      size = annot_size_mm, hjust = 0, vjust = 1.2,  fontface = "italic") +
    # The caption sits just below the first row (x = 0.5) and, in short plots, the
    # default axis expansion leaves too little room so the two-line caption is cut
    # off (#696). Reserve about one row-height of space below the last row (and a
    # little extra bottom margin) so the caption always fits, whatever the plot
    # height. Only applied when the caption is drawn, so global.stats = FALSE is
    # unchanged.
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.05), add = c(1, 0))) +
    theme(plot.margin = grid::unit(c(0.5, 0.5, 1, 0.5), "lines"))
  # switch off clipping for p-vals, bottom annotation:
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  # grid.draw(gt)
  # invisible(p)
  ggpubr::as_ggplot(gt)
}
