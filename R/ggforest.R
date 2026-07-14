#' Forest Plot for Cox Proportional Hazards Model
#'
#' @description Drawing Forest Plot for Cox proportional hazards model. In two panels the model structure is presented.
#' @param model an object of class coxph.
#' @param data a dataset used to fit survival curves. If not supplied then data
#'  will be extracted from 'fit' object.
#' @param main title of the plot.
#' @param cpositions relative positions of first three columns in the OX scale.
#' @param fontsize relative size of annotations in the plot. Default value: 0.7.
#' @param refLabel label for reference levels of factor variables. Either a single
#'  string (default \code{"reference"}, applied to every reference level) or a named
#'  character vector mapping a variable name to its reference label, e.g.
#'  \code{c(sex = "female (ref)", rx = "Obs (ref)")}; variables not named keep the
#'  default label. Match is on the model term name (before any \code{var.labels}
#'  relabelling).
#' @param palette the colour of the hazard-ratio points and confidence intervals.
#'  Default \code{NULL} draws them in black (unchanged). A single colour (name or
#'  hex) draws every point/interval in that one accent colour -- the recommended,
#'  tasteful choice, since colour on a forest plot does not otherwise carry meaning.
#'  Alternatively a palette -- a vector of colours or a palette name accepted by
#'  \code{\link[ggpubr]{get_palette}} (e.g. \code{"npg"}, \code{"lancet"}) --
#'  colours the points by variable (one colour per variable); no colour legend is
#'  drawn, as the rows are already labelled. A colour vector longer than the number
#'  of variables is interpolated (\code{get_palette} behaviour). Prefer a palette
#'  without grey (e.g. \code{"npg"}) so a point is not low-contrast on the shaded
#'  rows.
#' @param noDigits number of digits for estimates and p-values in the plot.
#' @param global.stats logical value. Default is TRUE. If FALSE, the bottom
#'   caption reporting the global statistics (number of events, global
#'   likelihood-ratio-test p-value, AIC and concordance index) is omitted. Useful
#'   when arranging several forest plots in a panel.
#' @param ref.display logical value. Default is TRUE. If FALSE, the rows that
#'   have no hazard ratio -- factor baselines, and any non-estimable / aliased or
#'   \code{strata()} terms, i.e. the rows labelled \code{refLabel} ("reference")
#'   -- are omitted from both the plot and the table, keeping only the estimated
#'   levels. Default TRUE shows every row (unchanged).
#' @param var.labels a named character vector giving display labels for the
#'   variable names, e.g. \code{c(age = "Age (years)", sex = "Sex")}. The names
#'   must match the model term names (as shown in the plot by default); only
#'   matched terms are relabelled, unmatched terms keep their original name.
#'   Default \code{NULL} uses the term names unchanged.
#' @param ggtheme a ggplot2 theme (e.g. the result of \code{theme(...)} or a
#'   complete theme) applied to the forest plot. Because \code{ggforest()}
#'   returns a rasterised \code{gtable}, a theme added to the returned object
#'   has no effect; pass it here instead, e.g.
#'   \code{ggtheme = theme(text = element_text(size = 14))}. It is added after
#'   the built-in theme: a partial \code{theme(...)} overrides only the matching
#'   elements, whereas a complete theme (e.g. \code{theme_void()}) replaces the
#'   built-in look entirely. Default \code{NULL} leaves the appearance unchanged.
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
#' # a single accent colour, and per-variable reference labels
#' ggforest(bigmodel, palette = "#0073C2",
#'   refLabel = c(sex = "female (ref)", rx = "Obs (ref)"))
#'
#' # a clean, publication-ready look: one accent colour and a slightly larger
#' # font. Add global.stats = FALSE when placing several forests in one figure.
#' ggforest(bigmodel, palette = "#0073C2", fontsize = 0.9)
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
  global.stats = TRUE, ref.display = TRUE, var.labels = NULL,
  palette = NULL, ggtheme = NULL) {
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

  # A variable that appears ONLY inside an interaction term (e.g. sex, rx in
  # `~ sex:rx` with no sex/rx main effect) is listed in dataClasses but has no
  # main-effect coefficient. Drawing its factor levels as "reference" rows is
  # misleading -- it implies a main effect that was never fit. Such variables are
  # skipped here; their coefficients are still drawn by the interaction block
  # below (mapped via model$assign). A variable that has its own model term
  # (main effect, incl. splines/transformations, whether or not it is also in an
  # interaction) is kept unchanged, so ordinary models are byte-identical (#594).
  .assign.names <- names(model$assign)
  .inter.vars <- unique(unlist(strsplit(
    grep(":", .assign.names, value = TRUE, fixed = TRUE), ":", fixed = TRUE)))
  .interaction_only <- function(v)
    !is.null(.assign.names) && !(v %in% .assign.names) && (v %in% .inter.vars)

  # Map each level of a factor/character term to the coefficient that represents
  # it under the contrasts the model ACTUALLY used, instead of assuming the
  # coefficient is named paste0(var, level). That assumption holds only for the
  # default contr.treatment (reference = first level). With a non-default base
  # (contr.treatment(base = k)) or other SAS-style contrasts, coxph names the
  # coefficients var + the contrast-matrix COLUMN names (level indices, e.g.
  # ph.ecog1/ph.ecog3/ph.ecog4 for base = 2 on levels 0/1/2/3), so the naive
  # paste0(var, level) match tags the wrong level as reference, puts each hazard
  # ratio on the wrong row, and silently drops the last coefficient (#404).
  # Returns a coefficient-match key per level (NA for the reference level), using
  # the stored contrast matrix (rows = levels, cols = coefficients) to route each
  # fitted coefficient to the level it belongs to. Falls back to the old
  # paste0(var, level) keys -- so the default contrasts stay byte-identical --
  # whenever the term is NOT a single-reference treatment-type contrast (e.g.
  # contr.helmert/contr.sum/contr.poly, for which a per-level reference row is not
  # well defined) or the contrast matrix cannot be reconciled with the fitted
  # coefficients.
  .factor_level_keys <- function(var, levs) {
    fallback <- paste0(var, levs)
    idx <- model$assign[[var]]
    if (is.null(idx)) {
      # A non-syntactic factor name (e.g. "risk grp") is stored backtick-quoted in
      # names(model$assign) but bare in dataClasses / model$contrasts, so the direct
      # [[var]] lookup misses it. Match on the backtick-stripped names so such a
      # factor is routed too, instead of dropping to the (mislabelling) fallback.
      pos <- which(gsub("`", "", names(model$assign)) == var)
      if (length(pos) == 1L) idx <- model$assign[[pos]]
    }
    if (is.null(idx)) return(fallback)
    # coef$term rownames are backtick-stripped below; strip here too so keys match
    cn <- gsub("`", "", coef$term[idx])         # coef names, in contrast-col order
    ctr <- model$contrasts[[var]]
    if (is.null(ctr)) return(fallback)
    f <- factor(levs, levels = levs)
    Cmat <- tryCatch({ stats::contrasts(f) <- ctr; stats::contrasts(f) },
                     error = function(e) NULL)
    if (is.null(Cmat) || nrow(Cmat) != length(levs) || ncol(Cmat) != length(cn))
      return(fallback)
    # treatment-type only: exactly one all-zero (reference) row AND every
    # coefficient column maps to exactly one level.
    if (sum(rowSums(Cmat != 0) == 0) != 1L || !all(colSums(Cmat != 0) == 1))
      return(fallback)
    keys <- rep(NA_character_, length(levs))
    for (j in seq_len(ncol(Cmat))) keys[which(Cmat[, j] != 0)] <- cn[j]
    keys
  }

  # extract statistics for every variable
  allTerms <- lapply(seq_along(terms), function(i){
    var <- names(terms)[i]
    if (.interaction_only(var)) return(NULL)
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
      # key = correct coefficient name per level (NA for the reference), resolved
      # from the model's actual contrasts so a non-default base is not mislabelled
      # (#404). For the default contr.treatment this equals paste0(var, level) for
      # the non-reference levels and NA for the reference, so output is unchanged.
      cbind(var = var, adf, pos = 1:nrow(adf),
            key = .factor_level_keys(var, as.character(adf$Var1)))
    }
    else if (terms[i] == "numeric") {
      data.frame(var = var, Var1 = "", Freq = nrow(data),
                 pos = 1, key = var)
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
      # key = the coefficient name itself (level is ""), matching the old
      # paste0(var, level) = vars key exactly, so these terms are byte-identical.
      data.frame(var = vars, Var1 = "", Freq = nrow(data),
                 pos = seq_along(vars), key = vars)
    }
  })
  # attr(model$terms, "dataClasses") lists only main-effect variables, so
  # interaction terms (e.g. sex:ph.ecog) were never visited by the loop above
  # and their coefficients were silently dropped from the plot and table (#536).
  # Add each interaction coefficient as its own row, mapped to the fitted
  # coefficients via model$assign (the same reliable mechanism used for
  # multi-coefficient terms above). Models with no interaction terms are
  # unaffected: .inter.terms is empty, so allTerms is unchanged.
  .inter.terms <- grep(":", names(model$assign), value = TRUE, fixed = TRUE)
  if (length(.inter.terms) > 0) {
    allTerms <- c(allTerms, lapply(.inter.terms, function(term){
      idx <- model$assign[[term]]
      vars <- coef$term[idx]
      data.frame(var = vars, Var1 = "", Freq = nrow(data), pos = seq_along(vars),
                 key = vars)
    }))
  }
  allTerms <- Filter(Negate(is.null), allTerms)   # drop interaction-only vars (#594)
  allTermsDF <- do.call(rbind, allTerms)
  colnames(allTermsDF) <- c("var", "level", "N", "pos", "key")
  # Match each row to its coefficient by the resolved key (contrast-aware for
  # factor levels, the coefficient name itself for numeric/spline/interaction
  # terms). A reference level has key = NA, which match() below leaves as NA (no
  # coefficient), i.e. the reference row -- identical to the old behaviour where a
  # reference level's paste0(var, level) simply found no matching coefficient (#404).
  inds <- as.character(allTermsDF$key)

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
  # Reference rows (factor baselines / non-estimable terms) have an NA estimate.
  # Capture the mask now, before `estimate` is zeroed below, so the reference
  # label and its text alignment key on this rather than a literal "reference"
  # string (which broke a custom refLabel).
  toShowExpClean$.isref <- is.na(toShowExpClean$estimate)
  # ref.display = FALSE drops the rows with no hazard ratio -- factor baselines,
  # plus any non-estimable / aliased or strata() terms -- from both the drawn
  # point and the table, keeping only the estimated levels. These are exactly the
  # rows with an NA estimate, i.e. the ones already labelled refLabel ("reference")
  # below, so the drop is self-consistent with the default display. A non-converged
  # (separation) coefficient is a large finite value, not NA, so no genuine
  # estimate is dropped. Default TRUE keeps every row -> existing plots unchanged (#563).
  if (!ref.display)
    toShowExpClean <- toShowExpClean[!is.na(toShowExpClean$estimate), , drop = FALSE]
  toShowExpClean$stars <- paste0(round(toShowExpClean$p.value, noDigits+1), " ",
    ifelse(toShowExpClean$p.value < 0.05, "*",""),
    ifelse(toShowExpClean$p.value < 0.01, "*",""),
    ifelse(toShowExpClean$p.value < 0.001, "*",""))
  toShowExpClean$ci <- paste0("(",toShowExpClean[,"conf.low.1"]," - ",toShowExpClean[,"conf.high.1"],")")
  # Reference label(s): a single string (default), or a named character vector
  # keyed by the model term name -- `var` still holds the raw term name here
  # (the var.labels remap runs below), so keying on it is clean.
  if (is.null(names(refLabel))) {
    toShowExpClean$estimate.1[toShowExpClean$.isref] <- refLabel[1]
  } else {
    lab <- refLabel[as.character(toShowExpClean$var[toShowExpClean$.isref])]
    lab[is.na(lab)] <- "reference"
    toShowExpClean$estimate.1[toShowExpClean$.isref] <- lab
  }
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
  # Optional user-supplied display labels for the variable names (#405). A named
  # character vector mapping model term name -> label (e.g.
  # c(age = "Age (years)", sex = "Sex")). Only names that match a term are
  # remapped; unmatched terms keep their original name, so the default
  # (var.labels = NULL) is unchanged.
  if (!is.null(var.labels)) {
    if (is.null(names(var.labels)))
      stop("`var.labels` must be a named character vector, e.g. ",
           "c(age = \"Age (years)\").", call. = FALSE)
    # Coerce to a plain named character vector (a named factor would otherwise
    # inject its integer codes; note `x[] <- as.character(x)` keeps a factor a
    # factor, so rebuild explicitly); ignore NA labels so a variable name is
    # never silently dropped.
    var.labels <- stats::setNames(as.character(var.labels), names(var.labels))
    .map <- var.labels[!is.na(var.labels)]
    .hit <- toShowExpClean$var %in% names(.map)
    toShowExpClean$var[.hit] <- .map[toShowExpClean$var[.hit]]
  }
  # Undeduplicated per-variable key for optional colouring (the `var` column below
  # is blanked for repeated levels, so it cannot be used as a colour group).
  toShowExpClean$.grp <- factor(toShowExpClean$var, levels = unique(toShowExpClean$var))
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

  # Hazard-ratio point + interval layers. Default (palette = NULL) keeps the exact
  # black, no-colour-aesthetic geoms so the output is unchanged; a palette colours
  # them by variable via a hidden manual colour scale. A single colour recycles to
  # every variable (one accent); a palette gives one colour per variable.
  .drawable <- toShowExpClean[!toShowExpClean$.nonfinite, , drop = FALSE]
  if (is.null(palette)) {
    .pt <- geom_point(data = .drawable, aes(x = .row, y = exp(estimate)),
                      pch = 15, size = 4)
    .eb <- geom_errorbar(data = .drawable,
                         aes(x = .row, ymin = exp(conf.low), ymax = exp(conf.high)),
                         width = 0.15)
    .col_scale <- NULL
  } else {
    .cols <- ggpubr::get_palette(palette, length(levels(toShowExpClean$.grp)))
    .cols <- stats::setNames(.cols, levels(toShowExpClean$.grp))
    .pt <- geom_point(data = .drawable,
                      aes(x = .row, y = exp(estimate), colour = .grp),
                      pch = 15, size = 4)
    .eb <- geom_errorbar(data = .drawable,
                         aes(x = .row, ymin = exp(conf.low), ymax = exp(conf.high),
                             colour = .grp), width = 0.15)
    .col_scale <- scale_colour_manual(values = .cols, guide = "none")
  }

  p <- ggplot(toShowExpClean, aes(seq_along(var), exp(estimate))) +
    geom_rect(aes(xmin = seq_along(var) - .5, xmax = seq_along(var) + .5,
      ymin = exp(rangeplot[1]), ymax = exp(rangeplot[2]),
      fill = ordered(seq_along(var) %% 2 + 1))) +
    scale_fill_manual(values = c("#FFFFFF33", "#00000033"), guide = "none") +
    .pt + .eb + .col_scale +
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
      vjust = ifelse(toShowExpClean$.isref, .5, -0.1)) +
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
  # Apply a user-supplied theme to the internal plot before it is rasterised
  # into a gtable. ggforest() returns ggpubr::as_ggplot(gtable), so a theme
  # added to the returned object cannot reach the inner text; passing it here
  # via `ggtheme` does (#530). Added last so it overrides the built-in theme.
  # Default NULL leaves the appearance unchanged.
  if (!is.null(ggtheme)) p <- p + ggtheme
  # switch off clipping for p-vals, bottom annotation:
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  # grid.draw(gt)
  # invisible(p)
  ggpubr::as_ggplot(gt)
}
