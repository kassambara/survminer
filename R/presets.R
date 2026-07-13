#' @include ggsurvtheme.R utilities.R
NULL

# Publication presets for ggsurvplot(preset = ...).
#
# Each preset is a named list of ggsurvplot() argument defaults. They are applied
# ONLY where the user did not pass the argument (see the resolver in
# ggsurvplot()), so an explicit user value always wins. The list is pure data and
# knows nothing about the return-mode / object assembly, so it is reused across
# every ggsurvplot dispatch path.
#
# An accessor function (rather than a top-level list) guarantees the theme
# constructors are defined before use and avoids any load-order coupling.
# Presets never carry a NULL value: to keep a survminer default, omit the key.
.survminer_presets <- function() {
  list(
    # Full evidence panel for a primary manuscript figure: confidence bands, the
    # log-rank p-value with its method label, the median-survival guide lines, a
    # number-at-risk table, a colourblind-safe palette and a clean classic theme.
    publication = list(
      conf.int          = TRUE,
      pval              = TRUE,
      pval.method       = TRUE,
      risk.table        = TRUE,
      surv.median.line  = "hv",
      palette           = "jco",
      surv.scale        = "percent",
      conf.int.alpha    = 0.15,
      risk.table.height = 0.25,
      tables.theme      = theme_cleantable(),
      ggtheme           = theme_surv_classic()
    ),
    # A clean, airy single panel for a supplement or a space-limited figure: no
    # risk table, no confidence bands, the p-value kept, a minimal theme.
    minimal = list(
      conf.int   = FALSE,
      pval       = TRUE,
      risk.table = FALSE,
      palette    = "jco",
      surv.scale = "percent",
      ggtheme    = theme_surv_minimal()
    ),
    # A textbook, methods-faithful look: grey step curves with "+" censor marks,
    # distinguished by line type as well as shade so they survive greyscale, the
    # log-rank p-value with its method label and a plain number-at-risk table.
    classic = list(
      conf.int     = FALSE,
      pval         = TRUE,
      pval.method  = TRUE,
      risk.table   = TRUE,
      palette      = "grey",
      linetype     = "strata",
      censor.shape = "+",
      tables.theme = theme_cleantable(),
      ggtheme      = theme_surv_classic()
    ),
    # Bold and legible for slides or a poster: large fonts and a big p-value via
    # the bold theme, no table clutter. (Line width is left at the survminer
    # default to avoid the ggplot2 `size`-vs-`linewidth` deprecation notice.)
    presentation = list(
      conf.int   = FALSE,
      pval       = TRUE,
      pval.size  = 7,
      risk.table = FALSE,
      palette    = "jco",
      surv.scale = "percent",
      ggtheme    = theme_surv_bold()
    )
  )
}

# Resolve a preset's defaults for a specific fit: start from the static list and
# apply the context-dependent tweaks. The strata count drives two adjustments:
# drop the median-survival guide lines when there are more than two strata (they
# clutter a multi-arm plot), and drop the p-value on a single-arm (null) model
# where there is nothing to compare (otherwise ggsurvplot warns and draws none).
# Robust to any fit type: on any doubt the static default is kept.
.resolve_preset <- function(preset, fit) {
  defs <- .survminer_presets()[[preset]]
  ns <- tryCatch(
    if (.is_survfit(fit) && !is.null(fit$strata)) length(fit$strata) else 1L,
    error = function(e) NA_integer_)
  if (!is.na(ns)) {
    if (ns > 2L && !is.null(defs$surv.median.line))
      defs$surv.median.line <- "none"
    if (ns <= 1L) {                      # nothing to compare on a null model
      if (!is.null(defs$pval))        defs$pval <- FALSE
      if (!is.null(defs$pval.method)) defs$pval.method <- FALSE
    }
  }
  defs
}
