# Fail the build on any ggplot2/lifecycle deprecation triggered by survminer's
# public plotting API. Meant to run against ggplot2-devel so we get lead time
# before a deprecation becomes a removal -- the package's main upstream-breakage
# risk.
#
# `lifecycle_verbosity = "error"` turns every deprecation into an error, so we do
# not depend on warnings propagating out of testthat (which captures them) or on
# ggplot2's imperfect "likely used in the <pkg>" attribution. We exercise the API
# directly and force a full ggplot_build() so draw-time deprecations surface too.
# A deprecation reached through a helper (e.g. `size` forwarded into
# ggpubr::geom_exec) throws here just the same, which is the class of bug this
# guard exists to catch. The API is deprecation-clean today; if a dependency
# later introduces one we cannot fix, waive that specific call here rather than
# dropping the guard.

options(lifecycle_verbosity = "error")
suppressPackageStartupMessages({
  library(survival)
  pkgload::load_all(".", quiet = TRUE)
})

set.seed(1)                              # keep the competing-risks case reproducible
lung2 <- lung
lung2$sex     <- factor(lung2$sex, labels = c("Male", "Female"))
lung2$ph.ecog <- factor(lung2$ph.ecog)
lung2 <- lung2[!is.na(lung2$ph.ecog) & lung2$ph.ecog %in% c("0", "1", "2"), ]
lung2$ph.ecog <- droplevels(lung2$ph.ecog)
km  <- survfit(Surv(time, status) ~ sex, data = lung2)
cox <- coxph(Surv(time, status) ~ sex + age + ph.ecog, data = lung2)

# each entry: a label and a thunk that returns a plot object (or a list of them).
# Aim to touch every exported plotting function, so a stray deprecated aesthetic
# anywhere in the drawing code is caught, not just in the curve engine.
cases <- list(
  "ggsurvplot"                = function() ggsurvplot(km, data = lung2),
  "ggsurvplot(size=)"         = function() ggsurvplot(km, data = lung2, size = 1.5),
  "ggsurvplot(conf.int,pval,risk.table)" =
                                function() ggsurvplot(km, data = lung2, conf.int = TRUE,
                                                      pval = TRUE, risk.table = TRUE,
                                                      ncensor.plot = TRUE),
  "ggsurvplot(fun=cumhaz)"    = function() ggsurvplot(km, data = lung2, fun = "cumhaz", size = 0.8),
  "ggsurvplot(fun=event)"     = function() ggsurvplot(km, data = lung2, fun = "event"),
  "ggsurvplot(ci step)"       = function() ggsurvplot(km, data = lung2, conf.int = TRUE,
                                                      conf.int.style = "step", size = 0.8),
  "ggsurvplot_facet"          = function() ggsurvplot_facet(km, data = lung2, facet.by = "sex"),
  "ggsurvplot_combine"        = function() ggsurvplot_combine(
                                  list(all = survfit(Surv(time, status) ~ 1, data = lung2),
                                       sex = km), data = lung2),
  "ggsurvtable"               = function() ggsurvtable(km, data = lung2),
  "ggrisktable"               = function() ggrisktable(km, data = lung2),
  "ggcumevents"               = function() ggcumevents(km, data = lung2),
  "ggcumcensor"               = function() ggcumcensor(km, data = lung2),
  "ggsurvevents"              = function() ggsurvevents(fit = km, data = lung2),
  "ggcoxzph"                  = function() ggcoxzph(cox.zph(cox)),
  "ggcoxdiagnostics"          = function() ggcoxdiagnostics(cox),
  "ggcoxfunctional"           = function() ggcoxfunctional(Surv(time, status) ~ age, data = lung2),
  "ggforest"                  = function() ggforest(cox, data = lung2),
  "ggforest_subgroup"         = function() ggforest_subgroup(cox, data = lung2,
                                             treatment = "sex", subgroups = "ph.ecog"),
  "ggadjustedcurves"          = function() ggadjustedcurves(cox, data = lung2,
                                                            method = "average", variable = "sex",
                                                            show.hr = TRUE),
  "ggrmst"                    = function() ggrmst(km, data = lung2, tau = 500),
  "ggcompetingrisks"          = function() {
      df <- data.frame(time = lung$time,
                       status = factor(ifelse(lung$status == 2, "death", "other"),
                                       levels = c("censored", "death", "other")))
      df$status[sample(nrow(df), 40)] <- "censored"
      ggcompetingrisks(survfit(Surv(time, status) ~ 1, data = df))
  }
)
if (requireNamespace("flexsurv", quietly = TRUE)) {
  cases[["ggflexsurvplot"]] <- function() {
    fit <- flexsurv::flexsurvreg(Surv(time, status) ~ sex, data = lung2, dist = "weibull")
    ggflexsurvplot(fit, data = lung2, conf.int.flex = TRUE)
  }
}

# Force drawing so build-time deprecations surface too. Descends into ggsurvplot
# objects (which hold several ggplots -- plot, table, ncensor) and into any list
# of ggplots (e.g. ggcoxzph returns one plot per covariate), building each.
force_build <- function(x) {
  if (inherits(x, "ggplot")) {
    invisible(ggplot2::ggplot_build(x))
  } else if (is.list(x) && !is.data.frame(x)) {
    for (el in x) force_build(el)
  }
  invisible(NULL)
}

failures <- character()
for (nm in names(cases)) {
  res <- tryCatch({ force_build(cases[[nm]]()); NULL },
                  error = function(e) conditionMessage(e))
  if (!is.null(res) && grepl("deprecat", res, ignore.case = TRUE)) {
    failures <- c(failures, sprintf("%s: %s", nm, gsub("[\r\n]+", " ", res)))
  } else if (!is.null(res)) {
    # a non-deprecation error (e.g. dev ggplot2 removed an API) is also a real
    # break we want to surface.
    failures <- c(failures, sprintf("%s [ERROR]: %s", nm, gsub("[\r\n]+", " ", res)))
  }
}

if (length(failures) > 0) {
  cat("::error::survminer plotting hit", length(failures), "deprecation(s)/break(s):\n")
  for (m in failures) cat(" - ", substr(m, 1, 240), "\n", sep = "")
  quit(status = 1)
}
cat("survminer plotting API is deprecation-clean under the current ggplot2. ✓\n")
