#' @include utilities.R
NULL
#' Adjusted Survival Curves for Cox Proportional Hazards Model
#' @importFrom tidyr gather
#' @importFrom dplyr summarise
#' @importFrom stats formula
#' @importFrom stats glm
#' @importFrom survival survexp
#' @importFrom dplyr group_by
#' @importFrom survival survfit
#' @description The function \code{surv_adjustedcurves()} calculates while the function \code{ggadjustedcurves()} plots adjusted survival curves for the \code{coxph} model.
#' The main idea behind this function is to present expected survival curves calculated based on Cox model separately for subpopulations. The very detailed description and interesting discussion of adjusted curves is presented in 'Adjusted Survival Curves' by Terry Therneau, Cynthia Crowson, Elizabeth Atkinson (2015) \code{https://cran.r-project.org/web/packages/survival/vignettes/adjcurve.pdf}.
#' Many approaches are discussed in this article. Currently four approaches (two unbalanced, one conditional and one marginal) are implemented in the \code{ggadjustedcurves()} function. See the section Details.
#' @details Currently four approaches are implemented in the \code{ggadjustedcurves()} function.
#'
#' For \code{method = "single"} a single survival curve is calculated and plotted. The curve presents an expected survival calculated for population \code{data} calculated based on the Cox model \code{fit}.
#'
#' For \code{method = "average"} a separate survival curve is plotted for each level of a variable listed as \code{variable}. If this argument is not specified, then it will be extracted from the \code{strata} component of \code{fit} argument.  Each curve presents an expected survival calculated for subpopulation from \code{data} based on a Cox model \code{fit}. Note that in this method subpopulations are NOT balanced.
#'
#' For \code{method = "marginal"} a survival curve is plotted for each level of a grouping variable selected by \code{variable} argument. If this argument is not specified, then it will be extracted from the \code{strata} component of \code{fit} object.  Subpopulations are balanced with respect to variables in the \code{fit} formula to keep distributions similar to these in the \code{reference} population. If no reference population is specified, then the whole \code{data} is used as a reference population instead. The balancing is performed in a following way: (1) for each subpopulation a logistic regression model is created to model the odds of being in the subpopulation against the reference population given the other variables listed in a \code{fit} object, (2) reverse probabilities of belonging to a specified subpopulation are used as weights in the Cox model, (3) the Cox model is refitted with weights taken into account, (4) expected survival curves are calculated for each subpopulation based on a refitted weighted model.
#'
#' For \code{method = "conditional"} a separate survival curve is plotted for each level of a grouping variable selected by \code{variable} argument. If this argument is not specified, then it will be extracted from the \code{strata} component of \code{fit} object.  Subpopulations are balanced in a following way: (1) the data is replicated as many times as many subpopulations are considered (say k), (2) for each row in original data a set of k copies are created and for every copy a different value of a grouping variable is assigned, this will create a new dataset balanced in terms of grouping variables, (3) expected survival is calculated for each subpopulation based on the new artificial dataset. Here the model \code{fit} is not refitted.
#'
#' Note that \code{surv_adjustedcurves} function calculates survival curves and based on this function one can calculate median survival.
#'
#'@inheritParams ggsurvplot_arguments
#'@param fit an object of class \link[survival]{coxph.object} - created with \link[survival]{coxph} function.
#'@param data a dataset for predictions. If not supplied then data will be extracted from the \code{fit} object.
#'@param reference a dataset for reference population, to which dependent variables should be balanced. If not specified, then the \code{data} will be used instead. Note that the \code{reference} dataset should contain all variables used in \code{fit} object.
#'@param method a character, describes how the expected survival curves shall be calculated. Possible options:
#' 'single' (average for population), 'average' (averages for subpopulations), 'marginal', 'conditional' (averages for subpopulations after rebalancing). See the Details section  for further information.
#'@param variable a character, name of the grouping variable to be plotted. If not supplied then it will be extracted from the model formula from the \code{strata()} component. If there is no \code{strata()} component then only a single curve will be plotted - average for the thole population.
#'@param fun an arbitrary function defining a transformation of the survival
#'  curve. Often used transformations can be specified with a character argument:
#'  "event" plots cumulative events (f(y) = 1-y), "cumhaz" plots the cumulative
#'  hazard function (f(y) = -log(y)), and "pct" for survival probability in
#'  percentage. For \code{surv_adjustedcurves()} the transformation is applied to
#'  the returned survival column; the default \code{NULL} leaves the survival
#'  probabilities unchanged.
#'@param ylab a label for oy axis.
#'@param size the curve size.
#'@param ggtheme function, ggplot2 theme name. Allowed values include ggplot2 official themes: see \code{\link[ggplot2]{theme}}.
#'@param risk.table logical value (or a valid \code{\link{ggsurvplot}()} risk-table
#'  string). Default is FALSE. If not FALSE, a number-at-risk table is drawn below
#'  the adjusted curves and the function returns a \code{\link{ggsurvplot}} object
#'  (printed with \code{print()}), instead of the plain \code{ggplot} returned by
#'  default. The table is the \strong{Kaplan-Meier} number at risk \emph{by the
#'  grouping} \code{variable}: the adjusted curves are model-based expectations for
#'  covariate profiles and have no literal number at risk of their own, so this
#'  table is meaningful only when \code{variable} is a real subgroup. It is computed
#'  on the rows the Cox model used (its complete cases), so its baseline count
#'  matches \code{fit$n}. For \code{method = "marginal"}/\code{"conditional"} the
#'  curves are balanced/reweighted while this count is the raw Kaplan-Meier number
#'  at risk. Note that when a table is drawn the return value is a \code{ggsurvplot}
#'  object, so post-hoc \code{+ theme()}/\code{+ scale_*} no longer apply -- use the
#'  arguments below (\code{xlim}, \code{break.time.by}, \code{ylab}, \code{palette},
#'  \code{ggtheme}) instead.
#'@param risk.table.height the height of the risk table relative to the plot when
#'  \code{risk.table} is drawn. Default is 0.25.
#'@param risk.table.title the risk-table title. Default \code{NULL} uses
#'  "Number at risk (Kaplan-Meier, by <variable>)".
#'@param break.time.by,xlim numeric time-axis break interval and axis limits,
#'  applied to \emph{both} the curves and the risk table so the two panels line up.
#'  Defaults \code{NULL} use the automatic breaks and the data's time range.
#'@param tables.y.text logical. Default TRUE. If FALSE, the risk-table strata tick
#'  labels are hidden.
#'@param tables.theme,fontsize the theme and font size of the risk table, as in
#'  \code{\link{ggsurvplot}()}.
#'@inheritParams ggpubr::ggpar
#'@param ... further arguments passed to the function \code{\link[ggpubr]{ggpar}} for customizing the plot.
#'@return Returns an object of class \code{gg} (a \code{ggplot}); when
#'  \code{risk.table} is not FALSE, returns a \code{\link{ggsurvplot}} object
#'  (curves + aligned number-at-risk table) instead.
#'
#'@author Przemyslaw Biecek, \email{przemyslaw.biecek@@gmail.com}
#'
#'@examples
#'
#' library(survival)
#' fit2 <- coxph( Surv(stop, event) ~ size, data = bladder )
#' # single curve
#' ggadjustedcurves(fit2, data = bladder)
#' curve <- surv_adjustedcurves(fit2, data = bladder)
#'
#' fit2 <- coxph( Surv(stop, event) ~ size + strata(rx), data = bladder )
#' # average in groups
#' ggadjustedcurves(fit2, data = bladder, method = "average", variable = "rx")
#' curve <- surv_adjustedcurves(fit2, data = bladder, method = "average", variable = "rx")
#'
#' # conditional balancing in groups
#' ggadjustedcurves(fit2, data = bladder, method = "marginal", variable = "rx")
#' curve <- surv_adjustedcurves(fit2, data = bladder, method = "marginal", variable = "rx")
#'
#' # selected reference population
#' ggadjustedcurves(fit2, data = bladder, method = "marginal", variable = "rx",
#'     reference = bladder[bladder$rx == "1",])
#'
#' # conditional balancing in groups
#' ggadjustedcurves(fit2, data = bladder, method = "conditional", variable = "rx")
#' curve <- surv_adjustedcurves(fit2, data = bladder, method = "conditional", variable = "rx")
#'
#' # With a Kaplan-Meier number-at-risk table by the grouping variable
#' #:::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' ggadjustedcurves(fit2, data = bladder, method = "average", variable = "rx",
#'                  risk.table = TRUE)
#'
#'\dontrun{
#' # this will take some time
#' fdata <- flchain[flchain$futime >=7,]
#' fdata$age2 <- cut(fdata$age, c(0,54, 59,64, 69,74,79, 89, 110),
#'                   labels = c(paste(c(50,55,60,65,70,75,80),
#'                                    c(54,59,64,69,74,79,89), sep='-'), "90+"))
#' fdata$group <- factor(1+ 1*(fdata$flc.grp >7) + 1*(fdata$flc.grp >9),
#'                       levels=1:3,
#'                       labels=c("FLC < 3.38", "3.38 - 4.71", "FLC > 4.71"))
#' # single curve
#' fit <- coxph( Surv(futime, death) ~ age*sex, data = fdata)
#' ggadjustedcurves(fit, data = fdata, method = "single")
#'
#' # average in groups
#' fit <- coxph( Surv(futime, death) ~ age*sex + strata(group), data = fdata)
#' ggadjustedcurves(fit, data = fdata, method = "average")
#'
#' # conditional balancing in groups
#' ggadjustedcurves(fit, data = fdata, method = "conditional")
#'
#' # marginal balancing in groups
#' ggadjustedcurves(fit, data = fdata, method = "marginal", reference = fdata)
#' }
#'
#' @rdname ggadjustedcurves
#' @export
ggadjustedcurves <- function(fit,
                                variable = NULL,
                                data = NULL,
                                reference = NULL,
                                method = "conditional",
                                fun = NULL,
                                palette = "hue",
                                ylab = "Survival rate", size = 1,
                                ggtheme = theme_survminer(),
                                risk.table = FALSE,
                                risk.table.height = 0.25,
                                risk.table.title = NULL,
                                break.time.by = NULL,
                                xlim = NULL,
                                tables.y.text = TRUE,
                                tables.theme = theme_survminer(),
                                fontsize = 4.5, ...) {
  stopifnot(method %in% c("marginal", "average", "conditional", "single"))
  draw.table <- !is.null(risk.table) && !isFALSE(risk.table) && !identical(risk.table, "")

  ylim <- NULL
  if (is.null(fun)) ylim <- c(0, 1)

  curve <- surv_adjustedcurves(fit = fit,
                               variable = variable,
                               data = data,
                               reference = reference,
                               method = method,
                               size = size,
                               ...)
  # Apply the transformation requested via `fun` (e.g. "event", "cumhaz",
  # "pct"). Previously `fun` only affected the y-axis limits and the plotted
  # curve was always the raw survival probability (#287, #498, #630, #660).
  # This is a no-op when fun = NULL (the default), so ordinary calls are
  # unchanged.
  curve <- .apply_surv_func(curve, fun = fun)
  # Relabel the y-axis to match `fun` when the user kept the default ylab
  # (#555). ggsurvplot() already does this; ggadjustedcurves() did not, so a
  # `fun = "cumhaz"` curve kept the "Survival rate" label. Only fires for the
  # untouched default label AND a character `fun`, so fun = NULL (the default)
  # and any user-supplied ylab are unchanged.
  if (identical(ylab, "Survival rate") && is.character(fun)) {
    ylab <- switch(fun,
                   log     = "log(Survival rate)",
                   event   = "Cumulative event",
                   cumhaz  = "Cumulative hazard",
                   cloglog = "log(-log(S(t)))",
                   pct     = "Survival rate (%)",
                   logpct  = "Survival rate (%)",
                   "Survival rate")   # identity / unrecognized -> keep default
  }
  time <- surv <- NULL
  pl <- ggplot(curve, aes(x = time, y = surv, color = variable)) +
    geom_step(linewidth = size) + ggtheme +
    scale_y_continuous(limits = ylim) +
    ylab(ylab)

  if (method == "single")
    pl <- pl + theme(legend.position = "none")

  # Default (no risk table): return the plain adjusted-curves ggplot, unchanged.
  # `xlim` is only forwarded to ggpar() when the user supplied it, so an ordinary
  # call (xlim = NULL) is byte-identical to before.
  if (!draw.table) {
    if (is.null(xlim)) return(ggpubr::ggpar(pl, palette = palette, ...))
    return(ggpubr::ggpar(pl, palette = palette, xlim = xlim, ...))
  }

  # ---- Kaplan-Meier number-at-risk table under the adjusted curves (#286) ----
  # The adjusted curves are model-based expectations for covariate profiles, so
  # they have no literal number at risk of their own. The honest object is the
  # Kaplan-Meier number at risk BY THE GROUPING VARIABLE, which is only meaningful
  # when that variable is a real subgroup (the same caveat as the vignette recipe).
  # We therefore:
  #  * build the KM table on the SAME rows the Cox model used (its complete cases),
  #    so the table's baseline n equals fit$n rather than over-counting rows the
  #    model dropped for missing covariates;
  #  * force a shared x scale (xlim / breaks) on the curves and the table so the
  #    two panels register on time;
  #  * reuse ggsurvplot()/print.ggsurvplot() for the aligned stacking, height and
  #    save methods, swapping the KM curve for the adjusted-curves plot.
  # For method = "marginal"/"conditional" the curves are balanced/reweighted while
  # this count is the raw Kaplan-Meier number at risk (documented).
  adj.plot <- ggpubr::ggpar(pl, palette = palette, ...)

  # Resolve the grouping variable exactly as surv_adjustedcurves() does: the
  # argument, else a strata() term; a model with neither gives a single curve and
  # an overall (~ 1) table.
  .var <- variable
  if (is.null(.var)) {
    .tl <- attr(stats::terms(fit$formula), "term.labels")
    .st <- grep("strata(", .tl, fixed = TRUE, value = TRUE)
    if (length(.st) > 0)
      .var <- gsub("[\\) ]", "",
                   gsub("strata(", "", .st[1], fixed = TRUE))
  }

  # Restrict the KM fit to the model's rows (complete cases on the model variables)
  # so the number at risk describes the population the adjusted curves are built on.
  .data <- as.data.frame(.get_data(fit, data))
  .mvars <- intersect(all.vars(stats::formula(fit)), names(.data))
  .cc <- .data[stats::complete.cases(.data[, .mvars, drop = FALSE]), , drop = FALSE]

  # (A grouping `variable` that is not a usable column has already errored in
  # surv_adjustedcurves() above, so the curves never build -- no guard needed here.)

  # Kaplan-Meier fit for the number-at-risk table, using the model's response.
  .resp <- stats::formula(fit)[[2]]
  .kmformula <- stats::as.formula(paste(deparse(.resp, width.cutoff = 500L), "~",
                                        if (is.null(.var)) "1" else .var))
  # Use survminer's surv_fit() (not survival::survfit()) so the fit retains its
  # formula/data for ggsurvplot()'s downstream extraction.
  .kmfit <- surv_fit(.kmformula, data = .cc)

  # Labels matching the adjusted-curve groups (strip the "var=" strata prefix), in
  # the same order as the curve colours (sorted levels).
  .labs <- if (is.null(.var)) NULL else as.character(sort(unique(.cc[[.var]])))
  # Shared x scale: the union time range, or the user-supplied xlim.
  .xmax <- if (!is.null(xlim)) max(xlim) else max(.kmfit$time, na.rm = TRUE)
  .xlim <- if (!is.null(xlim)) xlim else c(0, .xmax)
  # Make the honesty explicit on the figure itself.
  .rt.title <- risk.table.title
  if (is.null(.rt.title))
    .rt.title <- if (is.null(.var)) "Number at risk (Kaplan-Meier)"
                 else paste0("Number at risk (Kaplan-Meier, by ", .var, ")")

  # Build a KM ggsurvplot (aligned curve + number-at-risk table), then swap in the
  # adjusted-curves plot. Messages from the KM build are silenced (the visible
  # output is the adjusted plot, and any diagnostic there was already surfaced when
  # the curves were computed above).
  km.args <- list(fit = .kmfit, data = .cc, risk.table = risk.table,
                  palette = palette, xlim = .xlim,
                  risk.table.title = .rt.title, risk.table.height = risk.table.height,
                  tables.y.text = tables.y.text, tables.theme = tables.theme,
                  fontsize = fontsize)
  if (!is.null(.labs)) km.args$legend.labs <- .labs
  if (!is.null(break.time.by)) km.args$break.time.by <- break.time.by
  res <- suppressMessages(do.call(ggsurvplot, km.args))

  # Align the adjusted plot to the same x scale as the table and swap it in.
  adj.plot <- adj.plot + ggplot2::coord_cartesian(xlim = .xlim)
  if (!is.null(break.time.by))
    adj.plot <- adj.plot +
      ggplot2::scale_x_continuous(breaks = seq(0, .xmax, by = break.time.by))
  res$plot <- adj.plot
  res
}

#' @rdname ggadjustedcurves
#' @export
surv_adjustedcurves <- function(fit,
                             variable = NULL,
                             data = NULL,
                             reference = NULL,
                             method = "conditional",
                             fun = NULL,
                             size = 1,
                             ...) {
  stopifnot(method %in% c("marginal", "average", "conditional", "single"))
  data <- .get_data(fit, data)
  # Coerce to a plain data.frame: the curve helpers index columns with
  # data[, variable], which returns a one-column tibble (not a vector) for a
  # tibble input and then fails in sort()/unique() with "cannot xtfrm data
  # frame". A no-op for a data.frame, so existing results are unchanged.
  data <- as.data.frame(data)
  # deal with default arguments
  # reference = NULL
  if (is.null(reference))
    reference <- data
  # coerce a user-supplied reference too: the marginal method indexes
  # reference[, variable] and rbind()s it with data, so a tibble reference
  # would re-introduce the "cannot xtfrm data frame" error.
  reference <- as.data.frame(reference)

  # variable = NULL
  if (is.null(variable)) {
    # is there a 'strata' component?
    term.labels <- attr(terms(fit$formula), "term.labels")
    strata.term.labels <- grep(term.labels, pattern = "strata(", fixed = TRUE, value = TRUE)
    if (length(strata.term.labels) > 0) {
      variable <- gsub(
        gsub(
          strata.term.labels,
          pattern = "strata(", replacement = "", fixed = TRUE)[1],
        pattern = "[\\) ]", replacement = "")
      cat("The variable argument is missing. Using", variable, "as extracted from strata\n")
    } else {
      # if not then leave variable = NULL
      method = "single"
    }
  }

  # With the default method = "conditional", the curves are built from the
  # fitted model's predictions. If the grouping `variable` is NOT in the Cox
  # model it never enters the linear predictor, so every group gets an identical
  # curve (a single visible line) with no error -- a common source of confusion
  # (#623). Warn and point to method = "average"/"marginal", which are the
  # methods meant for a grouping variable absent from the model. Message-only:
  # the returned curves are unchanged. Not triggered for "average"/"marginal"/
  # "single", nor when the variable is in the model (incl. via a transform such
  # as strata()/ns(), which all.vars() still resolves to the underlying name).
  if (method == "conditional" && !is.null(variable) &&
      !(variable %in% all.vars(stats::formula(fit))))
    warning("method = \"conditional\" (the default) builds each group's curve ",
            "from the Cox model, but the grouping variable '", variable,
            "' is not in the model, so the curves for all groups are identical. ",
            "Use method = \"average\" or \"marginal\" to draw distinct curves for ",
            "a variable that is not in the model, or add '", variable,
            "' to the Cox model.", call. = FALSE)

  curve <- switch(method,
                  single = ggadjustedcurves.single(data, fit, size = size),
                  average =  ggadjustedcurves.average(data, fit, variable, size = size),
                  conditional = ggadjustedcurves.conditional(data, fit, variable, size = size),
                  marginal = ggadjustedcurves.marginal(data, fit, variable, reference, size = size))

  # Apply the transformation requested via `fun` (e.g. "event", "cumhaz", "pct")
  # to the returned survival column, so the data helper matches what
  # ggadjustedcurves() plots. No-op when fun = NULL (the default), so existing
  # results are unchanged (#630). ggadjustedcurves() keeps fun in its own scope
  # and does not pass it here, so the transform is never applied twice.
  curve <- .apply_surv_func(curve, fun = fun)
  curve
}


ggadjustedcurves.single <- function(data, fit, size = 1) {
  time <- surv <- variable <- NULL

  pred <- survexp(~1, data = data, ratetable = fit)

  curve <- data.frame(time = c(0,pred$time),
                      variable = "total",
                      surv = c(1, pred$surv))

  # plot moved to ggadjustedcurves
  # ggplot(curve, aes(x = time, y = surv, color = variable)) +
  #   geom_step(size = size) + theme(legend.position = "none")
  curve
}

ggadjustedcurves.average <- function(data, fit, variable, size = 1) {
  time <- surv <- NULL

  lev <- sort(unique(data[,variable]))
  pred <- survexp(as.formula(paste("~", variable)), data = data,
                  ratetable = fit)

  curve <- data.frame(time = rep(c(0,pred$time), length(lev)),
                      variable = factor(rep(lev, each=1+length(pred$time))),
                      surv = c(rbind(1, pred$surv)))

  # plot moved to ggadjustedcurves
  # ggplot(curve, aes(x = time, y = surv, color=variable)) +
  #   geom_step(size = size)
  curve
}

ggadjustedcurves.marginal <- function(data, fit, variable, reference, size = 1) {
  time <- surv <- NULL

  lev <- sort(unique(data[,variable]))
  reference[,variable] = "_reference_"
  df0 <- reference
  form <- paste(variable, "~", gsub(as.character(formula(fit))[3], pattern="\\+ *strata.*[^\\)].", replacement=""))

  allRes <- list()
  rwt <- numeric(nrow(data))
  for (level in lev) {
    indexes <- which(data[,variable] == level)
    if (length(indexes) > 0) {
      df1 <- data[indexes, ]
      ndf <- rbind(df0, df1)
      ndf[,variable] <- factor(ndf[,variable])
      model <- glm(as.formula(form), ndf, family="binomial")
      allRes[[level]] <- predict(model, newdata = data, type = "response")
      rwt[indexes] <- 1/allRes[[level]][indexes]
    }
  }

  nform <- paste(as.character(formula(fit))[2], "~", variable)
  nfit <- coxph(as.formula(nform), data = data, weights = rwt)

  pred <- survexp(as.formula(paste("~", variable)), data = data, ratetable = nfit)

  # remove leading zeros
  # while survexp returns non monotonic results
  if (length(dim(pred$surv))==2) {
    for (i in 1:ncol(pred$surv))
      for (j in nrow(pred$surv):2)
        if (pred$surv[j,i] > pred$surv[j - 1,i])
          pred$surv[j - 1,i] <- 1
  }

  curve <- data.frame(time = rep(c(0,pred$time), length(lev)),
                      variable = factor(rep(lev, each=1+length(pred$time))),
                      surv = c(rbind(1, pred$surv)))

  # plot moved to ggadjustedcurves
  # ggplot(curve, aes(x = time, y = surv, color=variable)) +
  #   geom_step(size = size)
  curve
}

ggadjustedcurves.conditional <- function(data, fit, variable, size = 1) {
  time <- surv <- NULL

  lev <- sort(unique(data[,variable]))
  ndata <- data[rep(1:nrow(data), each=length(lev)),
                setdiff(colnames(data), variable)]
  ndata[,variable] = rep(lev, nrow(data))

  pred <- survexp(as.formula(paste("~", variable)), data = ndata,
                  ratetable = fit)
  # remove leading zeros
  # while survexp returns non monotonic results
  if (length(dim(pred$surv)) == 2) {
    for (i in 1:ncol(pred$surv))
      for (j in nrow(pred$surv):2)
        if (pred$surv[j,i] > pred$surv[j - 1,i])
          pred$surv[j - 1,i] <- 1
  }

  curve <- data.frame(time = rep(c(0,pred$time), length(lev)),
                      variable = factor(rep(lev, each=1+length(pred$time))),
                      surv = c(rbind(1, pred$surv)))
  # plot moved to ggadjustedcurves
  # ggplot(curve, aes(x = time, y = surv, color=variable)) +
  #   geom_step(size = size)
  curve
}

