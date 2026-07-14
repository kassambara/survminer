#' @include ggsurvplot_core.R
NULL
#'Facet Survival Curves into Multiple Panels
#'
#'@description Draw multi-panel survival curves of a data set grouped by one or
#'  two variables.
#'@inheritParams ggsurvplot_arguments
#'@param pval logical value. If TRUE, a log-rank p-value is computed and
#'  displayed for each panel. Note that, unlike \code{\link{ggsurvplot}()},
#'  a numeric or character \code{pval} is not substituted here (there is one
#'  p-value per panel); such a value is ignored with a warning.
#'@param p.adjust.method method for adjusting the per-panel p-values for multiple
#'  comparisons across panels, passed to \code{\link[stats]{p.adjust}()} (e.g.
#'  \code{"BH"}, \code{"bonferroni"}, \code{"holm"}). The default \code{"none"}
#'  shows the raw per-panel p-values. When an adjustment method is used the
#'  displayed text is prefixed with \code{p.adjust.label}.
#'@param p.adjust.label the label placed before the adjusted p-value when
#'  \code{p.adjust.method} is not \code{"none"}. The default is \code{"adj.p ="}
#'  (e.g. \code{"adj.p = 0.03"}); set it to customise the wording, e.g.
#'  \code{"p.adj ="} or \code{"q ="}. For a very small p-value the label's
#'  trailing \code{"="} is replaced by \code{"<"} (\code{"adj.p < 0.0001"}).
#'@param facet.by character vector, of length 1 or 2, specifying grouping
#'  variables for faceting the plot. Should be in the data.
#'@param nrow,ncol Number of rows and columns in the pannel. Used only when the
#'  data is faceted by one grouping variable.
#'@param scales should axis scales of panels be fixed ("fixed", the default),
#'  free ("free"), or free in one dimension ("free_x", "free_y").
#'@param short.panel.labs logical value. Default is FALSE. If TRUE, create short
#'  labels for panels by omitting variable names; in other words panels will be
#'  labelled only by variable grouping levels.
#'@param panel.labs a list of one or two character vectors to modify facet label
#'  text. For example, panel.labs = list(sex = c("Male", "Female")) specifies
#'  the labels for the "sex" variable. For two grouping variables, you can use
#'  for example panel.labs = list(sex = c("Male", "Female"), rx = c("Obs",
#'  "Lev", "Lev2") ).
#'@param labeller a labeller function/specification, as in
#'  \code{\link[ggplot2]{facet_wrap}} (e.g. \code{ggplot2::label_both} or
#'  \code{ggplot2::as_labeller(c(...))}), controlling how the panel strip labels
#'  are formatted. Default \code{NULL} keeps the current labels. When supplied it
#'  takes precedence over \code{short.panel.labs}; it composes with
#'  \code{panel.labs} (which renames the underlying data levels, so the labeller
#'  sees the renamed values).
#'@param panel.labs.background a list to customize the background of panel
#'  labels. Should contain the combination of the following elements: \itemize{
#'  \item \code{color, linetype, size}: background line color, type and size
#'  \item \code{fill}: background fill color. } For example,
#'  panel.labs.background = list(color = "blue", fill = "pink").
#'@param panel.labs.font a list of aestheics indicating the size (e.g.: 14), the
#'  face/style (e.g.: "plain", "bold", "italic", "bold.italic") and the color
#'  (e.g.: "red") and the orientation angle (e.g.: 45) of panel labels.
#'@param panel.labs.font.x,panel.labs.font.y same as panel.labs.font but for x
#'  and y direction, respectively.
#'@param pval.in.label logical value. Default is FALSE. If TRUE, the per-panel
#'  p-value is appended to the panel strip label instead of being drawn inside the
#'  panel. Requires \code{pval = TRUE} and a single \code{facet.by} variable (with
#'  two faceting variables the p-value is drawn on the panels instead, with a
#'  warning). If a \code{labeller} is also supplied it takes precedence and the
#'  p-value is not added to the label.
#'@param risk.table logical value. Default is FALSE. If TRUE, a number-at-risk
#'  table is drawn below the faceted curves, faceted with the same structure so
#'  each panel's table aligns under its curves. Supported for a single
#'  \code{facet.by} variable; with two it warns and returns the plot without a
#'  table (a two-way \code{facet_grid} shares one y axis per row, so the per-panel
#'  strata cannot be labelled correctly). When \code{risk.table} is not FALSE the
#'  function returns an aligned \code{gtable} (printed with \code{print()}); the
#'  default (FALSE) returns the faceted ggplot unchanged. In the faceted table the
#'  strata rows are identified by their text labels in a single colour (not by the
#'  curve colours), because a panel that drops a stratum would otherwise recolour
#'  the remaining rows by position and mislabel them. The table rows are labelled by
#'  the grouping variable's own values; \code{legend.labs} relabels the curve legend
#'  but not the table rows.
#'@param risk.table.height,surv.plot.height the relative heights of the risk table
#'  and the survival plot when \code{risk.table} is drawn. Defaults are 0.25 and
#'  0.75.
#'@param tables.y.text logical value. Default is TRUE. In a faceted risk table the
#'  rows are identified by their text labels (the curve colours are not a reliable
#'  identifier across facets), so \code{FALSE} is not supported here: the labels are
#'  kept and a warning is issued.
#'@param tables.col,tables.theme,fontsize colour, theme and font size of the risk
#'  table, as in \code{\link{ggsurvplot}}.
#'@param ... other arguments to pass to the function \code{\link{ggsurvplot}}.
#' @examples
#' library(survival)
#'
#'# Facet by one grouping variables: rx
#'#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'fit <- survfit( Surv(time, status) ~ sex, data = colon )
#'ggsurvplot_facet(fit, colon, facet.by = "rx",
#'                 palette = "jco", pval = TRUE)
#'
#'# Facet by two grouping variables: rx and adhere
#'#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'ggsurvplot_facet(fit, colon, facet.by = c("rx", "adhere"),
#'                 palette = "jco", pval = TRUE)
#'
#'
#'# P-value in the panel labels (single faceting variable)
#'#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'ggsurvplot_facet(fit, colon, facet.by = "rx",
#'                 palette = "jco", pval = TRUE, pval.in.label = TRUE)
#'
#'# Faceted number-at-risk table
#'#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'ggsurvplot_facet(fit, colon, facet.by = "rx",
#'                 palette = "jco", risk.table = TRUE)
#'
#'# Another fit
#'#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'fit2 <- survfit( Surv(time, status) ~ sex + rx, data = colon )
#'ggsurvplot_facet(fit2, colon, facet.by = "adhere",
#'                 palette = "jco", pval = TRUE)
#'
#'@export
ggsurvplot_facet <- function(fit, data, facet.by,
                             color = NULL, palette = NULL,
                             legend.labs = NULL,
                             pval = FALSE, pval.method = FALSE, pval.size = 5,
                             pval.coord = NULL, pval.method.coord = NULL,
                             pval.in.label = FALSE,
                             p.adjust.method = "none", p.adjust.label = "adj.p =",
                             nrow = NULL, ncol = NULL,
                             scales = "fixed",
                             short.panel.labs = FALSE, panel.labs = NULL,
                             labeller = NULL,
                             panel.labs.background = list(color = NULL, fill = NULL),
                             panel.labs.font = list(face = NULL, color = NULL, size = NULL, angle = NULL),
                             panel.labs.font.x = panel.labs.font,
                             panel.labs.font.y = panel.labs.font,
                             risk.table = FALSE,
                             risk.table.height = 0.25, surv.plot.height = 0.75,
                             tables.y.text = TRUE, tables.col = "black",
                             tables.theme = theme_survminer(), fontsize = 4.5,
                             ...)
  {

  if(length(facet.by) > 2)
    stop("facet.by should be of length 1 or 2.")
  p.adjust.method <- match.arg(p.adjust.method, stats::p.adjust.methods)

  if(!is.null(panel.labs) & !.is_list(panel.labs))
      stop("Argument panel.labs should be a list. Read the documentation.")

  . <- NULL # used in pipes

  .labeller <- "label_value"
  if(short.panel.labs) .labeller <- label_both
  .dots <- list(...)

  # Extract fit components
  #:::::::::::::::::::::::::::::::::::::::::
  fit.ext <- .extract.survfit(fit, data)
  .formula <- fit.ext$formula
  surv.obj <- fit.ext$surv
  surv.vars <- fit.ext$variables
  all.variables <- c(surv.vars, facet.by) %>%
    unique()
  vars.notin.groupby <- setdiff(all.variables, facet.by)
  # Coerce to a plain data.frame so single-column extractions like
  # data[, .grouping.var] return a vector: for a tibble they return a
  # one-column tibble, so as.factor() below fails with "cannot xtfrm data
  # frames" (#591; same tibble gotcha as #548/#670).
  data <- as.data.frame(fit.ext$data.all)

  # Will the curves be refit below? They are refit to add faceting variables not in
  # the survfit formula, to relabel panels, or to build the ".strata." grouping.
  # When no refit happens the original (already-weighted) fit is kept, so weights
  # need no special handling.
  .will.refit <- (!all(facet.by %in% surv.vars)) || !is.null(panel.labs) ||
    (length(vars.notin.groupby) > 1)

  # Recover the fit's case weights for that refit. The refit dropped the original
  # weights, so faceted curves and the surv.median.line were computed UNWEIGHTED --
  # silently wrong (#556). We only recover weights supplied as a BARE COLUMN of
  # `data` (e.g. `weights = w`): such a column is, by construction, row-aligned with
  # the data the refit uses, so it can be forwarded safely. Weights given as an
  # expression or external object (e.g. `weights = df$w`) cannot be guaranteed
  # aligned to a possibly-reordered `data`, so rather than risk a SILENT
  # misweighting we fall back to the unweighted refit and warn. Unweighted fits are
  # byte-identical.
  .weights <- NULL
  if(!is.null(fit$call$weights)){
    w.sym <- fit$call$weights
    if(is.name(w.sym) && as.character(w.sym) %in% names(data)){
      w <- data[[as.character(w.sym)]]
      if(is.numeric(w) && length(w) == nrow(data)) .weights <- w
    }
    # Only warn when a refit will actually drop the weights. If no refit happens the
    # original weighted fit is kept, so the curves are already correctly weighted
    # and there is nothing to warn about.
    if(.will.refit && is.null(.weights))
      warning("ggsurvplot_facet() could not safely recover the fit's `weights` for ",
              "the faceted refit, so the curves are drawn UNWEIGHTED. Pass the ",
              "weights as a bare column of `data` (e.g. `weights = w`, not ",
              "`weights = df$w`) for weighted faceting.", call. = FALSE)
  }
  # The drawn curves are weighted when the original weighted fit is kept (no refit)
  # or the refit used recovered weights. The per-panel p-value always refits
  # unweighted, so the warning below flags that only when the curves ARE weighted.
  .curves.weighted <- !is.null(fit$call$weights) && (!.will.refit || !is.null(.weights))

  # Changing panel labs if specified
  #:::::::::::::::::::::::::::::::::::::::::
  if(!is.null(panel.labs)){
    for(.grouping.var in facet.by){
      if(!is.null(panel.labs[[.grouping.var]])){
        if(!is.factor(data[, .grouping.var]))
          data[, .grouping.var] <- as.factor(data[, .grouping.var])
        levels(data[, .grouping.var]) <- panel.labs[[.grouping.var]]
      }
    }
  }

  # If some of facet.by variables are not included in survfit formula,
  # ex: surv ~ sex & facet.by = "rx", we need to refit surv curves with all variables including facet.by
  # Required for faceting: all variables should be present in the survival table
  #
  # If the user decides to change panel labels,  we should also re-comput survival curves
  # to take into account new labels
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  if(!all(facet.by %in% surv.vars) | !is.null(panel.labs)) {
    .refit.formula <- .build_formula(surv.obj, all.variables)
    fit <- if(is.null(.weights)) surv_fit(.refit.formula, data = data)
           else surv_fit(.refit.formula, data = data, weights = .weights)
  }

  if(length(vars.notin.groupby) == 0){
    # All the survival-formula variables are used for faceting, e.g. a null model
    # `~ 1` faceted by X, or `~ X` faceted by X. Each panel then holds a single
    # curve with no extra within-panel grouping, so there is nothing to combine
    # into a `.strata.` column (calling .create_strata() with no variable errors
    # with "subscript out of bounds"). `fit` has already been (re)built above as
    # `Surv ~ facet.by`, giving one stratum per panel (#304).
    if(is.null(color)) color <- "strata"
    .survformula <- .build_formula(surv.obj, "1")
  }
  else if(length(vars.notin.groupby) == 1){
    # Colour by the grouping variable. This must be a plain data column: when the
    # fit was built from a transformed or mis-specified term (e.g. `~ I(sex)`,
    # `~ strata(sex)`, `~ cut(age, 3)`, or a formula assembled with
    # `eval(as.name(...))` inside a loop), the term is not a column name and
    # surv_summary() does not split it back out into its own column, so it cannot
    # be used to colour the panels. Previously the term was forwarded as `color`
    # and failed at draw time with a cryptic "Unknown colour name" / col2rgb()
    # error. Fail early with an actionable message instead (#380).
    if(is.null(color)){
      if(vars.notin.groupby %in% colnames(data)) color <- vars.notin.groupby
      else stop("ggsurvplot_facet() cannot colour the panels by the grouping term '",
                vars.notin.groupby, "': it is not a column in `data` (it looks like a ",
                "transformed or programmatically built formula term). Use a plain ",
                "variable in the survival formula -- add the grouping column to `data` ",
                "first, or build the formula with reformulate() when constructing it in ",
                "a loop/function -- so the panels can be coloured by it.", call. = FALSE)
    }
    .survformula <- .build_formula(surv.obj, vars.notin.groupby)
  }
  else{
    # ex: surv ~ sex + rx + adhere & facet.by = "adhere"
    # by default adhere is included in the legend.
    # But we would like to see sex+rx accross adhere groups
    # We need to clean the legend by removing adhere
    # solution:
    #       1. add a new column ".strata." in the data. ".strata." is a column containing
    #         the combination of the levels of variables that are not in facet.by.
    #       2. ReFit survival curves with .strata. + facet.by variables (here adhere)
    #       3. Color by .strata. and facet by facet.by (here adhere)

    # Create strata using the combination of vars.notin.groupby
    new.strata <- .create_strata(data, vars.notin.groupby, sep = "; ")
    strata.levels <- levels(new.strata)

    # Add strata to data
    data <- data %>%
      dplyr::mutate(.strata. = new.strata)

    # Refit survival curves
    new.surv.vars <- paste(c(".strata.", facet.by), collapse = " + ")
    .new.formula <- .build_formula(surv.obj, new.surv.vars)
    fit <- if(is.null(.weights)) surv_fit(formula = .new.formula, data = data)
           else surv_fit(formula = .new.formula, data = data, weights = .weights)

    if(is.null(color)) color <- ".strata."
    if(is.null(legend.labs)) legend.labs <- gsub(";", ",", strata.levels)
    # used to compute pvalue for each panel
    .survformula <- .build_formula(surv.obj, ".strata.")

  }


  # Plot survival curves
  #:::::::::::::::::::::::::::::::::::::::::

  ggsurv <- ggsurvplot_core(fit, data = data, color = color, palette = palette,
                            legend.labs = legend.labs, ...)


  # Compute per-panel p-values
  #:::::::::::::::::::::::::::::::::::::::::
  # ggsurvplot_facet() computes and shows a p-value for EACH panel, so unlike
  # ggsurvplot() a single numeric/character `pval` cannot be substituted. Treat
  # any non-FALSE `pval` as a request to draw the per-panel p-values (this also
  # stops a character `pval` from erroring in `if(pval)` with "argument is not
  # interpretable as logical"), and warn that the supplied value is ignored (#636).
  # The p-values are computed BEFORE faceting so they can optionally be placed in
  # the panel labels (pval.in.label); the drawing on the panels happens after.
  draw.pval <- !is.null(pval) && !isFALSE(pval) && !identical(pval, "")
  if(draw.pval && !isTRUE(pval))
    warning("ggsurvplot_facet() displays a p-value computed for each panel; a ",
            "numeric or character `pval` is not substituted. Use `pval = TRUE`.",
            call. = FALSE)
  # The per-panel p-value uses survival::survdiff(), which has no case-weights
  # argument, so it is computed UNWEIGHTED even when the fit was weighted. The
  # curves/median above are now weighted (#556); warn so the weighted-curve vs
  # unweighted-p-value distinction is explicit rather than silent.
  if(draw.pval && .curves.weighted)
    warning("ggsurvplot_facet() per-panel p-values are computed unweighted: ",
            "survival::survdiff() has no case-weights argument, so the weighted ",
            "log-rank test is not available here. The curves and median lines are ",
            "weighted; only the p-values are not.", call. = FALSE)
  pvals.df <- NULL
  if(draw.pval){
    # If the response is a Surv object built OUTSIDE `data` -- a bare-symbol
    # formula LHS such as `Survival <- Surv(...); survfit(Survival ~ x, data = D)`,
    # as opposed to `Surv(time, status) ~ x` -- the per-panel p-value below
    # row-subsets `data` and refits `.survformula` on each subset, re-evaluating
    # the full-length global response against a shorter subset -> model.frame
    # "variable lengths differ" (#467). Materialise that response once on the full
    # data (in the same scope the formula was created in) and point the p-value
    # formula at that column, so subsetting keeps it row-aligned. Gated to the
    # bare-symbol-not-a-column case: `Surv(...) ~ x` (a call, not a symbol) and a
    # response that is already a data column both take the unchanged path.
    resp.expr <- .formula[[2]]
    if (is.symbol(resp.expr) && !(as.character(resp.expr) %in% colnames(data))) {
      .resp.env <- environment(.formula)
      if (is.null(.resp.env)) .resp.env <- parent.frame()
      # Resolve the response only if the symbol is reachable and matches the data
      # length; otherwise fall back to the unchanged path (same error as before,
      # never a new/worse one). This resolves the common case where the Surv
      # object lives in an accessible (e.g. global) scope, as in the report.
      resp.val <- tryCatch(eval(resp.expr, envir = data, enclos = .resp.env),
                           error = function(e) NULL)
      if (!is.null(resp.val) && NROW(resp.val) == nrow(data)) {
        .resp.col <- ".survminer.response."
        data[[.resp.col]] <- resp.val
        .survformula <- stats::update(.survformula,
                                      stats::as.formula(paste(.resp.col, "~ .")))
      }
    }
    # Grouped data
    grouped.d <- surv_group_by(data, grouping.vars = facet.by)
    # Compute survfit on each subset ==> list of fits
    sf <- surv_fit(.survformula, grouped.d$data, ...)
    grouped.d <- grouped.d %>% tibble::add_column(fit = sf)
    # Compute pvalue and convert the list of data frame into one data frame
    # by binding rows of the data frame
    pvalue <- surv_pvalue(grouped.d$fit, grouped.d$data, pval.coord = pval.coord,
                          pval.method.coord = pval.method.coord,...) %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble()
    # Optionally adjust the per-panel p-values for multiple comparisons across the
    # panels (mirrors pairwise_survdiff()'s p.adjust.method). The default "none"
    # leaves the raw per-panel p-values and their text untouched -- byte-identical
    # to before. When a method is set, adjust the numeric `pval` across panels and
    # regenerate the displayed text (prefixed "adj.p =") using the same formatting
    # as surv_pvalue(); NA panels (e.g. a single group in a panel) stay NA (#407).
    if(p.adjust.method != "none"){
      pval.digits <- if("pval.digits" %in% names(list(...))) list(...)$pval.digits else 2
      adj <- stats::p.adjust(pvalue$pval, method = p.adjust.method)
      pvalue$pval <- adj
      # `p.adjust.label` is the full displayed prefix (default "adj.p ="); for the
      # very-small-p case its trailing "=" is dropped and a "<" is used instead,
      # so it reads "adj.p < 0.0001". This also adds the "<" for a label given
      # without a trailing "=" (e.g. "q" -> "q < 0.0001"), rather than dropping
      # the operator.
      lt.label <- paste(sub("\\s*=\\s*$", "", p.adjust.label), "<")
      pvalue$pval.txt <- ifelse(
        is.na(adj), pvalue$pval.txt,
        ifelse(adj < 1e-04, paste(lt.label, "0.0001"),
               paste(p.adjust.label, signif(adj, pval.digits)))
      )
    }
    # Select the grouping variable columns and cbind the corresponding pvalue
    pvals.df <- grouped.d %>%
      dplyr::select(!!!syms(facet.by)) %>%
      dplyr::bind_cols(pvalue)
  }

  # Optionally build a panel-label labeller carrying the per-panel p-value
  #:::::::::::::::::::::::::::::::::::::::::
  # pval.in.label appends each panel's p-value to its strip label. It composes with
  # `labeller` (an explicit labeller wins, with a warning) and needs a single
  # faceting variable (with two, a per-panel p-value has no single strip to sit in,
  # so it is drawn on the panels instead). Default FALSE leaves `labeller` -- and
  # hence the strip labels -- exactly as before.
  strip.labeller <- labeller
  add.pval.to.label <- isTRUE(pval.in.label) && !is.null(pvals.df)
  if(isTRUE(pval.in.label) && is.null(pvals.df))
    warning("ggsurvplot_facet(): `pval.in.label = TRUE` requires `pval = TRUE`; ",
            "there are no p-values to add to the panel labels.", call. = FALSE)
  if(add.pval.to.label && length(facet.by) == 2){
    warning("ggsurvplot_facet(): `pval.in.label` is only supported for a single ",
            "faceting variable; the p-values are drawn on the panels instead.",
            call. = FALSE)
    add.pval.to.label <- FALSE
  }
  if(add.pval.to.label && !is.null(labeller)){
    warning("ggsurvplot_facet(): both `labeller` and `pval.in.label` were supplied; ",
            "`labeller` takes precedence and the p-values are not added to the labels.",
            call. = FALSE)
    add.pval.to.label <- FALSE
  }
  if(add.pval.to.label){
    .fvar <- facet.by[1]
    .vals <- as.character(pvals.df[[.fvar]])
    # Match .facet()'s short.panel.labs formatting: value only, or "var: value".
    .base <- if(short.panel.labs) .vals else paste0(.fvar, ": ", .vals)
    # Reuse the exact displayed p-value text (already respects p.adjust and
    # pval.digits); a panel with no test (NA) keeps just its base label.
    .lab <- ifelse(is.na(pvals.df$pval.txt) | pvals.df$pval.txt == "",
                   .base, paste0(.base, "\n", pvals.df$pval.txt))
    strip.labeller <- ggplot2::as_labeller(stats::setNames(.lab, .vals))
  }

  # Faceting the main plot
  #:::::::::::::::::::::::::::::::::::::::::

  p <- .facet(ggsurv$plot, facet.by, nrow = nrow, ncol = ncol,
              scales = scales, short.panel.labs = short.panel.labs,
              panel.labs.background = panel.labs.background,
              panel.labs.font = panel.labs.font,
              panel.labs.font.x =panel.labs.font.x,
              panel.labs.font.y = panel.labs.font.y,
              labeller = strip.labeller)

  # Draw the p-values on the panels unless they were placed in the labels
  #:::::::::::::::::::::::::::::::::::::::::
  if(draw.pval && !add.pval.to.label){
    pval.x <- pval.y <- pval.txt <- method.x <- method.y <- method <-  NULL
    p <- p +
      geom_text(data = pvals.df, aes(x = pval.x, y = pval.y, label = pval.txt),
                hjust = 0, size = pval.size)
    if(pval.method)
      p <- p + geom_text(data = pvals.df,  aes(x = method.x, y = method.y, label = method),
                         hjust = 0, size = pval.size)
  }

  # Faceted number-at-risk table
  #:::::::::::::::::::::::::::::::::::::::::
  # Draw a number-at-risk table under the faceted curves. Build it by re-running
  # ggsurvplot_core() with the table on the same (possibly refit) survival object,
  # relabel the table's y axis to the within-panel group, facet it with the same
  # structure, and stack the two aligned grobs. Default risk.table = FALSE returns
  # the plain faceted ggplot unchanged.
  draw.table <- !is.null(risk.table) && !isFALSE(risk.table) && !identical(risk.table, "")
  # A faceted risk table is only well-defined for a SINGLE faceting variable. With
  # two, the table is drawn with facet_grid(), which shares one y axis per grid row;
  # each panel then only holds a subset of that row's strata, so the shared axis
  # cannot label (or colour) the per-panel strata correctly. Rather than draw a
  # misleading table, warn and return the plot without one (mirrors pval.in.label).
  if(draw.table && length(facet.by) != 1L){
    warning("ggsurvplot_facet(): a faceted risk table is only supported for a single ",
            "`facet.by` variable; returning the faceted plot without a risk table.",
            call. = FALSE)
    draw.table <- FALSE
  }
  # The within-panel grouping variable whose value labels the table's y axis. It is
  # a real column of the (refit) survival summary regardless of the `color` argument,
  # so labelling from it (rather than from `color`) stays correct even when the user
  # passes a literal colour. On default calls `color` equals this variable, so the
  # labels are unchanged.
  .within.var <- if(length(vars.notin.groupby) == 1L) vars.notin.groupby
                 else if(length(vars.notin.groupby) > 1L) ".strata."
                 else NULL
  if(draw.table){
    # Re-run core for the table on the faceted (refit) object. Pass
    # legend.labs = NULL: the table labels the FULL strata (.strata. x facet.by),
    # whose count differs from the colour groups, so forwarding legend.labs would
    # only trip a length warning -- we relabel the table y axis below from the
    # structured grouping column. The re-run is silenced (warnings + messages):
    # its only unique diagnostic is that benign legend.labs-length warning; every
    # other warning it could raise was already surfaced by the curve build above
    # on the same fit/data, so nothing user-actionable is hidden here.
    # In a faceted table each row is identified by its TEXT label (a single colour;
    # the curve colours are not a reliable identifier across facets). tables.y.text =
    # FALSE would hide that label and leave only the unreliable colour, so it is not
    # supported here: the table is always built with the labels shown, and we warn.
    if(!isTRUE(tables.y.text))
      warning("ggsurvplot_facet(): `tables.y.text = FALSE` is not supported for a ",
              "faceted risk table (its rows are identified by their text labels, not ",
              "by colour); the labels are shown.", call. = FALSE)
    # Drop unused factor levels of the grouping variable before building the table.
    # An empty level makes the strata colour palette longer than the strata actually
    # present, which errors in the colour extraction -- a table-only failure; the plot
    # itself tolerates it. An empty level contributes no curve and no table row, so
    # dropping it changes nothing visible.
    data.tab <- data
    if(!is.null(.within.var) && .within.var %in% names(data.tab) &&
       is.factor(data.tab[[.within.var]]))
      data.tab[[.within.var]] <- droplevels(data.tab[[.within.var]])
    core.tab <- suppressWarnings(suppressMessages(
      ggsurvplot_core(fit, data = data.tab, color = color, palette = palette,
                      legend.labs = NULL, risk.table = risk.table,
                      risk.table.height = risk.table.height,
                      surv.plot.height = surv.plot.height,
                      tables.y.text = TRUE, tables.col = tables.col,
                      tables.theme = tables.theme, fontsize = fontsize, ...)
    ))
    tab <- core.tab$table
    if(!is.null(tab)){
      # Relabel the table y axis to show only the within-panel grouping value (the
      # colour variable), not the full combined strata string. Read the value from
      # the structured grouping COLUMN carried in the table data (surv_summary()
      # splits the combined strata into per-variable columns), so a factor level
      # that itself contains '=', ',' or ';' is handled correctly -- no string
      # parsing of the combined label (cf. #291/#430/#599/#616/#680).
      tdata <- tab$data
      if(!is.null(.within.var) && .within.var %in% names(tdata) &&
         "strata" %in% names(tdata)){
        .lv <- as.character(levels(tdata$strata))
        if(length(.lv) == 0L) .lv <- unique(as.character(tdata$strata))
        .grp <- as.character(tdata[[.within.var]][match(.lv, as.character(tdata$strata))])
        # Match the main legend's formatting for the combined ".strata." column
        # (legend.labs above uses gsub(";", ",")).
        if(identical(.within.var, ".strata.")) .grp <- gsub(";", ",", .grp)
        # ggsurvtable draws the y axis with breaks = levels(strata) and
        # labels = rev(levels(strata)) (it maps aes(y = rev(strata)) so the first
        # stratum sits at the top). Replace only the label TEXT with the clean group
        # name, keeping the exact same breaks and rev() arrangement, so the labels
        # stay aligned with the plotted rows and their counts (a full scale
        # replacement would reset the order and silently swap the labels against the
        # counts).
        tab <- suppressMessages(
          tab + ggplot2::scale_y_discrete(breaks = .lv, labels = rev(.grp))
        )
      }
      # Draw the strata labels in a single neutral colour. ggsurvtable colours the
      # y-axis text by strata via a POSITIONAL colour vector (a fixed theme setting,
      # not a mapped scale), which facet_wrap cannot recolour per panel: once free_y
      # drops a stratum from a panel, the remaining rows are recoloured by position
      # and so point to the WRONG stratum by colour (e.g. a panel holding only the
      # second group paints its row the first group's colour). Each row is already
      # identified unambiguously by its (correct) label text and its panel, so a
      # single colour is used rather than one that could mislead. The table is always
      # built with the labels shown (element_text), so this override merges cleanly.
      # Honour a user-supplied solid `tables.col`; the strata-colouring request
      # ("strata"), a NULL, or a non-scalar value all fall back to black.
      .tcol <- if(is.null(tables.col) || length(tables.col) != 1L ||
                  identical(tables.col, "strata")) "black" else tables.col
      tab <- tab + ggplot2::theme(
        axis.text.y = ggplot2::element_text(colour = .tcol))
      # The refit strata are the within-panel group x facet.by combination, so each
      # panel must FREE its y scale to drop the strata that belong to the other panels
      # (otherwise every panel shows every combination). Keep the x scale in step with
      # the plot so the time axes still align.
      table.scales <- if(scales %in% c("free", "free_x")) "free" else "free_y"
      tp <- .facet(tab, facet.by, nrow = nrow, ncol = ncol, scales = table.scales,
                   short.panel.labs = short.panel.labs,
                   panel.labs.background = panel.labs.background,
                   panel.labs.font = panel.labs.font,
                   panel.labs.font.x = panel.labs.font.x,
                   panel.labs.font.y = panel.labs.font.y,
                   labeller = labeller)
      # Stack the aligned plot and table grobs, honouring the height ratio. Lock
      # both to a common left column layout first (shared with the classic path) so
      # their x-axes align, then row-bind.
      aligned <- .align_panel_widths(list(ggplot2::ggplotGrob(p),
                                          ggplot2::ggplotGrob(tp)))
      p.grob <- aligned[[1]]
      t.grob <- aligned[[2]]
      ncol.min <- min(ncol(p.grob), ncol(t.grob))
      g <- gridExtra::gtable_rbind(p.grob[, seq_len(ncol.min)],
                                   t.grob[, seq_len(ncol.min)], size = "max")
      # Set the plot vs table panel-row heights to surv.plot.height : risk.table.height.
      # After gtable_rbind the plot's panel rows keep their positions and the table's
      # rows are offset by nrow(p.grob); read each grob's own panel rows rather than
      # assuming the two share the same layout.
      .prows <- unique(p.grob$layout$t[grepl("^panel", p.grob$layout$name)])
      .trows <- unique(t.grob$layout$t[grepl("^panel", t.grob$layout$name)])
      if(length(.prows))
        g$heights[.prows] <- grid::unit(surv.plot.height / length(.prows), "null")
      if(length(.trows))
        g$heights[.trows + nrow(p.grob)] <- grid::unit(risk.table.height / length(.trows), "null")
      g <- structure(g, class = c("ggsurvplot_facet", "ggsurv", class(g)))
      return(g)
    }
  }

  p

}




# Helper function for faceting
.facet <- function(p,  facet.by, nrow = NULL, ncol = NULL,
                   scales = "fixed", short.panel.labs = FALSE,
                   panel.labs.background = list(color = NULL, fill = NULL),
                   panel.labs.font = list(face = NULL, color = NULL, size = NULL, angle = NULL),
                   panel.labs.font.x = panel.labs.font,
                   panel.labs.font.y = panel.labs.font,
                   labeller = NULL
                   )
{

  panel.labs.background <- .compact(panel.labs.background)
  panel.labs.font.x <- .compact(panel.labs.font.x)
  panel.labs.font.y <- .compact(panel.labs.font.y)

  .labeller <- "label_value"
  if(!short.panel.labs) .labeller <- label_both
  # A user-supplied labeller takes precedence over the short.panel.labs choice
  # (both control the strip-label formatting). Default NULL -> unchanged (#667).
  if(!is.null(labeller)) .labeller <- labeller

  if(length(facet.by) == 1){
    facet.formula <- paste0("~", facet.by) %>% stats::as.formula()
    p <- p + facet_wrap(facet.formula, nrow = nrow, ncol = ncol, scales = scales, labeller = .labeller)
  }
  else if(length(facet.by) == 2){
    facet.formula <- paste(facet.by, collapse = " ~ ") %>% stats::as.formula()
    p <- p + facet_grid(facet.formula, scales = scales, labeller = .labeller)
  }

  if(!.is_empty(panel.labs.background))
    p <- p + theme(strip.background = do.call(element_rect, panel.labs.background))
  if(!.is_empty(panel.labs.font.x))
    p <- p + theme(strip.text.x = do.call(element_text, panel.labs.font.x))
  if(!.is_empty(panel.labs.font.y))
    p <- p + theme(strip.text.y = do.call(element_text, panel.labs.font.y))

  p
}


#' @param x an object of class ggsurvplot_facet (the gtable returned by
#'   \code{ggsurvplot_facet()} when \code{risk.table} is drawn).
#' @param newpage logical. If TRUE (default), a new page is drawn before the plot.
#' @method print ggsurvplot_facet
#' @rdname ggsurvplot_facet
#' @export
print.ggsurvplot_facet <- function(x, newpage = TRUE, ...) {
  if(newpage) grid::grid.newpage()
  grid::grid.draw(x)
  invisible(x)
}




