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
                             nrow = NULL, ncol = NULL,
                             scales = "fixed",
                             short.panel.labs = FALSE, panel.labs = NULL,
                             labeller = NULL,
                             panel.labs.background = list(color = NULL, fill = NULL),
                             panel.labs.font = list(face = NULL, color = NULL, size = NULL, angle = NULL),
                             panel.labs.font.x = panel.labs.font,
                             panel.labs.font.y = panel.labs.font,
                             ...)
  {

  if(length(facet.by) > 2)
    stop("facet.by should be of length 1 or 2.")

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


  # Faceting the main plot
  #:::::::::::::::::::::::::::::::::::::::::

  p <- .facet(ggsurv$plot, facet.by, nrow = nrow, ncol = ncol,
              scales = scales, short.panel.labs = short.panel.labs,
              panel.labs.background = panel.labs.background,
              panel.labs.font = panel.labs.font,
              panel.labs.font.x =panel.labs.font.x,
              panel.labs.font.y = panel.labs.font.y,
              labeller = labeller)

  # Pvalues
  #:::::::::::::::::::::::::::::::::::::::::
  # ggsurvplot_facet() computes and shows a p-value for EACH panel, so unlike
  # ggsurvplot() a single numeric/character `pval` cannot be substituted. Treat
  # any non-FALSE `pval` as a request to draw the per-panel p-values (this also
  # stops a character `pval` from erroring in `if(pval)` with "argument is not
  # interpretable as logical"), and warn that the supplied value is ignored (#636).
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
    # Select the grouping variable columns and cbind the corresponding pvalue
    pvals.df <- grouped.d %>%
      dplyr::select(!!!syms(facet.by)) %>%
      dplyr::bind_cols(pvalue)
    pval.x <- pval.y <- pval.txt <- method.x <- method.y <- method <-  NULL
    p <- p +
      geom_text(data = pvals.df, aes(x = pval.x, y = pval.y, label = pval.txt),
                hjust = 0, size = pval.size)
    if(pval.method)
      p <- p + geom_text(data = pvals.df,  aes(x = method.x, y = method.y, label = method),
                         hjust = 0, size = pval.size)
  }

  # To do
  # facet tables
  # if(!is.null(ggsurv$table))
  # res$table <- .facet(ggsurv$table, facet.by, nrow = nrow, ncol = ncol,
  #                      scales = scales, panel.lab = panel.lab)
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




