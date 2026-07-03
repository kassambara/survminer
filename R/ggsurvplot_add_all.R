#' @include utilities.R ggsurvplot_core.R
NULL
#' Add Survival Curves of Pooled Patients onto the Main Plot
#'
#' @description Add survival curves of pooled patients onto the main plot stratified by grouping variables.
#' @inheritParams ggsurvplot_arguments
#' @param ... other arguments passed to the \code{\link{ggsurvplot}()} function.
#' @return Return a ggsurvplot.
#'@seealso \code{\link{ggsurvplot}}
#' @examples
#'library(survival)
#'
#'# Fit survival curves
#'fit <- surv_fit(Surv(time, status) ~ sex, data = lung)
#'
#'# Visualize survival curves
#'ggsurvplot(fit, data = lung,
#'           risk.table = TRUE, pval = TRUE,
#'           surv.median.line = "hv", palette = "jco")
#'
#'# Add survival curves of pooled patients (Null model)
#'# Use add.all = TRUE option
#'ggsurvplot(fit, data = lung,
#'           risk.table = TRUE, pval = TRUE,
#'           surv.median.line = "hv", palette = "jco",
#'           add.all = TRUE)
#'
#' @export
ggsurvplot_add_all <- function(fit, data, legend.title = "Strata", legend.labs = NULL,
                               pval = FALSE, legend = "top", ...)
{
  # `legend` is an explicit argument (not left to `...`) because it partial-matches
  # both `legend.title` and `legend.labs`, which made `legend = "right"` raise
  # "argument matches multiple formal arguments" before reaching ggsurvplot_core (#566).

  .dots <- list(...)
  # Extract fit components
  fit.ext <- .extract.survfit(fit, data)
  .formula <- fit.ext$formula
  surv.obj <- fit.ext$surv
  surv.vars <- fit.ext$variables
  data <- fit.ext$data.formula # data of only variables in formula
  if(is.null(fit$strata))
    stop("This is a null model. Sorry, can't add nothing more.", call. = FALSE)

  # Create strata using the combination of surv.vars
  .strata <- .create_strata(data, surv.vars)
  strata.levels <- levels(.strata)
  # Add strata to data
  data <- data %>%
    dplyr::mutate(.strata. = as.character(.strata))

  # Add 'all' column
  data <- mutate(data, .all. = "all")

  # Create a new grouping column gathering .strata. and the .all. columns
  .strata. <- .all. <- NULL
  data <- data %>%
    gather(key = "strata.name", value = "strata.val",
           .strata., .all.)
  data$strata.val <- factor(data$strata.val, levels = c("all", strata.levels))

  # Refit survival curves after adding "all"
  newformula <- .build_formula(surv.obj, "strata.val")
  newfit <- surv_fit(newformula, data)

  # Visualize using core ggsurvplot
  if(is.null(legend.labs)) legend.labs <- c("all", strata.levels)
  # The p-value must be computed on the ORIGINAL fit (comparing the real strata),
  # not on newfit which contains the extra "all" curve; it is then passed to
  # ggsurvplot_core() as pre-formatted text. get_coord = TRUE so we also get the
  # method-annotation coordinates.
  pval <- surv_pvalue(fit,
                      method = .dots$log.rank.weights,
                      data = fit.ext$data.all,
                      pval = pval, pval.coord = .dots$pval.coord,
                      pval.method.coord = .dots$pval.method.coord,
                      get_coord = TRUE)

  # Draw the p-value method (test name) here rather than in ggsurvplot_core:
  # because the p-value is forwarded as text, core would re-derive the method
  # from newfit and get an empty string, so pval.method was rendered blank (#673).
  show.method <- isTRUE(.dots$pval.method)
  .dots$pval.method <- NULL
  p <- do.call(ggsurvplot_core,
               c(list(newfit, data = data, legend = legend,
                      legend.title = legend.title, legend.labs = legend.labs,
                      pval = pval$pval.txt),
                 .dots))

  if(show.method && is.data.frame(pval) && "method" %in% colnames(pval) &&
     length(pval$method) > 0 && nzchar(pval$method[1])){
    method.size <- if(!is.null(.dots$pval.method.size)) .dots$pval.method.size
                   else if(!is.null(.dots$pval.size)) .dots$pval.size else 5
    p$plot <- p$plot +
      ggplot2::annotate("text", x = pval$method.x[1], y = pval$method.y[1],
                        label = pval$method[1], size = method.size, hjust = 0)
  }

  p
}

