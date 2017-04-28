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
                               pval = FALSE,  ...)
{

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
  pval <- surv_pvalue(fit,
                      method = .dots$log.rank.weights,
                      data = fit.ext$data.all,
                      pval = pval, pval.coord = .dots$pval.coord,
                      pval.method.coord = .dots$pval.method.coord)

  p <- ggsurvplot_core(newfit, data = data, legend.title = legend.title, legend.labs = legend.labs,
                       pval = pval$pval.txt,  ...)

  p
}

