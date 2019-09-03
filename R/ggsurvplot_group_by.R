#' @include ggsurvplot_list.R
NULL
#'Survival Curves of Grouped Data sets
#'
#'@description Survival curves of grouped data sets by one or two
#'  variables.
#'
#'  Survival analysis are often done on subsets defined by
#'  variables in the dataset. For example, assume that we have a cohort of
#'  patients with a large number of  clinicopathological and molecular
#'  covariates, including survival data, TP53 mutation status and the patients'
#'  sex (Male or Female).
#'
#'  One might be also interested in comparing the
#'  survival curves of Male and Female after grouping (or splitting ) the data
#'  by TP53 mutation status.
#'
#'  \code{ggsurvplot_group_by}() provides a
#'  convenient solution to create a multiple \link{ggsurvplot} of a data set
#'  grouped by one or two variables.
#'
#'@param fit a survfit object.
#'@param data a data frame used to fit survival curves.
#'@param group.by a character vector containing the name of grouping variables. Should be of length <= 2.
#'@param ... ... other arguments passed to the core function
#'  \code{\link{ggsurvplot}}.
#'
#'@details
#' \code{ggsurvplot_group_by}() works as follow:
#' \enumerate{
#' \item Create a grouped data sets using the function \code{\link{surv_group_by}()}, --> list of data sets
#' \item Map \code{\link{surv_fit}()} to each nested data --> Returns a list of survfit objects
#' \item Map \code{\link{ggsurvplot}()} to each survfit object --> list of survfit ggsurvplots
#' }
#' One can (optionally) arrange the list of ggsurvplots using \code{\link{arrange_ggsurvplots}()}
#'
#' @return Retuns a list of ggsurvplots.
#' @examples
#'# Fit survival curves
#'#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'library(survival)
#'fit <- survfit( Surv(time, status) ~ sex, data = colon )
#'
#'# Visualize: grouped by treatment rx
#'#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'ggsurv.list <- ggsurvplot_group_by(fit, colon, group.by = "rx", risk.table = TRUE,
#'                                  pval = TRUE, conf.int = TRUE, palette = "jco")
#'names(ggsurv.list)
#'
#'
#'# Visualize: grouped by treatment rx and adhere
#'#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'ggsurv.list <- ggsurvplot_group_by(fit, colon, group.by = c("rx", "adhere"),
#'                                  risk.table = TRUE,
#'                                  pval = TRUE, conf.int = TRUE, palette = "jco")
#'
#'names(ggsurv.list)
#'@export
ggsurvplot_group_by <- function(fit, data, group.by, ...){

  # Extract fit components
  #:::::::::::::::::::::::::::::::::::::::::
  fit.ext <- .extract.survfit(fit, data)
  .formula <- fit.ext$formula
  surv.obj <- fit.ext$surv
  surv.vars <- fit.ext$variables
  data <- fit.ext$data.all
  # Grouping the data ==> list of data sets
  #:::::::::::::::::::::::::::::::::::::::::
  grouped.d <- data %>%
    surv_group_by(grouping.vars = group.by)
  # Fit survival curves on each subset  ==> list of fits
  #:::::::::::::::::::::::::::::::::::::::::
  fits <- surv_fit(formula = .formula, data = grouped.d$data, ...)
  grouped.d <- grouped.d %>% tibble::add_column(fit = fits)
  # Map ggsurvplot to each fit ==> list of ggsurvplots
  #:::::::::::::::::::::::::::::::::::::::::
  ggsurvplot_list(fit = grouped.d$fit, data = grouped.d$data, ...)
}

