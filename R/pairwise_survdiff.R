#' @include utilities.R
NULL
#'Multiple Comparisons of Survival Curves
#'@description Calculate pairwise comparisons between group levels with
#'  corrections for multiple testing.
#'@param formula a formula expression as for other survival models, of the form
#'  Surv(time, status) ~ predictors.
#'@param data a data frame in which to interpret the variables occurring in the
#'  formula.
#'@param p.adjust.method method for adjusting p values (see
#'  \code{\link[stats]{p.adjust}}). Allowed values include c("holm", "hochberg",
#'  "hommel", "bonferroni", "BH", "BY", "fdr", "none"). If you don't want to
#'  adjust the p value (not recommended), use p.adjust.method = "none".
#'@param na.action a missing-data filter function. Default is
#'  options()$na.action.
#'@param rho a scalar parameter that controls the type of test. Allowed values
#'  include 0 (for Log-Rank test) and 1 (for peto & peto test).
#'@seealso survival::survdiff
#'@return Returns an object of class "pairwise.htest", which is a list
#'  containing the p values.
#'
#'@author Alboukadel Kassambara, \email{alboukadel.kassambara@@gmail.com}
#'
#'@examples
#'
#' library(survival)
#' library(survminer)
#' data(myeloma)
#'
#' # Pairwise survdiff
#' res <- pairwise_survdiff(Surv(time, event) ~ molecular_group,
#'      data = myeloma)
#' res
#'
#' # Symbolic number coding
#' symnum(res$p.value, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1),
#'    symbols = c("****", "***", "**", "*", "+", " "),
#'    abbr.colnames = FALSE, na = "")
#'
#'
#'@rdname pairwise_survdiff
#'@export
pairwise_survdiff <- function(formula, data, p.adjust.method = "BH", na.action, rho = 0)

{
  if(missing(na.action)) na.action <- options()$na.omit
  group_var <- attr(stats::terms(formula), "term.labels")
  group <- data[, group_var]
  ngroup <- length(levels(group))

  DNAME <- paste(deparse(substitute(data)), "and", group_var)
  METHOD <- "Log-Rank"
  METHOD <- if (rho == 0) "Log-Rank test"
  else if(rho==1) "Peto & Peto test"

  compare.levels <- function(i, j) {
    .subset = group %in% (unique(group))[c(i,j)]
    sdif <- survival::survdiff(formula, data = data[.subset, , drop = FALSE],
                               rho = rho, na.action = na.action)
    stats::pchisq(sdif$chisq, length(sdif$n) - 1, lower.tail = FALSE)
  }

  PVAL <- stats::pairwise.table(compare.levels, levels(group), p.adjust.method)

  res <- list(method = METHOD, data.name = DNAME, p.value = PVAL,
              p.adjust.method = p.adjust.method)
  class(res) <- "pairwise.htest"
  res
}
