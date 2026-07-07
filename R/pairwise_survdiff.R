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
#'  \code{\link[stats]{p.adjust}}). Allowed values include "holm", "hochberg",
#'  "hommel", "bonferroni", "BH", "BY", "fdr", "none". If you don't want to
#'  adjust the p value (not recommended), use p.adjust.method = "none".
#'@param na.action a missing-data filter function. Default is
#'  options()$na.action.
#'@param rho a scalar parameter that controls the type of test (used when
#'  \code{method = "survdiff"}). Allowed values include 0 (for Log-Rank test) and
#'  1 (for peto & peto test).
#'@param method the log-rank test to use, matching the \code{method} argument of
#'  \code{\link{surv_pvalue}}. Default \code{"survdiff"} uses
#'  \code{survival::survdiff()} (controlled by \code{rho}). A weighted log-rank
#'  test can be requested by its weight code, full name or abbreviation:
#'  \code{"n"}/Gehan-Breslow, \code{"sqrtN"}/Tarone-Ware, \code{"S1"}/Peto-Peto,
#'  \code{"S2"}/modified Peto-Peto, or \code{"FH_p=1_q=1"}/Fleming-Harrington.
#'  Weighted methods do not support \code{strata()} terms.
#'@param ref.group optional character string naming a single grouping level to
#'  use as the common reference/control. When supplied, every other group is
#'  compared against this one group only (rather than all pairwise comparisons),
#'  and the p-values are adjusted over just those comparisons -- useful for a
#'  many-treatments-vs-one-control design, which needs a smaller multiple-testing
#'  correction. The default \code{NULL} performs all pairwise comparisons.
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
pairwise_survdiff <- function(formula, data, p.adjust.method = "BH", na.action, rho = 0,
                              method = "survdiff", ref.group = NULL)

{
  if(missing(na.action)) na.action <- options()$na.action
  # Resolve the log-rank method/alias (shared with surv_pvalue()). "survdiff"
  # (default) keeps the exact survival::survdiff() + rho path; a weighted-test
  # name ("n"/Gehan-Breslow, "sqrtN"/Tarone-Ware, "S1"/Peto-Peto,
  # "S2"/modified Peto-Peto, "FH_p=1_q=1"/Fleming-Harrington) uses the internal
  # weighted log-rank test, matching surv_pvalue()'s method argument (#433).
  method <- .resolve_logrank_method(method)
  all_terms <- attr(stats::terms(formula), "term.labels")
  # Separate strata() terms (an adjustment, not a grouping variable): they are
  # not data columns, so using them to subset/group crashed with "undefined
  # columns selected". They are kept in the survdiff formula so the pairwise
  # test is stratified (#648). A survival::strata() term is also recognised: the
  # survival:: prefix is stripped so bare strata() is passed to survdiff() (a
  # stratification special on all survival versions). Otherwise the qualified
  # term was mistaken for a grouping variable -> "undefined columns selected"
  # (#672, flagged by T. Therneau).
  is_strata <- grepl("^(survival::)?strata\\(", all_terms)
  strata_terms <- sub("^survival::", "", all_terms[is_strata])
  group_var <- all_terms[!is_strata]
  if(length(group_var) == 0)
    stop("The formula must contain at least one grouping variable ",
         "(besides strata() terms).", call. = FALSE)
  surv_obj <- deparse(formula[[2]])

  DNAME <- paste(deparse(substitute(data)), "and", .collapse(group_var, sep = " + " ))
  METHOD <- "Log-Rank"
  if (method == "survdiff") {
    METHOD <- if (rho == 0) "Log-Rank test"
    else if(rho==1) "Peto & Peto test"
  } else {
    METHOD <- switch(method,
                     n = "Gehan-Breslow test", sqrtN = "Tarone-Ware test",
                     S1 = "Peto-Peto test", S2 = "modified Peto-Peto test",
                     `FH_p=1_q=1` = "Fleming-Harrington (p=1, q=1) test")
  }
  # Weighted log-rank tests operate on the two-group subset directly and do not
  # support a stratified comparison; keep those to the survdiff path.
  if (method != "survdiff" && length(strata_terms) > 0)
    stop("Weighted log-rank methods (method = \"", method, "\") do not support ",
         "strata() terms; use method = \"survdiff\" for a stratified test.",
         call. = FALSE)


  # Removing missing value
  # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .is.na <- data[, group_var, drop = FALSE] %>%
    apply(MARGIN = 1L, anyNA)
  data <- data[!.is.na, ]

  # Grouping variables
  # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  if(length(group_var) == 1){
    group <- unlist(data[, group_var])
    rhs <- group_var
  }
  else {
    # Create strata with multiple grouping variables and add it in the data
    group <- data[, group_var] %>%
      survival::strata()
    data <- data %>% mutate(..group.. = group)
    rhs <- "..group.."
  }
  # Rebuild the formula from the grouping variable, keeping any strata() terms
  # so the pairwise comparison is stratified (#648).
  formula <- .build_formula(surv_obj, paste(c(rhs, strata_terms), collapse = " + "))
  if(!is.factor(group)) group <- as.factor(group)
  group <- droplevels(group)

  compare.levels <- function(i, j) {
    .subset = group %in% (levels(group))[c(i,j)]
    sub_data <- data[.subset, , drop = FALSE]
    if (method == "survdiff") {
      sdif <- survival::survdiff(formula, data = sub_data,
                                 rho = rho, na.action = na.action)
      stats::pchisq(sdif$chisq, length(sdif$n) - 1, lower.tail = FALSE)
    } else {
      .weighted_logrank_test(formula, data = sub_data, method = method)$pvalue
    }
  }

  if(is.null(ref.group)){
    # All pairwise comparisons (default, unchanged).
    PVAL <- stats::pairwise.table(compare.levels, levels(group), p.adjust.method)
  }
  else {
    # Compare every other group against a single control/reference group only.
    # Fewer comparisons -> a smaller multiple-testing correction. The p-values
    # are adjusted over just these k-1 comparisons (#364).
    ref.group <- as.character(ref.group)
    if(length(ref.group) != 1)
      stop("`ref.group` must be a single grouping level.", call. = FALSE)
    if(!(ref.group %in% levels(group)))
      stop("`ref.group` (\"", ref.group, "\") is not one of the grouping levels: ",
           .collapse(levels(group), sep = ", "), ".", call. = FALSE)
    others <- setdiff(levels(group), ref.group)
    i <- match(ref.group, levels(group))
    raw <- vapply(others, function(g) compare.levels(i, match(g, levels(group))),
                  numeric(1))
    adj <- stats::p.adjust(raw, method = p.adjust.method)
    # One column (the reference), one row per other group -- same
    # "pairwise.htest" matrix shape that print.pairwise.htest expects.
    PVAL <- matrix(adj, ncol = 1L, dimnames = list(others, ref.group))
  }

  res <- list(method = METHOD, data.name = DNAME, p.value = PVAL,
              p.adjust.method = p.adjust.method)
  class(res) <- "pairwise.htest"
  res
}


# Collapse one or two vectors
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.collapse <- function(x, y = NULL, sep = "."){
  if(missing(y))
    paste(x, collapse = sep)
  else if(is.null(x) & is.null(y))
    return(NULL)
  else if(is.null(x))
    return (as.character(y))
  else if(is.null(y))
    return(as.character(x))
  else
    paste0(x, sep, y)
}

