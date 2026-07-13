#' @include utilities.R
NULL

#'Compute P-value Comparing Survival Curves
#'
#'@description Compute p-value from survfit objects or parse it when provided by
#'  the user. Survival curves are compared using the log-rank test (default).
#'  Other methods can be specified using the argument \code{method}.
#'@param fit A survfit object. Can be also a list of survfit objects.
#'@param data data frame used to fit survival curves. Can be also a list of
#'  data.
#'@param method method to compute survival curves. Default is "survdiff" (or
#'  "log-rank"). Allowed values are one of: \itemize{ \item "survdiff",
#'  log-rank; \item "1": log-rank, LR; --> Regular log-rank test, sensitive to
#'  detect late differences. \item "n": Gehan-Breslow (generalized Wilcoxon),
#'  GB; --> detect early differences. \item "sqrtN": Tarone-Ware, TW; --> detect
#'  early differences. \item "S1": Peto-Peto's modified survival estimate, PP;
#'  --> more robust than Tharone-Whare or Gehan-Breslow, detect early
#'  differences \item "S2": modified Peto-Peto (by Andersen), mPP \item
#'  "FH_p=1_q=1": Fleming-Harrington(p=1, q=1),  FH } To specify method, one can
#'  use either the weights (e.g.: "1", "n", "sqrtN", ...), or the full name
#'  ("log-rank", "gehan-breslow", "Peto-Peto", ...), or the acronyme LR, GB,
#'  .... Case insensitive partial match is allowed.\cr\cr For an \strong{arbitrary
#'  Fleming-Harrington} \eqn{G(\rho, \gamma)} weight, use \code{method = "FH"}
#'  together with \code{rho} and \code{gamma} (passed via \code{...}): the weight
#'  is \eqn{S(t^-)^{\rho}(1 - S(t^-))^{\gamma}}. \code{FH(0, 0)} is the log-rank
#'  test, \code{FH(1, 0)} emphasises early differences, \code{FH(0, 1)} late
#'  differences and \code{FH(1, 1)} the middle. See \code{\link{weighted_logrank}()}
#'  for a batch version. Note: for backward compatibility a bare
#'  \code{method = "FH"} (with no \code{rho}/\code{gamma}) is \code{FH(1, 1)};
#'  pass \code{rho = 0, gamma = 0} for the log-rank test. \cr\cr To learn more about
#'  the mathematical background behind the different log-rank weights, read the
#'  following blog post on R-Addict:
#'  \href{https://rpkgs.datanovia.com/survminer/articles/Specifiying_weights_in_log-rank_comparisons.html}{Comparing
#'  (Fancy) Survival Curves with Weighted Log-rank Tests}
#'@param test.for.trend logical value. Default is FALSE. If TRUE, returns the
#'  test for trend p-values. Tests for trend are designed to detect ordered
#'  differences in survival curves. That is, for at least one group. The test
#'  for trend can be only performed when the number of groups is > 2.
#'@param combine logical value. Used only when fit is a list of survfit objects.
#'  If TRUE, combine the results for multiple fits.
#'@param ... other arguments. One useful argument is \code{pval.digits}: the
#'  number of significant digits used to format the p-value in the returned
#'  \code{pval.txt}. Default is 2 (e.g. \code{"p = 0.0013"}); set
#'  \code{pval.digits = 3} for \code{"p = 0.00131"}, etc. P-values below 0.0001
#'  are still reported as \code{"p < 0.0001"}. The remaining arguments (pval,
#'  pval.coord, pval.method.coord) are only used internally to specify custom
#'  pvalue and pvalue-method coordinates on the survival plot; normally users
#'  don't need them.
#'
#'@return  Return a data frame with the columns (variable, pval, method,
#'  pval.txt, statistic and df). If additional arguments (pval, pval.coord,
#'  pval.method.coord, get_coord) are specified, then extra columns (pval.x,
#'  pval.y, method.x and method.y) are returned. \itemize{ \item pval: pvalue
#'  \item method: method used to compute pvalues \item pval.txt: formatted text
#'  ready to use for annotating plots \item statistic: the test statistic
#'  (chi-square) \item df: its degrees of freedom \item pval.x, pval.y: x & y
#'  coordinates of the pvalue for annotating the plot \item method.x, method.y: x
#'  & y coordinates of pvalue method }
#'
#' @examples
#'
#'library(survival)
#'
#'# Different survfits
#'#:::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'fit.null <- surv_fit(Surv(time, status) ~ 1, data = colon)
#'
#'fit1 <- surv_fit(Surv(time, status) ~ sex, data = colon)
#'
#'fit2 <- surv_fit(Surv(time, status) ~ adhere, data = colon)
#'
#'fit.list <- list(sex = fit1, adhere = fit2)
#'
#'# Extract the median survival
#'#:::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'surv_pvalue(fit.null)
#'
#'surv_pvalue(fit2, colon)
#'
#'surv_pvalue(fit.list)
#'
#'surv_pvalue(fit.list, combine = TRUE)
#'
#'# Grouped survfit
#'#:::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'fit.list2 <- surv_fit(Surv(time, status) ~ sex, data = colon,
#'                      group.by = "rx")
#'
#'surv_pvalue(fit.list2)
#'
#'# Get coordinate for annotion of the survival plots
#'#:::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'surv_pvalue(fit.list2, combine = TRUE, get_coord = TRUE)
#'
#'@export
#'@rdname surv_pvalue
surv_pvalue <- function(fit, data = NULL, method = "survdiff", test.for.trend = FALSE, combine = FALSE,   ...)
{

  if(inherits(data, c("surv_group_by")))
    data <- data$data
  if(is.null(data)){
    if(.is_list(fit)){
      data <- purrr::map(fit, .get_data, complain = FALSE)
    }
    else if(.is_survfit(fit))
      data <- .get_data(fit, data, complain = FALSE)
  }

  # Helper arguments: Used internally to estimate or specify
  # the coordinates of pvalue and pvalue methods on the survival plot
  pval <- TRUE
  pval.coord <- pval.method.coord <- fit.name <- variable <- NULL
  .dots <- list(...)
  .dots_n <- names(.dots)
  if("pval" %in% .dots_n) pval <- .dots$pval
  if("pval.coord" %in% .dots_n) pval.coord <- .dots$pval.coord
  if("pval.method.coord" %in% .dots_n) pval.method.coord <- .dots$pval.method.coord
  # pval.digits is read from ... (rather than a formal) so that a bare `pval`
  # argument passed by callers does not partial-match it (#343).
  pval.digits <- 2
  if("pval.digits" %in% .dots_n) pval.digits <- .dots$pval.digits

  # Fleming-Harrington parameters for method = "FH" (arbitrary G(rho, gamma)).
  rho <- if("rho" %in% .dots_n) .dots$rho else NULL
  gamma <- if("gamma" %in% .dots_n) .dots$gamma else NULL

  get_coord <- ("pval.coord" %in% .dots_n) | ("pval.method.coord" %in% .dots_n) | !is.null(.dots$get_coord)


  if(.is_list(fit) & .is_list(data)){

    if(length(fit) != length(data))
      stop("When fit and data are lists, ",
           "they should have the same length")

    res <- purrr::map2(fit, data, .pvalue,
                       method = method,  pval = pval,
                       pval.coord = pval.coord,  pval.method.coord = pval.method.coord,
                       get_coord =  get_coord, test.for.trend = test.for.trend,
                       pval.digits = pval.digits, rho = rho, gamma = gamma )
  }
  else if(.is_list(fit)){
    res <- purrr::map(fit, .pvalue,
                      data = data, method = method,  pval = pval,
                      pval.coord = pval.coord,  pval.method.coord = pval.method.coord,
                      get_coord  =  get_coord, test.for.trend = test.for.trend,
                      pval.digits = pval.digits, rho = rho, gamma = gamma)
  }
  else{
    res <- .pvalue(fit, data = data, method = method,  pval = pval,
                   pval.coord = pval.coord,  pval.method.coord = pval.method.coord,
                   get_coord  =  get_coord, test.for.trend = test.for.trend,
                   pval.digits = pval.digits, rho = rho, gamma = gamma ) %>%
      dplyr::select(variable, dplyr::everything())
  }

  # Add fit name to the  result
  if(.is_list(fit) ){
    res <- purrr::map2(res, .get_fit_names(fit),
                       function(df, rname){
                         #df$fit.name <- rname
                         # dplyr::select(df, fit.name, variable, dplyr::everything())
                         dplyr::select(df,  variable, dplyr::everything())
                       })
    if(combine) res <- .rbind_data_list(res)
  }


  return (res)
}



# Helper function to compute pvalue for one fit
# Will be used for a list of fits
.pvalue <- function(fit, data, method = "survdiff",  pval = TRUE,
                    pval.coord = NULL,  pval.method.coord = NULL, get_coord = FALSE,
                    test.for.trend = FALSE, pval.digits = 2,
                    rho = NULL, gamma = NULL)
{

  if(is.null(method)) method <- "survdiff"
  if(is.null(pval)) pval <- FALSE
  if(pval == "") pval <- FALSE

  . <- NULL

  # Resolve the method name/alias to its canonical form (shared with
  # pairwise_survdiff() via .resolve_logrank_method()).
  method <- .resolve_logrank_method(method)

  # Arbitrary Fleming-Harrington G(rho, gamma): when either rho or gamma is given
  # AND the requested method is the Fleming-Harrington family, use the general FH
  # weight with those parameters. Given with any other method they are ignored
  # (with a warning) so the user's explicit method wins. Without them the
  # fixed-weight methods are unchanged, so existing calls -- including the bare
  # `method = "FH"` alias (= FH(1,1)) -- are byte-identical.
  if(!is.null(rho) || !is.null(gamma)){
    if(method %in% c("FH", "FH_p=1_q=1")){
      method <- "FH"
      if(is.null(rho)) rho <- 0
      if(is.null(gamma)) gamma <- 0
      if(length(rho) != 1L || length(gamma) != 1L)
        stop("surv_pvalue() computes a single test; `rho` and `gamma` must be ",
             "length 1. Use weighted_logrank() to run several at once.", call. = FALSE)
      if(rho < 0 || gamma < 0)
        stop("Fleming-Harrington `rho` and `gamma` must be >= 0.", call. = FALSE)
    } else {
      warning("`rho`/`gamma` are only used with `method = \"FH\"`; ignoring them ",
              "for method = \"", method, "\".", call. = FALSE)
      rho <- 0; gamma <- 0
    }
  } else { rho <- 0; gamma <- 0 }

  statistic <- NA_real_; test.df <- NA_real_

  if(test.for.trend & method == "survdiff")
    method <- "1" # use survMisc

  # Extract fit components
  fit.ext <- .extract.survfit(fit, data)
  surv.vars <- fit.ext$variables
  if(.is_empty(surv.vars)) surv.vars  <- "" # Case of null model
  data <- fit.ext$data.all

  res <- list(pval = NA, method = "", pval.txt = "" )
  # Pvalue provided by user as numeric
  if(is.numeric(pval))
    res <- list(pval = pval, method = "", pval.txt = paste("p =", pval) )
  # Pvalue provided by user as text
  else if(is.character(pval))
    res <- list(pval = NA, method = "", pval.txt = pval)
  # One group, NULL model ==> there are no groups to compare
  else if(is.null(fit$strata) & pval == TRUE){
    warning("There are no survival curves to be compared. \n This is a null model.",
            call. = TRUE)
  }
  # else if pval = FALSE ===> exit
  else if(is.logical(pval) & !pval){}
  # Compute p-value using survdiff method
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  else if(method == "survdiff") {
    ssubset <- fit$call$subset
    if(is.null(ssubset))
      sdiff <- survival::survdiff(eval(fit$call$formula), data = data)
    else
      sdiff <- survival::survdiff(eval(fit$call$formula), data = data,
                                  subset = eval(fit$call$subset))
    test.df <- length(sdiff$n) - 1
    statistic <- sdiff$chisq
    pvalue <- stats::pchisq(sdiff$chisq, test.df, lower.tail = FALSE)
    pval.txt <- ifelse(pvalue < 1e-04, "p < 0.0001",
                       paste("p =", signif(pvalue, pval.digits)))
    res <- list(pval = pvalue, method = "Log-rank", pval.txt = pval.txt)
  }
  # Weighted log-rank tests (internal implementation)
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  else {
    wlr <- .weighted_logrank_test(
      formula = eval(fit$call$formula), data = data,
      method = method, rho = rho, gamma = gamma, test.for.trend = test.for.trend
    )
    statistic <- wlr$statistic
    test.df <- wlr$df
    pvalue <- round(wlr$pvalue, 4)
    pval.txt <- ifelse(pvalue < 1e-04, "p < 0.0001",
                       paste("p =", signif(pvalue, pval.digits)))
    res <- list(pval = pvalue, method = wlr$method_name, pval.txt = pval.txt)
  }
  res$statistic <- statistic
  res$df <- test.df
  res$variable <- .collapse(surv.vars, sep =  "+")
  # Pvalue coordinates to annotate the plot
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  res$pval.x <- ifelse(is.null(pval.coord[1]), max(fit$time)/50, pval.coord[1])
  res$pval.y <- ifelse(is.null(pval.coord[2]), 0.2, pval.coord[2])

  res$method.x <- ifelse(is.null(pval.method.coord[1]), max(fit$time)/50, pval.method.coord[1])
  res$method.y <- ifelse(is.null(pval.method.coord[2]), 0.3, pval.method.coord[2])
  res <- as.data.frame(res, stringsAsFactors = FALSE)

  pval.x <- pval.y <- method.x <- method.y <- NULL
  if(!get_coord)
    res <- res %>%
    dplyr::select(-pval.x, -pval.y, -method.x, -method.y)

  res
}


