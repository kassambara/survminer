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
#'  .... Case insensitive partial match is allowed.\cr\cr To learn more about
#'  the mathematical background behind the different log-rank weights, read the
#'  following blog post on R-Addict:
#'  \href{http://r-addict.com/2017/02/09/Fancy-Survival-Plots.html}{Comparing
#'  (Fancy) Survival Curves with Weighted Log-rank Tests}
#'@param test.for.trend logical value. Default is FALSE. If TRUE, returns the
#'  test for trend p-values. Tests for trend are designed to detect ordered
#'  differences in survival curves. That is, for at least one group. The test
#'  for trend can be only performed when the number of groups is > 2.
#'@param combine logical value. Used only when fit is a list of survfit objects.
#'  If TRUE, combine the results for multiple fits.
#'@param ... other arguments including pval, pval.coord, pval.method.coord.
#'  These are only used internally to specify custom pvalue, pvalue and pvalue
#'  method coordinates on the survival plot. Normally, users don't need these
#'  arguments.
#'
#'@return  Return a data frame with the columns (pval, method, pval.txt and
#'  variable). If additional arguments (pval, pval.coord, pval.method.coord,
#'  get_coord) are specified, then extra columns (pval.x, pval.y, method.x and
#'  method.y) are returned. \itemize{ \item pval: pvalue \item method: method
#'  used to compute pvalues \item pval.txt: formatted text ready to use for
#'  annotating plots \item pval.x, pval.y: x & y coordinates of the pvalue for
#'  annotating the plot \item method.x, method.y: x & y coordinates of pvalue
#'  method }
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

  get_coord <- ("pval.coord" %in% .dots_n) | ("pval.method.coord" %in% .dots_n) | !is.null(.dots$get_coord)


  if(.is_list(fit) & .is_list(data)){

    if(length(fit) != length(data))
      stop("When fit and data are lists, ",
           "they should have the same length")

    res <- purrr::map2(fit, data, .pvalue,
                       method = method,  pval = pval,
                       pval.coord = pval.coord,  pval.method.coord = pval.method.coord,
                       get_coord =  get_coord, test.for.trend = test.for.trend )
  }
  else if(.is_list(fit)){
    res <- purrr::map(fit, .pvalue,
                      data = data, method = method,  pval = pval,
                      pval.coord = pval.coord,  pval.method.coord = pval.method.coord,
                      get_coord  =  get_coord, test.for.trend = test.for.trend)
  }
  else{
    res <- .pvalue(fit, data = data, method = method,  pval = pval,
                   pval.coord = pval.coord,  pval.method.coord = pval.method.coord,
                   get_coord  =  get_coord, test.for.trend = test.for.trend ) %>%
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
                    test.for.trend = FALSE)
{

  if(is.null(method)) method <- "survdiff"
  if(is.null(pval)) pval <- FALSE
  if(pval == "") pval <- FALSE

  . <- NULL

  allowed.methods <- c("survdiff", "log-rank", "LR", "1",
                       "n", "Gehan-Breslow", "GB",
                       "sqrtN", "Tarone-Ware", "TW",
                       "S1", "Peto-Peto", "PP",
                       "S2", "modified Peto-Peto", "mPP",
                       "FH_p=1_q=1", "Fleming-Harrington(p=1, q=1)", "FH")

  method.names <- c(rep("survdiff", 4),
                    rep(c("n", "sqrtN", "S1", "S2", "FH_p=1_q=1"), each = 3))
  # don't use grep which will detect many positions for "n" or "FH
  choosed.method  <- which(tolower(allowed.methods) %in% tolower(method))
  if(.is_empty(choosed.method))
    stop("Don't support the choosed method: ", choosed.method, ". ",
         "Allowed methods include: ", .collapse(allowed.methods, sep = ", "))
  else method <- method.names[choosed.method] %>% .[1]

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
    pvalue <- stats::pchisq(sdiff$chisq, length(sdiff$n) - 1, lower.tail = FALSE)
    pval.txt <- ifelse(pvalue < 1e-04, "p < 0.0001",
                       paste("p =", signif(pvalue, 2)))
    res <- list(pval = pvalue, method = "Log-rank", pval.txt = pval.txt)
  }
  # Other possibilities to compute pvalue using the survMisc package
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  else {
    tenfit <- ten(eval(fit$call$formula), data = data)
    capture.output(comp(tenfit)) -> null_dev
    # comp modifies tenfit object (ten class: ?survMisc::ten)
    # and adds attributes with tests
    if(test.for.trend)
      attributes(tenfit)$tft$tft -> tests
    else
    attributes(tenfit)$lrt -> tests

    if(test.for.trend & is.null(tests))
      stop("Test for trend is NULL. ",
           "Note that, the number of groups should be > 2 to perform the test for trend. ")

    lr_p <- tests$pChisq
    if(is.null(lr_p)) lr_p <- tests$pNorm
    lr_w <- tests$W

    # check str(tests) -> W:weights / pNorm:p-values
    pvalue <- round(lr_p[lr_w == method], 4)
    pval.txt <- ifelse(pvalue < 1e-04, "p < 0.0001",
                       paste("p =", signif(pvalue, 2)))
    test_name <- c("Log-rank", "Gehan-Breslow",
                   "Tarone-Ware", "Peto-Peto",
                   "modified Peto-Peto", "Fleming-Harrington (p=1, q=1)")
    # taken from ?survMisc::comp
    method <- test_name[lr_w == method]
    if(test.for.trend)
      method <- paste0(method, ", tft")
    res <- list(pval = pvalue, method = method, pval.txt = pval.txt)
  }
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


