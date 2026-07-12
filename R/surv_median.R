#' @include utilities.R
NULL
#'Median of Survival Curves
#'
#'@description Returns the median survival with upper and lower confidence
#'  limits for the median. The confidence limits are taken at the confidence
#'  level the survfit was built with (95\% by default); a fit stored without
#'  confidence limits (e.g. \code{conf.type = "none"}) returns \code{NA} limits.
#'@param fit A survfit object. Can be also a list of survfit objects.
#'@param combine logical value. Used only when fit is a list of survfit objects.
#'  If TRUE, combine the results for multiple fits.
#'@return Returns for each fit, a data frame with the following column:
#'  \itemize{ \item strata: strata/group names \item median: median survival of
#'  each group \item lower: lower confidence limit of the median \item upper:
#'  upper confidence limit of the median }The confidence limits use the fit's own
#'  confidence level (95\% by default), and are \code{NA} for a fit built without
#'  confidence limits. Returns a list of data frames when the input is a
#'  list of survfit objects. If combine = TRUE, results are combined into one single data frame.
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
#'surv_median(fit.null)
#'
#'surv_median(fit2)
#'
#'surv_median(fit.list)
#'
#'surv_median(fit.list, combine = TRUE)
#'
#'# Grouped survfit
#'#:::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'fit.list2 <- surv_fit(Surv(time, status) ~ sex, data = colon,
#'                      group.by = "rx")
#'surv_median(fit.list2)
#' @rdname surv_median
#'@export
surv_median <- function(fit, combine = FALSE){

  # Helper function to extract the median survival of
  # a survfit object
  .median <- function(fit){
    if(!is.null(fit$strata) | is.matrix(fit$surv)) {
      .table <- as.data.frame(summary(fit)$table)
    }else{
      .table <- t(as.data.frame(summary(fit)$table)) %>%
        as.data.frame()
      rownames(.table) <- "All"
    }
    .table$strata <- rownames(.table)

    # summary()$table names the confidence-limit columns "<level>LCL"/"<level>UCL"
    # (e.g. "0.95LCL", or "0.9LCL" for conf.int = 0.9); a fit stored without CI
    # (conf.type = "none") has neither. The old code hard-coded "0.95LCL"/"0.95UCL",
    # so it errored both on a no-CI fit and on a non-default confidence level (#818).
    # Detect the columns by suffix and fall back to NA when absent, so the median is
    # always returned -- without limits for a no-CI fit. The default 0.95 case is
    # unchanged (the same LCL/UCL values, renamed to lower/upper).
    .lcl <- grep("LCL$", colnames(.table), value = TRUE)
    .ucl <- grep("UCL$", colnames(.table), value = TRUE)
    .table$lower <- if (length(.lcl) >= 1L) .table[[.lcl[1]]] else NA_real_
    .table$upper <- if (length(.ucl) >= 1L) .table[[.ucl[1]]] else NA_real_
    .table <- .table %>%
      dplyr::select(dplyr::all_of(c("strata", "median", "lower", "upper")))
    rownames(.table) <- NULL
    .table
  }

  if(.is_list(fit)){

    res <- purrr::map(fit, .median)
    if(is.null(names(res))) names(res) <- .get_fit_names(fit)

    if(combine)
      res <- .rbind_data_list(res )

    res
  }
  else .median(fit)
}


