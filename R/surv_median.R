#' @include utilities.R
NULL
#'Median of Survival Curves
#'
#'@description Returns the median survival with upper and lower confidence
#'  limits for the median at 95\% confidence levels.
#'@param fit A survfit object. Can be also a list of survfit objects.
#'@param combine logical value. Used only when fit is a list of survfit objects.
#'  If TRUE, combine the results for multiple fits.
#'@return Returns for each fit, a data frame with the following column:
#'  \itemize{ \item strata: strata/group names \item median: median survival of
#'  each group \item lower: 95\% lower confidence limit \item upper: 95\% upper
#'  confidence limit }Returns a list of data frames when the input is a
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

    .table <- .table %>%
      dplyr::select_(.dots = c("strata",  "median", "`0.95LCL`", "`0.95UCL`"))
    colnames(.table) <- c("strata", "median", "lower", "upper")
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


