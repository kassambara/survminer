#' @include utilities.R ggsurvplot_core.R
NULL
#'Plot a List of Survfit Objects
#'
#'@description Take a list of survfit objects and produce a list of
#'  \code{ggsurvplots}.
#'
#'@param fit a list of survfit objects.
#'@param data data used to fit survival curves. Can be also a list of same
#'  length than \code{fit}.
#'@param title title of the plot. Can be a character vector or a list of titles
#'  of same length than \code{fit}. If \code{title} is not specified and
#'  \code{fit} is a named list, then the names of fit list are used as title.
#'@param legend.title legend title for each plot. Can be a character vector or a
#'  list of titles of same length than fit.
#'@param legend.labs character vector specifying legend labels. Used to replace
#'  the names of the strata from the fit. Should be given in the same order as
#'  those strata. Can be a list when \code{fit} is a list.
#'@param ... other arguments passed to the core function
#'  \code{\link{ggsurvplot}}
#'
#'@seealso \code{\link{ggsurvplot}}
#'
#'@return Returns a list of ggsurvplots.
#'
#' @examples
#'
#' library(survival)
#'
#' # Create a list of formulas
#'#:::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' data(colon)
#' f1 <- survfit(Surv(time, status) ~ adhere, data = colon)
#' f2 <- survfit(Surv(time, status) ~ rx, data = colon)
#' fits <- list(sex = f1, rx = f2)
#'
#' # Visualize
#'#:::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' legend.title <- list("sex", "rx")
#' ggsurvplot_list(fits, colon, legend.title = legend.title)
#'
#'@export
ggsurvplot_list <- function(fit, data, title = NULL, legend.labs = NULL,
                            legend.title = "Strata",  ...)
{

  if(.is_list(fit) & is.null(title))
    title <- names(fit)

  if(.is_list (fit) & .is_list (data)){
    if(length(fit) != length(data))
      stop("fit and data are lists with different length. ",
           "Can't handle that. When fit and data are lists, ",
           "they should have the same length.")
  }
  else if(.is_list (data)){
    stop("data is a list but not fit. Can't handle that. ",
         "If data is list, then fit should be a list too.")
  }

  # ggsurvplot options. Each of these options can be provided as a list
  # when fit is a list
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ggsurv.options <- list(fit = fit, legend.title = legend.title)
  if(!is.null(legend.labs))
    ggsurv.options$legend.labs <- legend.labs
  if(!is.null(title))
    ggsurv.options$title <- title

  # Case 1: fit and data are lists.
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  if(.is_list (fit) & .is_list (data)){
    ggsurv.options$data <- data # map fit to each data
    res <- purrr::pmap(ggsurv.options, ggsurvplot_core, ... )
  }

  # Case 2: fit is a list but not data.
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # Differents fits are applied to the same data set
  else if(.is_list (fit) & !.is_list (data)){
    res <- purrr::pmap(ggsurv.options, ggsurvplot_core, data = data, ... )
  }

  # Case 3: standard ggsurvplot of one fit
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  else
    res <- ggsurvplot_core(fit, data = data, title = title,
                           legend.labs = legend.labs,
                           legend.title = legend.title,   ...)

  if(.is_list(fit)){
    names(res) <- names(fit)
    res <- structure(res, class = c(class(res), "ggsurvplot_list"))
  }

  res
}
