#' @include utilities.R
NULL
#'Nice Summary of a Survival Curve
#'@description Compared to the default summary() function, surv_summary()
#'  creates a data frame containing a nice summary from
#'  \code{\link[survival]{survfit}} results.
#'@param x an object of class survfit
#'@return An object of class \bold{'surv_summary'}, which is a data frame with
#'  the following columns: \itemize{ \item time: the time points at which the
#'  curve has a step. \item n.risk: the number of subjects at risk at t. \item
#'  n.event: the number of events that occur at time t. \item  n.censor: number
#'  of censored events. \item surv: estimate of survival. \item std.err:
#'  standard error of survival. \item upper: upper end of confidence interval.
#'  \item lower: lower end of confidence interval. \item strata: stratification of survival curves.}
#
#'  In a situation, where survival curves have been fitted with one or more
#'  variables, surv_summary object contains \bold{extra columns} representing the
#'  variables. This makes it possible to facet the output of
#'  \code{\link{ggsurvplot}} by strata or by some combinations of factors.
#'
#'  surv_summary object has also an attribut named \bold{'table'} containing
#'  information about the survival curves, including medians of survival with
#'  confidence intervals, as well as, the total number of subjects and the
#'  number of event in each curve.
#'
#'
#' @examples
#'
#'# Fit survival curves
#' require("survival")
#' fit <- survfit(Surv(time, status) ~ rx + adhere, data = colon)
#'
#' # Summarize
#' res.sum <- surv_summary(fit)
#' head(res.sum)
#'
#' # Information about the survival curves
#' attr(res.sum, "table")
#'
#'
#'@export
surv_summary <- function (x){
  res <- as.data.frame(.compact(unclass(x)[c("time", "n.risk",
                                            "n.event", "n.censor")]))
  if (inherits(x, "survfitms")) {
    surv <- 1 - x$prev
    upper <- 1 - x$upper
    lower <- 1 - x$lower
    res <- cbind(res, surv = c(surv), std.err = c(x$std.err),
                 upper = c(upper), lower = c(lower))
    res$state <- rep(x$states, each = nrow(surv))
  }
  else {
    # Case of survfit(res.cox, newdata)
    if(is.matrix(x$surv)){
      ncurve <- ncol(x$surv)
      res <- data.frame(time = rep(x$time, ncurve), n.risk = rep(x$n.risk, ncurve),
                        n.event = rep(x$n.event, ncurve), n.censor = rep(x$n.censor, ncurve))
      res <- cbind(res, surv = .flat(x$surv), std.err = .flat(x$std.err),
                   upper = .flat(x$upper), lower = .flat(x$lower))
      res$strata <- as.factor(rep(colnames(x$surv), each = nrow(x$surv)))
    }
    # case of standard survfit() or survfit(res.cox)
    else res <- cbind(res, surv = x$surv, std.err = x$std.err,
                      upper = x$upper, lower = x$lower)
  }
  if (!is.null(x$strata)) {
    res$strata <- rep(names(x$strata), x$strata)
    res$strata <- .clean_strata(res$strata, x)
    # Add column for each variable in survival fit
    variables <- .get_variables(res$strata)
    for(variable in variables) res[[variable]] <- .get_variable_value(variable, res$strata, x)
  }
  structure(res, class = c("data.frame", "surv_summary"))
  attr(res, "table") <-  as.data.frame(summary(x)$table)
  res
}



# Helper functions
# ++++++++++++++++++
# Get variable names in strata
# strata is a vector
.get_variables <- function(strata){
  variables <- sapply(as.vector(strata),
                      function(x){
                        x <- unlist(strsplit(x, "=|,\\s+", perl=TRUE))
                        x[seq(1, length(x), 2)]
                        })
  unique(as.vector(variables))
}

# level of a given variable
.get_variable_value <- function(variable, strata, fit){
  res <- sapply(as.vector(strata), function(x){
          x <- unlist(strsplit(x, "=|(\\s+)?,\\s+", perl=TRUE))
          index <- grep(variable, x)
          .trim(x[index+1])
        })
  res <- as.vector(res)
  var_levels <- levels(eval(fit$call$data)[, variable])
  if(!is.null(var_levels)) res <- factor(res, levels = var_levels)
  else res <- as.factor(res)
  res
}


# Take a data frame and return a flatten value
.flat <- function(x){
  x <- as.data.frame(x)
  x <- tidyr::gather_(x,
                      key_col = "key", value_col = "value",
                      gather_cols = colnames(x))
  x$value
}

# remove dollar sign ($) in strata, in the situation, where
# the user uses data$variable to fit survival curves
.clean_strata <- function(strata, fit){
  strata <- as.character(strata)
  is_dollar_sign <- grepl("$", strata[1], fixed=TRUE)
  if(is_dollar_sign) {
    data_name <- unlist(strsplit(strata[1], "$", fixed =TRUE))[1]
    strata <- gsub(paste0(data_name, "$"), "", strata, fixed=TRUE)
    strata <- as.factor(strata)
  }
  else if(!missing(fit)) strata <- factor(strata, levels = names(fit$strata))
  return(strata)
}
