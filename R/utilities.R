#' @import ggplot2
#' @import ggpubr
#' @importFrom survival Surv
#' @importFrom survival survfit
#' @importFrom survival survdiff
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom methods is
#' @importFrom stats pchisq
#' @importFrom survMisc ten comp
#' @importFrom utils capture.output


# Count the number of ggplots in a list
.count_ggplots <- function(list.objects){
  nplot <- 0
  for(i in 1:length(list.objects)){
    if(is.ggplot(list.objects[[i]])) nplot <- nplot +1
  }
  nplot
}

# Extract legend from a ggplot
.get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if(length(leg) > 0) leg <- tmp$grobs[[leg]] # if legend
  else leg <- NULL
  return(leg)
}

# Connect observations by stairs.
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Connect observations by stairs.
#
# mapping the aesthetic mapping
# data a layer specific dataset
# stat the statistical transformation to use on the data for this layer
# position the position adjustment to use for overlapping points on this layer
# na.rm logical frag whether silently remove missing values
#  ... other arguments passed to methods
.geom_confint <- function (mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, ...) {
  ggplot2::layer(mapping = mapping,
                 data = data,
                 stat = stat,
                 geom = GeomConfint,
                 position = position,
                 params = list(na.rm = na.rm, ...))
}
GeomConfint <- ggplot2::ggproto('GeomConfint', ggplot2::GeomRibbon,
                                required_aes = c("x", "ymin", "ymax"),
                                draw_group = function(self, data, panel_scales, coord, na.rm = FALSE) {
                                  if (na.rm) data <- data[stats::complete.cases(self$required_aes), ]
                                  data <- data[order(data$group, data$x), ]
                                  data <- self$stairstep_confint(data)
                                  ggplot2:::GeomRibbon$draw_group(data, panel_scales, coord, na.rm = na.rm)
                                },
                                stairstep_confint = function (data) {
                                  data <- as.data.frame(data)[order(data$x), ]
                                  n <- nrow(data)
                                  ys <- rep(1:n, each = 2)[-2 * n]
                                  xs <- c(1, rep(2:n, each = 2))
                                  data.frame(x = data$x[xs], ymin = data$ymin[ys], ymax = data$ymax[ys],
                                             data[xs, setdiff(names(data), c("x", "ymin", "ymax"))])
                                }
)

GeomConfint_old <- ggplot2::ggproto('GeomConfint_old', ggplot2::GeomRibbon,
                                required_aes = c("x", "ymin", "ymax"),
                                draw_group = function(data, panel_scales, coord, na.rm = FALSE) {
                                  if (na.rm) data <- data[complete.cases(data[c("x", "ymin", "ymax")]), ]
                                  data <- rbind(data, data)
                                  data <- data[order(data$x), ]
                                  data$x <- c(data$x[2:nrow(data)], NA)
                                  data <- data[complete.cases(data["x"]), ]
                                  GeomRibbon$draw_group(data, panel_scales, coord, na.rm = FALSE)
                                }
#                                 draw_group = function(self, data, panel_scales, coord, na.rm = FALSE) {
#                                   if (na.rm) data <- data[stats::complete.cases(self$required_aes), ]
#                                   data <- data[order(data$group, data$x), ]
#                                   data <- self$stairstep_confint(data)
#                                   ggplot2:::GeomRibbon$draw_group(data, panel_scales, coord, na.rm = FALSE)
#                                 },
#                                 stairstep_confint = function (data) {
#                                   data <- as.data.frame(data)[order(data$x), ]
#                                   n <- nrow(data)
#                                   ys <- rep(1:n, each = 2)[-2 * n]
#                                   xs <- c(1, rep(2:n, each = 2))
#                                   data.frame(x = data$x[xs], ymin = data$ymin[ys], ymax = data$ymax[ys],
#                                              data[xs, setdiff(names(data), c("x", "ymin", "ymax"))])
#                                 }
)


# Remove NULL items in a vector or list
#
# x a vector or list
.compact <- function(x){Filter(Negate(is.null), x)}

# remove white space at the head and the tail of a string
.trim <- function(x){gsub("^\\s+|\\s+$", "", x)}

# Take a data frame and return a flatten value
.flat <- function(x){
  if(is.null(x)) return(NA)
  x <- as.data.frame(x)
  x <- tidyr::gather_(x,
                      key_col = "key", value_col = "value",
                      gather_cols = colnames(x))
  x$value
}

# extract dataset if not provided
.get_data <- function(fit, data = NULL, complain = TRUE) {
  if(is.null(data)){
    if (complain)
      warning ("The `data` argument is not provided. Data will be extracted from model fit.")
    data <- eval(fit$call$data)
    if (is.null(data))
      stop("The `data` argument should be provided either to ggsurvfit or survfit.")
  }
  data
}

# Compute default axis breaks as ggplot2
#-------------------------------------
# Return a vector of axis labels
.get_default_breaks <- function(x, .log = FALSE){
  if(!.log) scales::extended_breaks()(x)
  else scales::log_breaks()(x)
}


# Get survival summary for a specified time points
#------------------------------------------------
# fit: survfit object
# data: data used for survfit
# times: a vector of timepoints
#
# Return a data frame with the following components:
#   - strata: stratification of curve estimation
#   - time: the timepoints on the curve
#   - n.risk: the number of subjects at risk at time t-0
#   - n.event: the cumulative number of events that have occurred since the last time listed until time t+0
#   - n.censor: number of censored subjects
#   - strata_size: number of subject in the strata
.get_timepoints_survsummary <- function(fit, data, times, decimal.place = 0)
{
  survsummary <- summary(fit, times = times, extend = TRUE)

  if (is.null(fit$strata)) {
    .strata <- factor(rep("All", length(survsummary$time)))
    strata_names <- "All"
    strata_size <- rep(fit$n, length(.strata))
  }
  else {
    .strata <- factor(survsummary$strata)
    strata_names <- names(fit$strata)
    nstrata <- length(strata_names)
    strata_size <- rep(fit$n, each = length(.strata)/nstrata)
  }

  strata <- .clean_strata(.strata)
  res <- data.frame(
    strata = strata,
    time = survsummary$time,
    n.risk = round(survsummary$n.risk, digits = decimal.place),
    pct.risk = round(survsummary$n.risk*100/strata_size),
    n.event = round(survsummary$n.event, digits = decimal.place),
    cum.n.event = unlist(by(survsummary$n.event, strata, cumsum)),
    n.censor = round(survsummary$n.censor, digits = decimal.place),
    cum.n.censor = unlist(by(survsummary$n.censor, strata, cumsum)),
    strata_size = strata_size
  )

  if(!is.null(fit$strata)){
    variables <- .get_variables(res$strata, fit, data)
    for(variable in variables) res[[variable]] <- .get_variable_value(variable, res$strata, fit, data)
  }
  rownames(res) <- 1:nrow(res)
  res
}

# Get variable names in strata
# -----------------------------------------
# strata: a vector
# fit: survfit object
# data: data used to fit survival curves
.get_variables <- function(strata, fit, data = NULL){
  variables <- sapply(as.vector(strata),
                      function(x){
                        x <- unlist(strsplit(x, "=|,\\s+", perl=TRUE))
                        x[seq(1, length(x), 2)]
                      })
  variables <- unique(as.vector(variables))
  variables <- intersect(variables, colnames(.get_data(fit, data) ))
  variables
}

# levels of a given variable used in survfit formula
# ----------------------------
# variable: variable name
.get_variable_value <- function(variable, strata, fit, data = NULL){
  res <- sapply(as.vector(strata), function(x){
    x <- unlist(strsplit(x, "=|(\\s+)?,\\s+", perl=TRUE))
    # When a factor name is the same as one of its level, index is of length 2
    index <- grep(paste0("^", variable, "$"), x)[1]
    .trim(x[index+1])
  })
  res <- as.vector(res)
  var_levels <- levels(.get_data(fit, data)[, variable])
  if(!is.null(var_levels)) res <- factor(res, levels = var_levels)
  else res <- as.factor(res)
  res
}


# remove dollar sign ($) in strata
# ---------------------------------
# remove dollar sign ($) in strata, in the situation, where
# the user uses data$variable to fit survival curves
.clean_strata <- function(strata, fit){
  is_dollar_sign <- grepl("$", as.character(strata)[1], fixed=TRUE)
  if(is_dollar_sign) {
    strata <- as.character(strata)
    data_name <- unlist(strsplit(strata[1], "$", fixed =TRUE))[1]
    strata <- gsub(paste0(data_name, "$"), "", strata, fixed=TRUE)
    strata <- as.factor(strata)
  }
  else if(!missing(fit)) strata <- factor(strata, levels = names(fit$strata))
  return(strata)
}


# Set large dash as y tick labels when ytext = FALSE
# Each dash corresponds to a strata
# This is used for tables under the main survival plots
#
.set_large_dash_as_ytext <- function(ggp){
  ggp + theme(axis.text.y = ggtext::element_markdown(size = 50, vjust = 0.5),
        axis.ticks.y = element_blank())
}


# Transform x-axis labels according to the scale see ggsurvplot().
#
# labels: numeric vector (x-axis labels)
#xscale: numeric or character values (see
# gsurvplot). If numeric, the value is used to divide the labels on the x axis.
# For example, a value of 365.25 will give labels in years instead of the
# original days. If character, allowed options include one of c("d_m", "d_y",
# "m_d", "m_y", "y_d", "y_m"), where d = days, m = months and y = years. For
# example, xscale = "d_m" will transform labels from days to months; xscale =
# "m_y", will transform labels from months to years.
.format_xticklabels <- function(labels, xscale){

  # 1 year = 365.25 days
  # 1 month = 365.25/12 = 30.4375 days
  if(is.numeric(xscale)) xtrans <- 1/xscale
  else
    xtrans <- switch(xscale,
                     d_m = 12/365.25,
                     d_y = 1/365.25,
                     m_d = 365.25/12,
                     m_y = 1/12,
                     y_d = 365.25,
                     y_m = 12,
                     1
    )
  round(labels*xtrans,2)
}


# Extract strata colors used in survival curves
# Will be used to color the y.text of risk table and cumevents table
.extract_ggplot_colors <- function(p, grp.levels){
  g <- ggplot_build(p)
  .cols <- unlist(unique(g$data[[1]]["colour"]))
  if(!is.null(grp.levels)){
    if(length(.cols)==1) .cols <- rep(.cols, length(grp.levels))
    names(.cols) <- grp.levels
  }
  .cols
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Helper functions for survival curves
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

.is_survfit <- function(fit){
  inherits(fit, "survfit")
}

.is_grouped_data <- function(data){
  inherits(data, c("surv_group_by"))
}

.get_fit_formula <- function(fit){
  fit$call$formula %>%
    stats::as.formula()
}

.build_formula <- function(surv.obj, variables){
  . <- NULL
  paste(variables, collapse  = " + ") %>%
    paste0(surv.obj, " ~ ", .) %>%
    stats::as.formula()
}

# Function defining a transformation of the survival curve
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# see ?survival::plot.survfit
# d: data frame containing the column surv, upper and lower
# fun the function
.apply_surv_func <- function(d, fun = NULL){

  if (!is.null(fun)) {
    if (is.character(fun)) {
      fun <- switch(fun, log = function(y) log(y),
                    event = function(y) 1 - y,
                    cumhaz = function(y) -log(y),
                    cloglog = function(y) log(-log(y)),
                    pct = function(y) y * 100,
                    logpct = function(y) 100 * y,
                    identity = function(y) y,
                    stop("Unrecognized survival function argument"))
    }
    else if (!is.function(fun)) {
      stop("Invalid 'fun' argument")
    }
    cnames <- colnames(d)
    if("surv" %in% cnames) d$surv <- fun(d$surv)
    if("upper" %in% cnames) d$upper <- fun(d$upper)
    if("lower" %in% cnames) d$lower <- fun(d$lower)
  }
  return(d)
}


# Get the names of formulas
#.........................................................................
# If formulas is a named lists, returns the list names if available.
# If formula names are not available, collapse the variables in the formula, and use this as the formula name
# If formula is a formula object, returns collapsed variable names using "+"
.get_formula_names <- function(formula){

  # helper function to return collapsed variable names for one formula.
  .fname <- function(formula){
    res <- attr(stats::terms(formula), "term.labels")
    if(.is_empty(res)) res <- "null_model"
    else res <- .collapse(res, sep = " + ")
    res
  }

  if(.is_list(formula)){
    fnames <- names(formula)
    if(is.null(fnames)) fnames <- purrr::map(formula, .fname) %>%
        unlist()
  }

  else fnames <- .fname(formula)
  fnames
}



# Get the names of fit
# If fit is a named lists, returns the list names if available.
# If fit names are not available, collapse the variables in the formula, and use this as the fit name
#.........................................................................
.get_fit_names <- function(fit){

  if(.is_list(fit)){
    fnames <- names(fit)
    if(is.null(fnames)) {
      fnames <- purrr::map(fit, .get_fit_formula) %>%
        .get_formula_names()
    }
  }
  else fnames <- .get_fit_formula(fit) %>%
      .get_formula_names()
  fnames
}


# Extract survfit components
#.................................................................................
# Return a list: list(formula, surv, variables, data.all, data.formula)
#       - formula: survival formula
#       - surv: surv object
#       - variables: vector of variable names
#       - data.all: the dataset used in survfit
#       - data.formula: data off all variables in the formula including time and status
.extract.survfit <- function(fit, data = NULL){

  if(inherits(fit, c("survfit.cox", "survfitcox")))
    return(list())

  .formula <- fit$call$formula %>%
    stats::as.formula()
  surv.obj <- deparse(.formula[[2]])
  surv.vars <- attr(stats::terms(.formula), "term.labels")
  data.all <- data <- .get_data(fit, data = data, complain = FALSE)
  # data of variables used in formula
  data.formula <- stats::get_all_vars(.formula, data = data) #%>%
  #na.omit()

  list(formula = .formula, surv = surv.obj,
       variables = surv.vars,
       data.all = data.all,
       data.formula = data.formula)
}


# Create strata from variable names
#.................................................................................
# Returns a factor
# Example:
# library(survival)
# .create_strata(colon, c("sex", "rx"))
.create_strata <- function(data, var.names, sep = ", "){

  # Strata
  .strata <- data[, var.names, drop = FALSE] %>%
    survival::strata(sep = sep)
  .strata.levels <- levels(.strata)
  # Replace  "=" by ".
  .strata <- gsub("=", ":", .strata) %>% .trim()
  .strata.levels <- gsub("=", ":", .strata.levels) %>% .trim()
  factor(.strata, levels = .strata.levels)
}



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# General helper functions
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Returns the levels of a factor variable
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.levels <- function(x){
  if(!is.factor(x)) x <- as.factor(x)
  levels(x)
}

# Check if is a list
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.is_list <- function(x){
  inherits(x, c("list", "vctrs_list_of"))
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

# Check if en object is empty
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.is_empty <- function(x){
  length(x) == 0
}


# Pasting the column name to each value of a dataframe
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.paste_colnames <- function(data, sep = "."){

  data <- as.data.frame(data)

  if(ncol(data) == 1){

    res <- paste0(colnames(data), ".", data[[1]])
    res <- data.frame(x = res, stringsAsFactors = FALSE)
    colnames(res) <- colnames(data)
    return(res)
  }

  res <- apply(data, 1,
               function(row, cname){paste(cname, row, sep = sep)},
               colnames(data)
  ) %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE)
  colnames(res) <- colnames(data)
  res
}


# Bind data list by rows
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# add id columns for a named data list
.rbind_data_list <- function(df.list){

  .names <- names(df.list)

  if(!is.null(.names)){
    df.list <- purrr::map2(df.list, .names, function(df, .name){
      dplyr::mutate(df, id = .name)
    })
  }

  id <- NULL
  res <- dplyr::bind_rows(df.list) %>%
    dplyr::select(id, dplyr::everything())
  res
}



