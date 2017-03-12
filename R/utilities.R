#' @import ggplot2
#' @import ggpubr


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
  }
  data
}

# Compute default axis breaks as ggplot2
#-------------------------------------
# Return a vector of axis labels
.get_default_breaks <- function(x){
  scales::extended_breaks()(x)
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
.get_timepoints_survsummary <- function(fit, data, times)
{
  survsummary <- summary(fit, times = times, extend = TRUE)

  if (is.null(fit$strata)) {
    .strata <- factor(rep("All", length(times)))
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
    n.risk = survsummary$n.risk,
    pct.risk = round(survsummary$n.risk*100/strata_size),
    n.event = survsummary$n.event,
    cum.n.event = unlist(by(survsummary$n.event, strata, cumsum)),
    n.censor = survsummary$n.censor,
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
    index <- grep(paste0("^", variable, "$"), x)
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
  ggp + theme(axis.text.y = element_text(size = 50, vjust = 0.35),
        axis.ticks.y = element_blank())
}

