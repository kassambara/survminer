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
#' @importFrom utils capture.output
#' @importFrom rlang !! sym


# Check if an installed package version is superior to a specified version
# Version, pkg: character vector
is_pkg_version_sup<- function(pkg, version){
  vv <- as.character(utils::packageVersion(pkg))
  cc <- utils::compareVersion(vv, version) > 0
  cc
}

# TRUE for ggplot2 >= 4.0.0. In 4.x theme elements are S7 objects, and ggtext's
# element_markdown() (still an old-style S3 element) can no longer be merged with
# a user-supplied element_text() ("Can't merge the `axis.text.y` theme element",
# #557). ggplot2 4.x does support a per-label colour vector in element_text(), so
# on 4.x we colour risk-table y labels with element_text() instead of
# element_markdown(); < 4.0 keeps element_markdown() (which merges fine there).
.ggplot2_ge_4 <- function(){
  utils::compareVersion(as.character(utils::packageVersion("ggplot2")), "4.0.0") >= 0
}

# Count the number of ggplots in a list
.count_ggplots <- function(list.objects){
  nplot <- 0
  for(i in 1:length(list.objects)){
    if(ggplot2::is_ggplot(list.objects[[i]])) nplot <- nplot +1
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
                                  if (!is.null(data$linewidth)) data$linewidth <- data$linewidth[1]
                                  if (!is.null(data$linetype)) data$linetype <- data$linetype[1]
                                  ggplot2::GeomRibbon$draw_group(data, panel_scales, coord, na.rm = na.rm)
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
#                                   ggplot2::GeomRibbon$draw_group(data, panel_scales, coord, na.rm = FALSE)
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
# Replacement of deprecated tidyr::gather_() by pivot_longer()
# Use col_vary = "slowest" to keep the same behavior as gather_()
.flat <- function(x) {
  if (is.null(x)) return(NA)
  x <- as.data.frame(x)
  x <- tidyr::pivot_longer(x,
                           cols = colnames(x), 
                           names_to = "key",
                           values_to = "value",
                           cols_vary = "slowest")

  x$value
}


# extract dataset if not provided
.get_data <- function(fit, data = NULL, complain = TRUE) {
  if(is.null(data)){
    if (complain)
      warning ("The `data` argument is not provided. Data will be extracted from model fit.")
    # Re-derive the data from the stored call. When the fit was created in a
    # non-global environment (e.g. inside a function or a {targets} pipeline),
    # the referenced object is out of scope at plot time and eval() throws a
    # cryptic "object '<name>' not found". A survfit object stores no reference
    # to its creation environment, so the data cannot be recovered here; turn
    # the cryptic failure into an actionable message (keeping the original
    # error as context) telling the user to pass `data` explicitly (#521).
    data <- tryCatch(
      eval(fit$call$data),
      error = function(e) stop(
        "The `data` used to fit the model could not be extracted automatically. ",
        "This happens when the model was fitted in a non-global environment ",
        "(e.g. inside a function or a {targets} pipeline). ",
        "Please provide it explicitly, e.g. `ggsurvplot(fit, data = mydata)`.\n",
        "Original error: ", conditionMessage(e),
        call. = FALSE
      )
    )
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

  # Percentage at risk. The denominator is normally the (unweighted) stratum size
  # fit$n. For a WEIGHTED survfit, n.risk is weighted and can exceed fit$n, which
  # made pct.risk > 100% (#561). Only in that case fall back to the weighted
  # number at risk at the origin (a proper denominator that makes the curve start
  # at 100%). For any UNWEIGHTED fit n.risk <= fit$n always -- including
  # left-truncated Surv(start, stop) data -- so the guard never fires and the
  # output is byte-identical to before.
  pct.denom <- strata_size
  if (any(survsummary$n.risk > strata_size, na.rm = TRUE)) {
    n0.risk <- summary(fit, times = 0, extend = TRUE)$n.risk
    if (is.null(fit$strata))
      pct.denom <- rep(n0.risk[1], length(.strata))
    else
      pct.denom <- rep(n0.risk, each = length(.strata)/nstrata)
  }

  strata <- .clean_strata(.strata, fit)
  res <- data.frame(
    strata = strata,
    time = survsummary$time,
    n.risk = round(survsummary$n.risk, digits = decimal.place),
    pct.risk = round(survsummary$n.risk*100/pct.denom),
    n.event = round(survsummary$n.event, digits = decimal.place),
    cum.n.event = round(unlist(by(survsummary$n.event, strata, cumsum)), digits = decimal.place),
    n.censor = round(survsummary$n.censor, digits = decimal.place),
    cum.n.censor = round(unlist(by(survsummary$n.censor, strata, cumsum)), digits = decimal.place),
    strata_size = strata_size
  )

  if(!is.null(fit$strata)){
    variables <- .get_variables(res$strata, fit, data)
    for(variable in variables) res[[variable]] <- .get_variable_value(variable, res$strata, fit, data)
  }
  rownames(res) <- 1:nrow(res)
  res
}

# Strata variable names, taken from the survfit FORMULA (not by splitting the
# strata strings). survival builds each strata name as "var1=level1, var2=level2"
# from these exact term labels, so knowing them lets us split a stratum only at
# the "var=" positions -- levels may then safely contain "=", ",", "$", ">", etc.
# (#291, #430, #599, #616, #680). Returns NULL when the names can't be recovered
# (e.g. survfit.cox / survfit(coxph, newdata)), so callers fall back to the legacy
# string-splitting behavior and stay byte-identical for those objects.
# -----------------------------------------
.strata_variable_names <- function(fit){
  if(missing(fit) || is.null(fit)) return(NULL)
  if(inherits(fit, c("survfit.cox", "survfitcox"))) return(NULL)
  tl <- tryCatch(
    attr(stats::terms(.get_fit_formula(fit)), "term.labels"),
    error = function(e) NULL
  )
  if(length(tl) == 0) return(NULL)
  tl
}

# Escape regex metacharacters so a (possibly non-syntactic) variable name can be
# used literally inside a pattern.
.escape_regex <- function(x){
  gsub("([.\\\\|()\\[\\]{}^$*+?-])", "\\\\\\1", x, perl = TRUE)
}

# HTML-escape the ampersand and angle brackets so text rendered by ggtext's
# element_markdown()/gridtext is shown literally instead of being parsed as an
# HTML/markdown tag. A risk-table label beginning with ">" (e.g. "> 1 Risk
# factor") was otherwise read as a <blockquote> and errored (#532). Order
# matters: escape "&" first so we don't double-escape the "&lt;"/"&gt;" we add.
.escape_markdown <- function(x){
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

# Split each survfit strata string "V1=L1, V2=L2, ..." into its level values,
# scoping the split to the KNOWN variable names (in formula order) so a level may
# contain "=", ",", "$", ">", etc. without being mis-split. Returns a data frame
# with one column per varname (in `varnames`) and one row per stratum.
# -----------------------------------------
.split_strata <- function(strata, varnames){
  strata <- as.character(strata)
  k <- length(varnames)
  prefixes <- paste0("^", .escape_regex(varnames), "=")
  if(k == 1){
    out <- data.frame(sub(prefixes[1], "", strata), stringsAsFactors = FALSE)
    names(out) <- varnames
    return(out)
  }
  # Split only at ", " that is immediately followed by a known "var=" token.
  alt <- paste0("(?:", paste(.escape_regex(varnames), collapse = "|"), ")")
  split_re <- paste0(",\\s+(?=", alt, "=)")
  rows <- lapply(strsplit(strata, split_re, perl = TRUE), function(parts){
    vals <- rep(NA_character_, k)
    used <- rep(FALSE, k)
    for(p in parts){
      for(i in seq_len(k)){
        if(!used[i] && grepl(prefixes[i], p)){
          vals[i] <- sub(prefixes[i], "", p)
          used[i] <- TRUE
          break
        }
      }
    }
    vals
  })
  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(out) <- varnames
  out
}

# Get variable names in strata
# -----------------------------------------
# strata: a vector
# fit: survfit object
# data: data used to fit survival curves
.get_variables <- function(strata, fit, data = NULL){
  variables <- .strata_variable_names(fit)
  if(is.null(variables)){
    # Legacy fallback (no recoverable formula): split the strata strings.
    variables <- sapply(as.vector(strata),
                        function(x){
                          x <- unlist(strsplit(x, "=|,\\s+", perl=TRUE))
                          x[seq(1, length(x), 2)]
                        })
    variables <- unique(as.vector(variables))
  }
  # For the data$variable fit form the term label is "data$var" while the actual
  # column is "var"; strip any "prefix$" so it still matches a data column (the
  # strata itself is dollar-cleaned by .clean_strata). No-op for ordinary names.
  variables <- sub("^.*\\$", "", variables)
  variables <- intersect(variables, colnames(.get_data(fit, data) ))
  variables
}

# levels of a given variable used in survfit formula
# ----------------------------
# variable: variable name
.get_variable_value <- function(variable, strata, fit, data = NULL){
  varnames <- .strata_variable_names(fit)
  if(!is.null(varnames) && variable %in% varnames){
    # Robust path: read the level out of the "variable=" block only. survival
    # right-pads level names to a common width in strata labels, so trim (as the
    # legacy path did) before matching against the factor levels.
    res <- .trim(.split_strata(strata, varnames)[[variable]])
  } else {
    # Legacy fallback (no recoverable formula, or a transformed term).
    res <- sapply(as.vector(strata), function(x){
      x <- unlist(strsplit(x, "=|(\\s+)?,\\s+", perl=TRUE))
      # When a factor name is the same as one of its level, index is of length 2
      index <- grep(paste0("^", variable, "$"), x)[1]
      .trim(x[index+1])
    })
  }
  res <- as.vector(res)
  var_levels <- levels(.get_data(fit, data)[[variable]])
  if(!is.null(var_levels)){
    # survival right-pads level names in strata labels, so `res` was trimmed;
    # match it against the (also trimmed) factor levels but keep the ORIGINAL
    # levels as the values, so a level with genuine surrounding whitespace (e.g.
    # "Obs, ") still matches instead of becoming NA (#616). For ordinary levels
    # (no surrounding space) this is identical to factor(res, levels=var_levels).
    res <- factor(var_levels[match(.trim(res), .trim(var_levels))], levels = var_levels)
  }
  else res <- as.factor(res)
  res
}


# remove dollar sign ($) in strata
# ---------------------------------
# remove dollar sign ($) in strata, in the situation, where
# the user uses data$variable to fit survival curves.
# Only the genuine data$variable form should have its "data$" prefix stripped:
# that form puts a "$" in the formula term label. A "$" that merely appears inside
# a factor LEVEL (e.g. a LaTeX-style label "Marker $<50\\%$") must NOT trigger
# stripping, which previously mangled the strata and reordered other variables in
# the legend (#680). When the formula isn't recoverable (fit missing/cox), fall
# back to the legacy heuristic (a "$" anywhere in the first stratum).
.clean_strata <- function(strata, fit){
  have_fit <- !missing(fit) && !is.null(fit)
  varnames <- if(have_fit) .strata_variable_names(fit) else NULL
  strip_dollar <- if(!is.null(varnames)) any(grepl("$", varnames, fixed = TRUE))
                  else grepl("$", as.character(strata)[1], fixed = TRUE)
  if(isTRUE(strip_dollar)) {
    strata <- as.character(strata)
    data_name <- unlist(strsplit(strata[1], "$", fixed =TRUE))[1]
    strata <- gsub(paste0(data_name, "$"), "", strata, fixed=TRUE)
    strata <- as.factor(strata)
  }
  else if(have_fit && !is.null(fit$strata)) strata <- factor(strata, levels = names(fit$strata))
  return(strata)
}


# Set large dash as y tick labels when ytext = FALSE
# Each dash corresponds to a strata
# This is used for tables under the main survival plots
#
.set_large_dash_as_ytext <- function(ggp, size = NULL){
  # `size = 50` is passed when the table is first built (ggsurvtable), giving the
  # large dash. When re-applied at print time (print.ggsurvplot) `size` is omitted,
  # so the size already on the element is kept -- otherwise a user's
  # `p$table <- p$table + theme(axis.text.y = element_markdown(size = ...))` would
  # be overwritten back to 50 (#642). A non-customized table still carries the
  # build-time 50, so the default is byte-identical.
  if(is.null(size)){
    size <- ggp$theme$axis.text.y$size
    if(is.null(size)) size <- 50
  }
  ggp + theme(axis.text.y = ggtext::element_markdown(size = size, vjust = 0.5),
        axis.ticks.y = element_blank())
}

# Colour the (risk) table y tick labels per strata with a colour vector.
# On ggplot2 >= 4.0 use element_text() -- it accepts a per-label colour vector and,
# unlike ggtext's element_markdown(), can be merged with a user element_text() when
# customizing the table (#557). ggplot2 4.x prints a one-time "Vectorized input to
# element_text() is not officially supported" note at theme-assembly time; it is
# muffled here so it doesn't reach users on every render (the assembled element is
# stored and does not re-warn at draw time). On ggplot2 < 4.0, where element_text()
# has no colour vector, keep element_markdown() (which merges fine there).
# markdown = TRUE forces element_markdown() (used on ggplot2 < 4.0, and on 4.x when
# y.text = FALSE where the labels are already an element_markdown() large dash that
# an element_text() could not be merged onto).
.set_ytext_colour <- function(ggp, colour, markdown = !.ggplot2_ge_4()){
  if(markdown){
    ggp + theme(axis.text.y = ggtext::element_markdown(colour = colour))
  } else {
    withCallingHandlers(
      ggp + theme(axis.text.y = ggplot2::element_text(colour = colour)),
      warning = function(w){
        if(grepl("Vectorized input to", conditionMessage(w), fixed = TRUE))
          invokeRestart("muffleWarning")
      }
    )
  }
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
  .d <- g$data[[1]]
  # One colour per group, in group order. Using unique() here would collapse
  # duplicated palette colours and return a vector shorter than the number of
  # groups, so naming it with grp.levels then failed with "'names' attribute
  # [n] must be the same length as the vector [m]" (#397, #519, #595, #691).
  # For a palette of distinct colours this yields the same result as before.
  .cols <- .d$colour[match(sort(unique(.d$group)), .d$group)]
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
  # fit$call$formula may be an unevaluated symbol when the fit was built from a
  # formula stored in a variable (e.g. survfit(frm, data)); as.formula() then
  # fails with "object of type 'symbol' is not subsettable". stats::formula()
  # resolves it via survival's method; fall back to the old path if unavailable.
  # If BOTH fail -- the formula variable was in a non-global scope (a function,
  # lapply()/nest_by(), a {targets} pipeline) that is gone by plot time, and a
  # survfit stores no reference to its creation environment -- raise an
  # actionable message pointing to surv_fit() instead of the cryptic error
  # (#533; symbol-formula cluster #324/#436).
  tryCatch(
    stats::formula(fit),
    error = function(e1) tryCatch(
      stats::as.formula(fit$call$formula),
      # e1 (from stats::formula) names the missing formula object, e.g.
      # "object 'form' not found" -- more informative than e2's generic
      # "object of type 'symbol' is not subsettable", so surface e1 as context.
      error = function(e2) stop(
        "The model formula could not be extracted from the survfit object. ",
        "This happens when the model was fitted from a formula stored in a ",
        "variable in a non-global environment (e.g. `survfit(form, data)` inside ",
        "a function, `lapply()`/`nest_by()`, or a {targets} pipeline), whose ",
        "formula object is out of scope at plot time. Build the fit with ",
        "survminer's `surv_fit()` instead of `survival::survfit()` -- it retains ",
        "the formula and data -- e.g. `surv_fit(form, data = data)`.\n",
        "Original error: ", conditionMessage(e1),
        call. = FALSE
      )
    )
  )
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


# Time-axis breaks for the survival plot and the risk/censor tables (#695, #435)
#.........................................................................
# `break.time.by` may be a single numeric step (regular breaks from 0, the
# historical behaviour) OR a numeric vector of explicit break positions, used
# as-is. Using the same value for the curve and the tables keeps their x-axes
# aligned. A length-1 value reproduces the old seq() output exactly.
.time_breaks <- function(break.time.by, max.time){
  if (length(break.time.by) > 1) sort(unique(break.time.by))
  else seq(0, max.time, by = break.time.by)
}

# Y-axis breaks for the number-of-censoring panel (ncensor.plot) (#542)
#.........................................................................
# One break per distinct censoring count crowds the short panel: with many
# distinct counts the integer labels overlap. Keep the original
# one-break-per-count behaviour when there are few (<= 5) distinct counts
# (byte-identical to before), otherwise fall back to ~5 evenly spaced integer
# breaks so the labels don't collide.
.ncensor_y_breaks <- function(n.censor){
  breaks <- sort(unique(n.censor))
  if (length(breaks) > 5) {
    rng <- range(breaks)
    step <- max(1, ceiling(diff(rng) / 4))
    breaks <- seq(rng[1], rng[2], by = step)
  }
  breaks
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

  .formula <- .get_fit_formula(fit)
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



