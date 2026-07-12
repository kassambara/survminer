#' @include ggsurvplot_core.R surv_summary.R ggsurvplot_df.R
NULL
#' Combine a List of Survfit Objects on the Same Plot
#'
#' @description Combine multiple survfit objects on the same plot. For example,
#'   one might wish to plot progression free survival and overall survival on
#'   the same graph (and also stratified by treatment assignment).
#'   \code{ggsurvplot_combine()} provides an extension to the
#'   \code{\link{ggsurvplot}()} function for doing that.
#'
#' @inheritParams ggsurvplot_arguments
#' @param fit a named list whose elements are either survfit objects or
#'   \code{\link{surv_summary}()} data frames (the latter are drawn directly, as
#'   in \code{\link{ggsurvplot_df}()}). Note that the number-at-risk /
#'   cumulative tables and median lines are only available for survfit elements
#'   (they cannot be recomputed from a summary data frame).
#' @param data the data frame used to compute survival curves. Optional; not
#'   needed when \code{fit} contains only \code{surv_summary()} data frames.
#' @param keep.data logical value specifying whether the plot data frame should be kept in the result.
#' Setting these to FALSE (default) can give much smaller results and hence even save memory allocation time.
#' @param ... other arguments to pass to the \code{\link{ggsurvplot}()} function.
#'
#' @examples
#' library(survival)
#
#' # Create a demo data set
#' #::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'  set.seed(123)
#'  demo.data <- data.frame(
#'    os.time = colon$time,
#'    os.status = colon$status,
#'    pfs.time = sample(colon$time),
#'    pfs.status = colon$status,
#'    sex = colon$sex, rx = colon$rx, adhere = colon$adhere
#'  )
#'
#' # Ex1: Combine null models
#' #::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'  # Fit
#'  pfs <- survfit( Surv(pfs.time, pfs.status) ~ 1, data = demo.data)
#'  os <- survfit( Surv(os.time, os.status) ~ 1, data = demo.data)
#'  # Combine on the same plot
#'  fit <- list(PFS = pfs, OS = os)
#'  ggsurvplot_combine(fit, demo.data)
#'
#' # Combine survival curves stratified by treatment assignment rx
#' #::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' # Fit
#' pfs <- survfit( Surv(pfs.time, pfs.status) ~ rx, data = demo.data)
#' os <- survfit( Surv(os.time, os.status) ~ rx, data = demo.data)
#' # Combine on the same plot
#' fit <- list(PFS = pfs, OS = os)
#' ggsurvplot_combine(fit, demo.data)
#'
#'@export
ggsurvplot_combine <- function(fit, data = NULL,
                               risk.table = FALSE, risk.table.pos = c("out", "in"),
                               cumevents = FALSE, cumcensor = FALSE,
                               tables.col = "black", tables.y.text = TRUE, tables.y.text.col = TRUE,
                               ggtheme = theme_survminer(), tables.theme = ggtheme,
                               keep.data = FALSE, risk.table.y.text = tables.y.text,
                               ...)

{
  .dots <- list(...)
  . <- NULL

  # risk.table argument
  risk.table.pos <- match.arg(risk.table.pos)
  risktable <- .parse_risk_table_arg(risk.table)
  risk.table <- risktable$display
  risk.table.type <- risktable$type

  # Check fit: it should be a named list of survfit objects
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  if(!inherits(fit, "list"))
    stop("fit should be a list of survfit")

  fitnames <- names(fit)
  if(is.null(fitnames)){
    fitnames <-  deparse(substitute(fit)) %>%
      as.character() %>% gsub("list\\(|\\)", "", .) %>%
      strsplit(., ",\\s*", perl = TRUE) %>% unlist()
  }
  names(fit) <- fitnames

  # A list element may be a survfit object (the usual input) OR a surv_summary()
  # data frame, which is used directly for the curves -- the data-frame analogue
  # of ggsurvplot_df() (#323). The number-at-risk / cumulative tables and the
  # median lines need the survfit object (they are recomputed at the plot's time
  # breaks), so they are not available from a summary data frame; error clearly
  # rather than failing cryptically downstream.
  has_df_fit <- any(vapply(fit, is.data.frame, logical(1)))
  if(has_df_fit){
    wants.tables <- !isFALSE(risk.table) || !isFALSE(cumevents) || !isFALSE(cumcensor)
    smline <- .dots$surv.median.line
    wants.median <- !is.null(smline) && !identical(smline, "none")
    if(wants.tables || wants.median)
      stop("When `fit` contains a surv_summary() data frame, risk.table, ",
           "cumevents, cumcensor and surv.median.line are not available (they ",
           "require the survfit object). Pass survfit objects for those, or turn ",
           "these options off.", call. = FALSE)
  }

    # Create a list-column data containing the list of fits
    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    grouped.d <- tibble::tibble(name = names(fit)) %>%
      mutate(fit = fit)



    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # Surv summary of each fit
    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # Helper function
    .surv_summary <- function(fit, fitname, ...){
      time <- n.censor <- surv <- upper <- lower <- strata <- NULL
      # A surv_summary() data frame is used as-is; a survfit is summarised (#323).
      # Coerce to a plain data.frame first: a tibble's `df[, col]` returns a
      # one-column tibble that breaks downstream factor()/select() ("cannot xtfrm
      # data frame"), the pitfall documented for surv_group_by/ggadjustedcurves
      # (#501/#548). Also require the surv_summary() columns so a wrong data frame
      # gives a clear error instead of silently producing an empty plot.
      if(is.data.frame(fit)){
        survsummary <- as.data.frame(fit)
        .needed <- c("time", "surv", "upper", "lower", "n.censor")
        .missing <- setdiff(.needed, colnames(survsummary))
        if(length(.missing) > 0)
          stop("A data-frame element of `fit` is missing the surv_summary() ",
               "column(s): ", .collapse(.missing, sep = ", "),
               ". Pass a surv_summary() output (see ?surv_summary).", call. = FALSE)
      }
      else survsummary <- surv_summary(fit, ...)
      # Bind fit name to the srata levels in survsummary
      if(is.null(survsummary$strata)) survsummary$strata <- as.factor(rep("All", nrow(survsummary)))
      strata.levels <- paste(fitname, "::", .levels(survsummary$strata), sep = "")
      survsummary$strata <- paste(fitname, "::", survsummary$strata, sep = "") %>%
        factor(levels = strata.levels)
      survsummary %>%
        dplyr::select(time, n.censor, surv, upper, lower, strata)
    }

    # Surv summary
    grouped.d <- grouped.d %>%
      mutate(survsummary = purrr::map2(grouped.d$fit, grouped.d$name,
                                       .surv_summary, data = data))

    # Row bind all survsummary data
    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    all.levels <- NULL
    for(ss in grouped.d$survsummary )
      all.levels <- c(all.levels, .levels(ss$strata))
    # convert strata into character before binding
    # avoid this warning: Unequal factor levels: coercing to character
    grouped.d$survsummary <- purrr::map(grouped.d$survsummary,
                                 function(x){
                                   x$strata <- as.character(x$strata)
                                   x
                                 })
    all.survsummary <- dplyr::bind_rows(grouped.d$survsummary)
    all.survsummary$strata <- factor(all.survsummary$strata, levels = all.levels)

    # Survplot of combined survsummary data frame
    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    p <- ggsurvplot_df(all.survsummary, ggtheme = ggtheme, ...)

    # Median survival lines. ggsurvplot_df() does not draw them (that is handled
    # in ggsurvplot_core for a single fit); forward surv.median.line here by
    # computing the medians from the list of fits (#316).
    surv.median.line <- .dots$surv.median.line
    if(is.null(surv.median.line)) surv.median.line <- "none"
    surv.median.line <- match.arg(surv.median.line, c("none", "hv", "h", "v"))
    if(surv.median.line %in% c("hv", "h", "v")){
      .fun <- .dots$fun
      if(!is.null(.fun) && .fun %in% c("cumhaz", "cloglog"))
        warning("Adding survival median lines is not allowed when fun is: ", .fun)
      else {
        med_y <- if(!is.null(.fun) && .fun == "pct") 50 else 0.5
        medians <- surv_median(fit, combine = TRUE)$median
        p <- .add_median_lines(p, medians, type = surv.median.line, med_y = med_y)
      }
    }

    res <- list(plot = p)


    # The main plot parameters, will be used to plot survival tables
    pms <- attr(p, "parameters")
    surv.color <- pms$color
    # Honour the risk-table type parsed from the `risk.table` argument (e.g.
    # risk.table = "nrisk_cumcensor"), falling back to an explicit
    # risk.table.type in ... and then to "absolute" (#641).
    pms$risk.table.type <- if(!is.null(.dots$risk.table.type)) .dots$risk.table.type
                           else risk.table.type

    pms$risk.table.title <- .dots$risk.table.title
    pms$cumevents.title <- .dots$cumevents.title
    pms$cumcensor.title <- .dots$cumcensor.title
    # Honour risk.table.fontsize (as ggsurvplot() does), falling back to
    # fontsize (#514).
    pms$fontsize <- if(!is.null(.dots$risk.table.fontsize)) .dots$risk.table.fontsize
                    else .dots$fontsize
    pms$ggtheme <- ggtheme
    pms$ylab <- pms$legend.title
    pms$tables.theme <- tables.theme
    pms$y.text <- risk.table.y.text
    if(tables.y.text.col) pms$y.text.col <- .extract_ggplot_colors (p, grp.levels = pms$legend.labs)
    pms$color <- tables.col
    if(risk.table.pos == "in") pms$color <- surv.color


    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # survival table of each fit: risk.table, ncensor, nevents
    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    time.breaks <- pms$time.breaks
    # Helper function
    .surv_table <- function(fit, fitname, data, times){
      time <- n.censor <- surv <- upper <- lower <- strata <- NULL
      survtable <- .get_timepoints_survsummary(fit, data, times)
      # Bind fit name to the srata levels in survtable
      if(is.null(survtable$strata)) survtable$strata <- as.factor(rep("All", nrow(survtable)))
      strata.levels <- paste(fitname, "::", .levels(survtable$strata), sep = "")
      survtable$strata <- paste(fitname, "::", survtable$strata, sep = "") %>%
        factor(levels = strata.levels)
      survtable %>%
        dplyr::select( dplyr::all_of(c("strata", "time", "n.risk", "pct.risk",
                                       "n.event", "cum.n.event", "n.censor",
                                       "cum.n.censor", "strata_size")))
    }

    # The number-at-risk / cumulative tables are recomputed from each survfit at
    # the plot's time breaks, so they are only available for survfit inputs. For
    # a surv_summary() data-frame input the table options are already refused
    # above, so this whole block is skipped and the survfit path is unchanged.
    all.survtable <- NULL
    if(!has_df_fit){
      grouped.d <- grouped.d %>%
        mutate(survtable = purrr::map2(grouped.d$fit, grouped.d$name,
                                       .surv_table, data = data, times = time.breaks))

      # Row bind all survival table
      #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      all.levels <- NULL
      for(ss in grouped.d$survtable )
        all.levels <- c(all.levels, .levels(ss$strata))
      # convert strata into character before binding
      # avoid this warning: Unequal factor levels: coercing to character
      grouped.d$survtable <- map(grouped.d$survtable,
                                 function(x){
                                   x$strata <- as.character(x$strata)
                                   x
                                 })
      all.survtable <- dplyr::bind_rows(grouped.d$survtable)
      all.survtable$strata <- factor(all.survtable$strata, levels = all.levels)

       # Risk table
      #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      pms$fit <- list(table = all.survtable, time = all.survsummary$time)
      tables <- do.call(ggsurvtable, pms)

      if(risk.table)
        res$table <- tables$risk.table
      if(cumevents)
        res$cumevents <- tables$cumevents
      if(cumcensor)
        res$ncensor.plot <- tables$cumcensor
    }
    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    if(keep.data){
    res$data.survplot <- tibble::as_tibble(all.survsummary)
    if(!is.null(all.survtable)) res$data.survtable <- tibble::as_tibble(all.survtable)
    }


    # Defining attributs for ggsurvplot
    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    tables.height <- ifelse(is.null(.dots$tables.height), 0.25, .dots$tables.height)
    surv.plot.height <- ifelse(is.null(.dots$surv.plot.height), 0.75, .dots$surv.plot.height)
    heights <- list(
      plot =  surv.plot.height,
      table =  ifelse(risk.table, tables.height, 0),
      ncensor.plot = ifelse(cumcensor, tables.height, 0),
      cumevents = ifelse(cumevents, tables.height, 0)
    )
    y.text <- list(
      table =  tables.y.text,
      cumevents = tables.y.text,
      cumcensor = tables.y.text
    )
    y.text.col <- list(
      table =  tables.y.text.col,
      cumevents = tables.y.text.col,
      cumcensor = tables.y.text.col
    )

    class(res) <- c("ggsurvplot", "ggsurv", "list")
    attr(res, "heights") <- heights
    attr(res, "y.text") <- y.text
    attr(res, "y.text.col") <- y.text.col
    attr(res, "legend.position") <- pms$legend
    attr(res, "legend.labs") <- pms$legend.labs
    attr(res, "cumcensor") <- cumcensor
    attr(res, "risk.table.pos") <- risk.table.pos


    res
  }


