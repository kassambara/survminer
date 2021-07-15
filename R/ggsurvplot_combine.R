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
#' @param fit a named list of survfit objects.
#' @param data the data frame used to compute survival curves.
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
ggsurvplot_combine <- function(fit, data,
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
      survsummary <- surv_summary(fit, ...)
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
    grouped.d$survsummary <- map(grouped.d$survsummary,
                                 function(x){
                                   x$strata <- as.character(x$strata)
                                   x
                                 })
    all.survsummary <- dplyr::bind_rows(grouped.d$survsummary)
    all.survsummary$strata <- factor(all.survsummary$strata, levels = all.levels)

    # Survplot of combined survsummary data frame
    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    p <- ggsurvplot_df(all.survsummary, ggtheme = ggtheme, ...)
    res <- list(plot = p)


    # The main plot parameters, will be used to plot survival tables
    pms <- attr(p, "parameters")
    surv.color <- pms$color
    pms$risk.table.type <- ifelse(is.null(.dots$risk.table.type),
                                  "absolute", .dots$risk.table.type)

    pms$risk.table.title <- .dots$risk.table.title
    pms$cumevents.title <- .dots$cumevents.title
    pms$cumcensor.title <- .dots$cumcensor.title
    pms$fontsize <- .dots$fontsize
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
        dplyr::select_( .dots = c("strata", "time", "n.risk", "pct.risk",
                                  "n.event", "cum.n.event", "n.censor",
                                  "cum.n.censor", "strata_size"))
    }

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
    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    if(keep.data){
    res$data.survplot <- tibble::as_tibble(all.survsummary)
    res$data.survtable <- tibble::as_tibble(all.survtable)
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


