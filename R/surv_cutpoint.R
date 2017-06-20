#'Determine the Optimal Cutpoint for Continuous Variables
#'
#'@description Determine the optimal cutpoint for one or multiple continuous
#'  variables at once, using the maximally selected rank statistics from the
#'  'maxstat' R package. This is an outcome-oriented  methods  providing  a
#'  value of a cutpoint that correspond to the most significant relation with
#'  outcome (here, survival).
#'  \itemize{
#'  \item \code{surv_cutpoint()}: Determine the optimal cutpoint for each variable using 'maxstat'.
#'  \item \code{surv_categorize()}: Divide each variable values based on the cutpoint returned by \code{surv_cutpoint()}.
#'  }
#'@param data a data frame containing survival information (time, event) and
#'  continuous variables (e.g.: gene expression data).
#'@param time,event column names containing time and event data, respectively.
#'  Event values sould be 0 or 1.
#'@param variables a character vector containing the names of variables of
#'  interest, for wich we want to estimate the optimal cutpoint.
#'@param minprop the minimal proportion of observations per group.
#'@param progressbar logical value. If TRUE, show progress bar. Progressbar is
#'  shown only, when the number of variables > 5.
#'
#'@return \itemize{
#'
#'  \item{ \bold{surv_cutpoint()}: returns an object of class 'surv_cutpoint',
#'  which is a list with the following components: \itemize{ \item maxstat
#'  results for each variable (see ?maxstat::maxstat) \item cutpoint: a data
#'  frame containing the optimal cutpoint of each variable. Rows are variable
#'  names and columns are c("cutpoint", "statistic"). \item data: a data frame
#'  containing the survival data and the original data for the specified
#'  variables. \item minprop: the minimal proportion of observations per group.
#'  \item not_numeric: contains data for non-numeric variables, in the context
#'  where the user provided categorical variable names in the argument
#'  variables.} } Methods defined for surv_cutpoint object are summary, print
#'  and plot.
#'
#'  \item \bold{surv_categorize()}: returns an object of class
#'  'surv_categorize', which is a data frame containing the survival data and
#'  the categorized variables.
#'
#'  }
#'
#'@author Alboukadel Kassambara, \email{alboukadel.kassambara@@gmail.com}
#'
#'@examples
#' # 0. Load some data
#' data(myeloma)
#' head(myeloma)
#'
#' # 1. Determine the optimal cutpoint of variables
#' res.cut <- surv_cutpoint(myeloma, time = "time", event = "event",
#'    variables = c("DEPDC1", "WHSC1", "CRIM1"))
#'
#' summary(res.cut)
#'
#' # 2. Plot cutpoint for DEPDC1
#' # palette = "npg" (nature publishing group), see ?ggpubr::ggpar
#' plot(res.cut, "DEPDC1", palette = "npg")
#'
#' # 3. Categorize variables
#' res.cat <- surv_categorize(res.cut)
#' head(res.cat)
#'
#' # 4. Fit survival curves and visualize
#' library("survival")
#' fit <- survfit(Surv(time, event) ~DEPDC1, data = res.cat)
#' ggsurvplot(fit, data = res.cat, risk.table = TRUE, conf.int = TRUE)
#'
#'@rdname surv_cutpoint
#'@export
surv_cutpoint <- function(data, time = "time", event = "event", variables,
                    minprop = 0.1, progressbar = TRUE)
  {
  if(!inherits(data, "data.frame"))
    stop("data should be an object of class data.frame")
  data <- as.data.frame(data)
  if(!all(c(time, event) %in% colnames(data)))
    stop("Specify correct column names containing time and event values.")
  if(!all(variables %in% colnames(data)))
    stop("Some variables are not found in the data: ",
         paste(setdiff(variables, colnames(data)), collapse =", "))

  not_numeric_vars <- .get_not_numeric_vars(data[, variables, drop = FALSE])
  variables <- setdiff(variables, not_numeric_vars) # keep only numeric variables
  if(length(variables)==0) stop("At least, one numeric variables required.")

  nvar <- length(variables)
  if(nvar <= 5) progressbar <- FALSE
  if(progressbar) pb <- utils::txtProgressBar(min = 0, max = nvar, style = 3)
  surv_data <- data.frame(time = data[, time], event = data[, event])
  res <- list()
  for (i in 1:nvar){
    var_i <- variables[i]
    surv_data$var <- data[, var_i]
    max_stat_i <- maxstat::maxstat.test(survival::Surv(time, event) ~ var, data = surv_data,
                                      smethod = "LogRank", pmethod="none",
                                      minprop = minprop, maxprop = 1-minprop,
                                      alpha = alpha)
    res[[var_i]] <- max_stat_i
    if(progressbar) utils::setTxtProgressBar(pb, i)
  }
  colnames(surv_data) <- c(time, event)
  res$data <- cbind.data.frame(surv_data[, 1:2, drop = FALSE], data[, variables, drop = FALSE])
  res$minprop <- minprop
  if(!is.null(not_numeric_vars)) res$not_numeric <- data[, not_numeric_vars, drop = FALSE]
  res <- structure(res, class = c("list", "surv_cutpoint"))
  res$cutpoint <- summary(res)
  res
}


#' @param x,object an object of class surv_cutpoint
#' @param labels labels for the levels of the resulting category.
#' @rdname surv_cutpoint
#' @export
surv_categorize <- function(x,  variables = NULL, labels = c("low", "high")){
  if(!inherits(x, "surv_cutpoint"))
    stop("x must be an object of class surv_cutpoint.")

  data <- x$data
  surv_data <- x$data[, 1:2]
  data <- x$data[, -1*c(1:2), drop = FALSE]
  not_numeric <- x$not_numeric

  if(is.null(variables)) variables <- colnames(data)
  data <- data[, variables, drop = FALSE]
  cutpoints <- x$cutpoint[variables,"cutpoint"]
  nvar <- length(variables)
  if(nvar >=2){
    res <- apply(t(data), 2, .dichotomize, cutpoints, labels)
    res <- as.data.frame(t(res))
    colnames(res) <- variables
  }
  else {
    res <- data
    res[, 1] <- .dichotomize(res[, 1], cutpoints, labels)
  }
  res <- cbind.data.frame(surv_data, res)
  if(!is.null(not_numeric)) res <- cbind.data.frame(res, not_numeric)
  attr(res, "labels") <- labels
  structure(res, class = c("data.frame", "surv_categorize"))
}






#' @param ... other arguments. For plots, see ?ggpubr::ggpar
#' @method summary surv_cutpoint
#' @rdname surv_cutpoint
#' @export
summary.surv_cutpoint <- function(object, ...){

  if(!inherits(object, "surv_cutpoint"))
    stop("x must be an object of class surv_cutpoint.")

  # Extract data
  data <- object$data[, -1*c(1:2), drop = FALSE]
  object <- object[colnames(data)] # keep only variables
  res <- lapply(object,
               function(x){
                 cutpoint <- x$estimate
                 statistic <- x$statistic
                 rr <- c(cutpoint, statistic)
                 names(rr) <- c("cutpoint", "statistic")
                 return(rr)
               })
  res <- t(as.data.frame(res))
  as.data.frame(res)
}

#' @method print surv_cutpoint
#' @rdname surv_cutpoint
#' @export
print.surv_cutpoint <- function(x, ...){

  if(!inherits(x, "surv_cutpoint"))
    stop("x must be an object of class surv_cutpoint.")
  print(x$cutpoint)
}


#' @param ggtheme function, ggplot2 theme name. Default value is
#'   \link{theme_classic}. Allowed values include ggplot2 official themes. see
#'   ?ggplot2::ggtheme.
#' @param bins Number of bins for histogram. Defaults to 30.
#' @method plot surv_cutpoint
#' @rdname surv_cutpoint
#' @export
plot.surv_cutpoint <- function(x, variables = NULL, ggtheme = theme_classic(), bins = 30, ...)
  {

  if(!inherits(x, "surv_cutpoint"))
    stop("x must be an object of class surv_cutpoint.")

  data <- x$data
  surv_data <- x$data[, 1:2]
  data <- x$data[, -1*c(1:2), drop = FALSE]
  if(is.null(variables)) variables <- colnames(data)
  data <- data[, variables, drop = FALSE]
  cutpoints <- x$cutpoint[variables,"cutpoint"]
  nvar <- length(variables)

  p <- list()
  for(variable in variables){
    max_stat <- x[[variable]]

    p_data <- data.frame(
      stats = max_stat$stats,
      cuts = max_stat$cuts,
      grps = .dichotomize(max_stat$cuts, max_stat$estimate)
     )

    vline_df <- data.frame(x1 = max_stat$estimate, x2 = max_stat$estimate,
                           y1 = 0, y2 = max(p_data$stats))
    cutpoint_label <- paste0("Cutpoint: ", round(max_stat$estimate,2))
    x1 <- y1 <- x2 <- y2 <- NULL
    max_stat_p <- ggplot(data = p_data, mapping=aes_string("cuts", "stats"))+
      geom_point(aes_string(color = "grps"), shape = 19, size = 0.5)+
      geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
                   data = vline_df, linetype = "dashed", size = 0.5)+
      ggplot2::annotate("text", x = max_stat$estimate, y = 0.5,
                        label = cutpoint_label, size = 4)+
      labs(y = "Standardized Log-Rank Statistic",
           x = variable, title = "Maximally Selected Rank Statistics")
    max_stat_p <- .ggpar(max_stat_p, gggtheme = ggtheme, ...)

    distribution <- ggpubr::gghistogram(p_data, x = "cuts", fill = "grps",
                                        main = "Distribution", ylab = "Density",
                                        xlab = variable, add_density = TRUE, bins = bins)
    distribution <- .ggpar(distribution, gggtheme = ggtheme, ...)
    res <- list(maxstat = max_stat_p, distribution = distribution)

    attr(res, "name") <- variable
    attr(res, "cutpoint") <- max_stat$estimate
    res <- structure(res, class = c("list", "plot_surv_cutpoint", "ggsurv"))

    p[[variable]] <- res
  }
  p
}

#' @param newpage open a new page. See \code{\link{grid.arrange}}.
#' @method print plot_surv_cutpoint
#' @rdname surv_cutpoint
#' @export
print.plot_surv_cutpoint <- function(x, ..., newpage = TRUE){
  if(!inherits(x, "plot_surv_cutpoint"))
    stop("x must be an object of class plot_surv_cutpoint.")
  x$distribution <- x$distribution  + theme(legend.position = "none")+labs(x = NULL)

  plots <- rev(x)
  grobs <- widths <- list()

  for (i in 1:length(plots)) {
    grobs[[i]] <- ggplotGrob(plots[[i]])
    widths[[i]] <- grobs[[i]]$widths[2:5]
  }
  maxwidth <- do.call(grid::unit.pmax, widths)
  for (i in 1:length(grobs)) {
    grobs[[i]]$widths[2:5] <- as.list(maxwidth)
  }

  cutpoint <- attr(x, "cutpoint")
  main <- name <- attr(x, "name")
  # main <- paste0(name,"- cutpoint: ", attr(x, "cutpoint"))
  do.call(gridExtra::grid.arrange, c(grobs, top = main, newpage = newpage))
}


# Helper function
# %%%%%%%%%%%%%%%%%%%%%

.ggpar <- function(p, ggtheme = theme_classic(),...){
  argmt <- list(...)
  p <- ggpubr::ggpar(p, ggtheme = ggtheme,...)
  if(is.null(argmt$font.x)) p <- p + theme(axis.text.x = element_text(face = "plain"))
  if(is.null(argmt$font.y)) p <- p + theme(axis.text.y = element_text(face = "plain"))
  p
}

# Helper functions
#+++++++++++++++++++++++++++++++++
.dichotomize <- function(x, cutpoint, labels = c("low", "high")){
  grps <- x
  grps[x <= cutpoint] = labels[1]
  grps[x > cutpoint] = labels[2]

  grps
}

# Get not numeric columns in a data.frame
.get_not_numeric_vars <- function(data_frame){
  is_numeric <- sapply(data_frame, is.numeric)
  if(sum(!is_numeric) == 0) res = NULL
  else res <- colnames(data_frame[, !is_numeric, drop = FALSE])
  res
}



