#' @include ggsurvplot_core.R
NULL
#'Facet Survival Curves into Multiple Panels
#'
#'@description Draw multi-panel survival curves of a data set grouped by one or
#'  two variables.
#'@inheritParams ggsurvplot_arguments
#'@param facet.by character vector, of length 1 or 2, specifying grouping
#'  variables for faceting the plot. Should be in the data.
#'@param nrow,ncol Number of rows and columns in the pannel. Used only when the
#'  data is faceted by one grouping variable.
#'@param scales should axis scales of panels be fixed ("fixed", the default),
#'  free ("free"), or free in one dimension ("free_x", "free_y").
#'@param short.panel.labs logical value. Default is FALSE. If TRUE, create short
#'  labels for panels by omitting variable names; in other words panels will be
#'  labelled only by variable grouping levels.
#'@param panel.labs a list of one or two character vectors to modify facet label
#'  text. For example, panel.labs = list(sex = c("Male", "Female")) specifies
#'  the labels for the "sex" variable. For two grouping variables, you can use
#'  for example panel.labs = list(sex = c("Male", "Female"), rx = c("Obs",
#'  "Lev", "Lev2") ).
#'@param panel.labs.background a list to customize the background of panel
#'  labels. Should contain the combination of the following elements: \itemize{
#'  \item \code{color, linetype, size}: background line color, type and size
#'  \item \code{fill}: background fill color. } For example,
#'  panel.labs.background = list(color = "blue", fill = "pink").
#'@param panel.labs.font a list of aestheics indicating the size (e.g.: 14), the
#'  face/style (e.g.: "plain", "bold", "italic", "bold.italic") and the color
#'  (e.g.: "red") and the orientation angle (e.g.: 45) of panel labels.
#'@param panel.labs.font.x,panel.labs.font.y same as panel.labs.font but for x
#'  and y direction, respectively.
#'@param ... other arguments to pass to the function \code{\link{ggsurvplot}}.
#' @examples
#' library(survival)
#'
#'# Facet by one grouping variables: rx
#'#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'fit <- survfit( Surv(time, status) ~ sex, data = colon )
#'ggsurvplot_facet(fit, colon, facet.by = "rx",
#'                 palette = "jco", pval = TRUE)
#'
#'# Facet by two grouping variables: rx and adhere
#'#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'ggsurvplot_facet(fit, colon, facet.by = c("rx", "adhere"),
#'                 palette = "jco", pval = TRUE)
#'
#'
#'# Another fit
#'#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'fit2 <- survfit( Surv(time, status) ~ sex + rx, data = colon )
#'ggsurvplot_facet(fit2, colon, facet.by = "adhere",
#'                 palette = "jco", pval = TRUE)
#'
#'@export
ggsurvplot_facet <- function(fit, data, facet.by,
                             color = NULL, palette = NULL,
                             legend.labs = NULL,
                             pval = FALSE, pval.method = FALSE, pval.coord = NULL, pval.method.coord = NULL,
                             nrow = NULL, ncol = NULL,
                             scales = "fixed",
                             short.panel.labs = FALSE, panel.labs = NULL,
                             panel.labs.background = list(color = NULL, fill = NULL),
                             panel.labs.font = list(face = NULL, color = NULL, size = NULL, angle = NULL),
                             panel.labs.font.x = panel.labs.font,
                             panel.labs.font.y = panel.labs.font,
                             ...)
  {

  if(length(facet.by) > 2)
    stop("facet.by should be of length 1 or 2.")

  if(!is.null(panel.labs) & !.is_list(panel.labs))
      stop("Argument panel.labs should be a list. Read the documentation.")

  . <- NULL # used in pipes

  .labeller <- "label_value"
  if(short.panel.labs) .labeller <- label_both
  .dots <- list(...)

  # Extract fit components
  #:::::::::::::::::::::::::::::::::::::::::
  fit.ext <- .extract.survfit(fit, data)
  .formula <- fit.ext$formula
  surv.obj <- fit.ext$surv
  surv.vars <- fit.ext$variables
  all.variables <- c(surv.vars, facet.by) %>%
    unique()
  vars.notin.groupby <- setdiff(all.variables, facet.by)
  data <- fit.ext$data.all

  # Changing panel labs if specified
  #:::::::::::::::::::::::::::::::::::::::::
  if(!is.null(panel.labs)){
    for(.grouping.var in facet.by){
      if(!is.null(panel.labs[[.grouping.var]])){
        if(!is.factor(data[, .grouping.var]))
          data[, .grouping.var] <- as.factor(data[, .grouping.var])
        levels(data[, .grouping.var]) <- panel.labs[[.grouping.var]]
      }
    }
  }

  # If some of facet.by variables are not included in survfit formula,
  # ex: surv ~ sex & facet.by = "rx", we need to refit surv curves with all variables including facet.by
  # Required for faceting: all variables should be present in the survival table
  #
  # If the user decides to change panel labels,  we should also re-comput survival curves
  # to take into account new labels
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  if(!all(facet.by %in% surv.vars) | !is.null(panel.labs)) {
    fit <- .build_formula(surv.obj, all.variables) %>%
      surv_fit(., data = data)
  }

  if(length(vars.notin.groupby) == 1){
    if(is.null(color)) color <- vars.notin.groupby
    .survformula <- .build_formula(surv.obj, vars.notin.groupby)
  }
  else{
    # ex: surv ~ sex + rx + adhere & facet.by = "adhere"
    # by default adhere is included in the legend.
    # But we would like to see sex+rx accross adhere groups
    # We need to clean the legend by removing adhere
    # solution:
    #       1. add a new column ".strata." in the data. ".strata." is a column containing
    #         the combination of the levels of variables that are not in facet.by.
    #       2. ReFit survival curves with .strata. + facet.by variables (here adhere)
    #       3. Color by .strata. and facet by facet.by (here adhere)

    # Create strata using the combination of vars.notin.groupby
    new.strata <- .create_strata(data, vars.notin.groupby, sep = "; ")
    strata.levels <- levels(new.strata)

    # Add strata to data
    data <- data %>%
      dplyr::mutate(.strata. = new.strata)

    # Refit survival curves
    new.surv.vars <- paste(c(".strata.", facet.by), collapse = " + ")
    .new.formula <- .build_formula(surv.obj, new.surv.vars)
    fit <- surv_fit(formula = .new.formula, data = data)

    if(is.null(color)) color <- ".strata."
    if(is.null(legend.labs)) legend.labs <- gsub(";", ",", strata.levels)
    # used to compute pvalue for each panel
    .survformula <- .build_formula(surv.obj, ".strata.")

  }


  # Plot survival curves
  #:::::::::::::::::::::::::::::::::::::::::

  ggsurv <- ggsurvplot_core(fit, data = data, color = color, palette = palette,
                            legend.labs = legend.labs, ...)


  # Faceting the main plot
  #:::::::::::::::::::::::::::::::::::::::::

  p <- .facet(ggsurv$plot, facet.by, nrow = nrow, ncol = ncol,
              scales = scales, short.panel.labs = short.panel.labs,
              panel.labs.background = panel.labs.background,
              panel.labs.font = panel.labs.font,
              panel.labs.font.x =panel.labs.font.x,
              panel.labs.font.y = panel.labs.font.y)

  # Pvalues
  #:::::::::::::::::::::::::::::::::::::::::
  if(pval){
    # Grouped data
    grouped.d <- surv_group_by(data, grouping.vars = facet.by)
    # Compute survfit on each subset ==> list of fits
    sf <- surv_fit(.survformula, grouped.d$data, ...)
    grouped.d <- grouped.d %>% tibble::add_column(fit = sf)
    # Compute pvalue and convert the list of data frame into one data frame
    # by binding rows of the data frame
    pvalue <- surv_pvalue(grouped.d$fit, grouped.d$data, pval.coord = pval.coord,
                          pval.method.coord = pval.method.coord,...) %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble()
    # Select the grouping variable columns and cbind the corresponding pvalue
    pvals.df <- grouped.d %>%
      dplyr::select_( .dots = facet.by) %>%
      dplyr::bind_cols(pvalue)
    pval.x <- pval.y <- pval.txt <- method.x <- method.y <- method <-  NULL
    p <- p +
      geom_text(data = pvals.df, aes(x = pval.x, y = pval.y, label = pval.txt),
                hjust = 0)
    if(pval.method)
      p <- p + geom_text(data = pvals.df,  aes(x = method.x, y = method.y, label = method),
                         hjust = 0)
  }

  # To do
  # facet tables
  # if(!is.null(ggsurv$table))
  # res$table <- .facet(ggsurv$table, facet.by, nrow = nrow, ncol = ncol,
  #                      scales = scales, panel.lab = panel.lab)
  p

}




# Helper function for faceting
.facet <- function(p,  facet.by, nrow = NULL, ncol = NULL,
                   scales = "fixed", short.panel.labs = FALSE,
                   panel.labs.background = list(color = NULL, fill = NULL),
                   panel.labs.font = list(face = NULL, color = NULL, size = NULL, angle = NULL),
                   panel.labs.font.x = panel.labs.font,
                   panel.labs.font.y = panel.labs.font
                   )
{

  panel.labs.background <- .compact(panel.labs.background)
  panel.labs.font.x <- .compact(panel.labs.font.x)
  panel.labs.font.y <- .compact(panel.labs.font.y)

  .labeller <- "label_value"
  if(!short.panel.labs) .labeller <- label_both

  if(length(facet.by) == 1){
    facet.formula <- paste0("~", facet.by) %>% stats::as.formula()
    p <- p + facet_wrap(facet.formula, nrow = nrow, ncol = ncol, scales = scales, labeller = .labeller)
  }
  else if(length(facet.by) == 2){
    facet.formula <- paste(facet.by, collapse = " ~ ") %>% stats::as.formula()
    p <- p + facet_grid(facet.formula, scales = scales, labeller = .labeller)
  }

  if(!.is_empty(panel.labs.background))
    p <- p + theme(strip.background = do.call(element_rect, panel.labs.background))
  if(!.is_empty(panel.labs.font.x))
    p <- p + theme(strip.text.x = do.call(element_text, panel.labs.font.x))
  if(!.is_empty(panel.labs.font.y))
    p <- p + theme(strip.text.y = do.call(element_text, panel.labs.font.y))

  p
}




