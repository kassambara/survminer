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
#'@param risk.table logical. If TRUE, risk tables are displayed below faceted plots.
#'  Default is FALSE. Risk tables will match the faceting structure of the main plot.
#'  For a 2x2 grid of survival plots, you'll get a 2x2 grid of risk tables directly below.
#'  See \code{\link{ggsurvplot}} for more details.
#'@param risk.table.height the height of the risk table on the grid. Increase
#'  the value when you have many strata. Default is 0.25.
#'@param surv.plot.height the height of the survival plot on the grid. Default
#'  is 0.75.
#'@param tables.y.text logical. Default is TRUE. If FALSE, risk table y axis
#'  tick labels will be hidden.
#'@param tables.col color to be used for risk table. Default value is "black".
#'@param tables.theme function, ggplot2 theme name. Default value is
#'  \link{theme_survminer}.
#'@param fontsize font size to be used for the risk table.
#'@param pval.in.label logical. If TRUE, p-values are added to facet labels.
#'  Each facet displays only its corresponding p-value. Default is FALSE. 
#'  Requires pval = TRUE.
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
#'# Facet with p-values in labels
#'#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'ggsurvplot_facet(fit, colon, facet.by = "rx",
#'                 palette = "jco", pval = TRUE,
#'                 pval.in.label = TRUE)
#'
#'# Facet with risk table
#'#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'ggsurvplot_facet(fit, colon, facet.by = "rx",
#'                 palette = "jco",
#'                 risk.table = TRUE,
#'                 risk.table.height = 0.3)
#'
#'# Note: Risk tables are aligned below each facet panel
#'# For 1 faceting variable (e.g., rx with 3 levels), you get:
#'#  - Row 1: 3 survival plot panels side-by-side
#'#  - Row 2: 3 risk table panels side-by-side (aligned with plots above)
#'
#'# Facet with both risk table and p-values in labels
#'#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'ggsurvplot_facet(fit, colon, facet.by = "rx",
#'                 palette = "jco", 
#'                 pval = TRUE,
#'                 pval.in.label = TRUE,
#'                 risk.table = TRUE)
#'
#'# Facet by two grouping variables: rx and adhere
#'#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'ggsurvplot_facet(fit, colon, facet.by = c("rx", "adhere"),
#'                 palette = "jco", pval = TRUE)
#'
#'# With risk table
#'ggsurvplot_facet(fit, colon, facet.by = c("rx", "adhere"),
#'                 palette = "jco", risk.table = TRUE)
#'
#'# Note: For 2 faceting variables, you get a grid layout:
#'#  - Top: Grid of survival plots (e.g., 3x2 for rx × adhere)
#'#  - Bottom: Grid of risk tables matching the same layout
#'#  Each risk table aligns perfectly with its corresponding survival plot above
#'
#'# Another fit
#'#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'fit2 <- survfit( Surv(time, status) ~ sex + rx, data = colon )
#'ggsurvplot_facet(fit2, colon, facet.by = "adhere",
#'                 palette = "jco", pval = TRUE,
#'                 pval.in.label = TRUE,
#'                 risk.table = TRUE)
#'
#'@export
ggsurvplot_facet <- function(fit, data, facet.by,
                             color = NULL, palette = NULL,
                             legend.labs = NULL,
                             pval = FALSE, pval.method = FALSE, pval.coord = NULL, pval.method.coord = NULL,
                             pval.in.label = FALSE,
                             nrow = NULL, ncol = NULL,
                             scales = "fixed",
                             short.panel.labs = FALSE, panel.labs = NULL,
                             panel.labs.background = list(color = NULL, fill = NULL),
                             panel.labs.font = list(face = NULL, color = NULL, size = NULL, angle = NULL),
                             panel.labs.font.x = panel.labs.font,
                             panel.labs.font.y = panel.labs.font,
                             risk.table = FALSE,
                             risk.table.height = 0.25,
                             surv.plot.height = 0.75,
                             tables.y.text = TRUE,
                             tables.col = "black",
                             tables.theme = theme_survminer(),
                             fontsize = 4.5,
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
    
    # Store the cleaned strata labels for later use
    cleaned_strata_labels <- NULL
    if(is.null(legend.labs)) {
      # Clean up the legend labels from strata.levels
      # strata.levels are like "sex:0; adhere:0" - clean to show just the variables
      cleaned_strata_labels <- gsub(";", ",", strata.levels)
      cleaned_strata_labels <- gsub("\\.strata\\.\\s*[=:]?\\s*", "", cleaned_strata_labels)
      cleaned_strata_labels <- trimws(cleaned_strata_labels)
      legend.labs <- cleaned_strata_labels
    }
    
    # used to compute pvalue for each panel
    .survformula <- .build_formula(surv.obj, ".strata.")

  }


  # Plot survival curves
  #:::::::::::::::::::::::::::::::::::::::::

  # When coloring by .strata., don't pass legend.labs to avoid "unknown labels" message
  # We'll set a cleaner legend later
  plot_legend_labs <- if(!is.null(color) && color == ".strata.") NULL else legend.labs
  
  ggsurv <- ggsurvplot_core(fit, data = data, color = color, palette = palette,
                            legend.labs = plot_legend_labs, ...)
  
  # Clean up the legend if we're coloring by .strata.
  # The legend title will be "Strata" (auto-generated from ".strata.")
  # and we want to hide it or make it cleaner
  if(!is.null(color) && color == ".strata." && !is.null(ggsurv)) {
    # Update legend to show cleaner labels
    # ggsurv$plot contains the ggplot object
    if("ggplot" %in% class(ggsurv)) {
      ggsurv <- ggsurv + ggplot2::labs(color = NULL)  # If ggsurv is a ggplot
    } else if(!is.null(ggsurv$plot) && "ggplot" %in% class(ggsurv$plot)) {
      ggsurv$plot <- ggsurv$plot + ggplot2::labs(color = NULL)  # If ggsurv is a list with $plot
    }
  }

  # Compute pvalues for facets
  #:::::::::::::::::::::::::::::::::::::::::
  pvals.df <- NULL
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
      dplyr::select(!!!syms(facet.by)) %>%
      dplyr::bind_cols(pvalue)
  }

  # Create custom labeller with p-values if requested
  #:::::::::::::::::::::::::::::::::::::::::
  .labeller <- "label_value"
  if(short.panel.labs) .labeller <- label_both
  
  if(pval.in.label && !is.null(pvals.df)) {
    # Create a lookup table for p-values based on facet combinations
    # For 1D faceting: facet.by has 1 variable
    # For 2D faceting: facet.by has 2 variables
    
    # Convert pvals.df to a named vector for reliable lookup
    # Key = facet level, Value = p-value label
    pval_labels <- character()
    
    for(i in 1:nrow(pvals.df)) {
      facet_value <- as.character(pvals.df[[facet.by[1]]][i])
      pval_str <- format(pvals.df$pval[i], digits = 2, scientific = TRUE)
      
      if(short.panel.labs) {
        pval_labels[facet_value] <- paste0(facet_value, "\np=", pval_str)
      } else {
        pval_labels[facet_value] <- paste0(facet.by[1], ": ", facet_value, "\np=", pval_str)
      }
    }
    
    # Create a labeller using the named vector
    # This ensures each facet value maps to exactly one label
    .labeller <- ggplot2::as_labeller(pval_labels)
  }

  # Faceting the main plot
  #:::::::::::::::::::::::::::::::::::::::::

  p <- .facet(ggsurv$plot, facet.by, nrow = nrow, ncol = ncol,
              scales = scales, short.panel.labs = short.panel.labs,
              panel.labs.background = panel.labs.background,
              panel.labs.font = panel.labs.font,
              panel.labs.font.x = panel.labs.font.x,
              panel.labs.font.y = panel.labs.font.y,
              custom.labeller = .labeller)

  # Add pvalues as text on plot (if not in label)
  #:::::::::::::::::::::::::::::::::::::::::
  if(pval && !pval.in.label && !is.null(pvals.df)){
    pval.x <- pval.y <- pval.txt <- method.x <- method.y <- method <-  NULL
    p <- p +
      geom_text(data = pvals.df, aes(x = pval.x, y = pval.y, label = pval.txt),
                hjust = 0)
    if(pval.method)
      p <- p + geom_text(data = pvals.df,  aes(x = method.x, y = method.y, label = method),
                         hjust = 0)
  }

  # Add faceted risk table if requested
  #:::::::::::::::::::::::::::::::::::::::::
  if(risk.table){
    # For the risk table, we need to pass legend.labs that match the refitted model
    # The refitted model has strata for all combinations of .strata. x facet.by
    # To avoid warnings about legend.labs length, we pass NULL and let it use defaults
    risk_legend_labs <- NULL
    
    # Re-call ggsurvplot_core with risk.table = TRUE to get the risk table
    # Suppress warnings and messages about legend.labs length and unknown labels
    # Use suppressWarnings/Messages and also temporarily redirect stderr to catch cli messages
    ggsurv_with_table <- suppressWarnings(suppressMessages({
      # Temporarily suppress cli messages by capturing stderr
      msg_con <- file(tempfile(), open = "wt")
      sink(msg_con, type = "message")
      on.exit({
        sink(type = "message")
        close(msg_con)
      }, add = TRUE)
      
      result <- ggsurvplot_core(fit, data = data, color = color, 
                                palette = palette, legend.labs = risk_legend_labs,
                                risk.table = TRUE,
                                risk.table.height = risk.table.height,
                                surv.plot.height = surv.plot.height,
                                tables.y.text = tables.y.text,
                                tables.col = tables.col,
                                tables.theme = tables.theme,
                                fontsize = fontsize,
                                ...)
      
      result
    }))
    
    if(!is.null(ggsurv_with_table$table)){
      # Clean up the risk table Y-axis labels to remove redundant information
      # The labels come from the strata column which has values like ".strata.=sex:0; adhere:0, rx=Obs"
      # We need to: 1) Remove ".strata.=" prefix, 2) Remove faceting variable (rx)
      
      # Function to clean a single label
      clean_label <- function(label) {
        # Remove .strata. prefix (with or without = or :)
        label <- gsub("\\.strata\\.\\s*[=:]?\\s*", "", label)
        # Replace ; with ,
        label <- gsub(";", ",", label)
        # Split on common delimiters
        parts <- strsplit(label, "[,;]\\s*")[[1]]
        # Trim whitespace from each part
        parts <- trimws(parts)
        # Filter out parts that start with any faceting variable name
        facet_patterns <- paste0("^\\s*(", paste(facet.by, collapse="|"), ")\\s*[=:\\s]")
        parts <- parts[!grepl(facet_patterns, parts, ignore.case = TRUE)]
        # Rejoin remaining parts and trim final result
        if(length(parts) > 0) {
          return(trimws(paste(parts, collapse=", ")))
        } else {
          return(trimws(label))  # Fallback if all parts were filtered
        }
      }
      
      # Get original strata levels from the table data
      if(!is.null(ggsurv_with_table$table$data) && "strata" %in% names(ggsurv_with_table$table$data)) {
        original_levels <- levels(ggsurv_with_table$table$data$strata)
        if(is.null(original_levels)) {
          original_levels <- unique(as.character(ggsurv_with_table$table$data$strata))
        }
        
        # Clean the labels
        cleaned_labels <- sapply(original_levels, clean_label, USE.NAMES = FALSE)
        
        # Update the Y-axis scale with cleaned labels
        # Suppress the message about replacing existing scale
        ggsurv_with_table$table <- suppressMessages(
          ggsurv_with_table$table + 
            ggplot2::scale_y_discrete(labels = setNames(cleaned_labels, original_levels))
        )
      }
      
      # Facet the risk table to match the survival plot structure
      # Use "free_y" and space="free_y" to allow each facet to show only its relevant strata
      # This will automatically hide empty/unused strata levels in each facet panel
      risk_table_plot <- .facet(ggsurv_with_table$table, facet.by, nrow = nrow, ncol = ncol,
                                scales = "free_y",  # Free Y axis scales
                                short.panel.labs = short.panel.labs,
                                panel.labs.background = panel.labs.background,
                                panel.labs.font = panel.labs.font,
                                panel.labs.font.x = panel.labs.font.x,
                                panel.labs.font.y = panel.labs.font.y,
                                custom.labeller = .labeller,
                                space = "free_y")  # Free Y axis space to drop unused levels
      
      # Convert to grobs
      p_grob <- ggplotGrob(p)
      table_grob <- ggplotGrob(risk_table_plot)
      
      # Use gtable_rbind for proper alignment
      # This approach aligns ALL columns (axes, panels, strips, etc.)
      min_ncol <- min(ncol(p_grob), ncol(table_grob))
      
      # Stack the grobs vertically, taking only matching columns
      g <- gridExtra::gtable_rbind(
        p_grob[, 1:min_ncol], 
        table_grob[, 1:min_ncol], 
        size = "max"
      )
      
      # Align all column widths using the maximum width for each column
      g$widths <- grid::unit.pmax(p_grob$widths, table_grob$widths)
      
      # Return the arranged grob
      class(g) <- c("ggsurvplot_facet", "ggsurv", "gtable", class(g))
      return(g)
    }
  }
  
  # Return just the plot if no risk table
  p

}




# Helper function for faceting
.facet <- function(p,  facet.by, nrow = NULL, ncol = NULL,
                   scales = "fixed", short.panel.labs = FALSE,
                   panel.labs.background = list(color = NULL, fill = NULL),
                   panel.labs.font = list(face = NULL, color = NULL, size = NULL, angle = NULL),
                   panel.labs.font.x = panel.labs.font,
                   panel.labs.font.y = panel.labs.font,
                   custom.labeller = NULL,
                   space = "fixed"
                   )
{

  panel.labs.background <- .compact(panel.labs.background)
  panel.labs.font.x <- .compact(panel.labs.font.x)
  panel.labs.font.y <- .compact(panel.labs.font.y)

  .labeller <- "label_value"
  if(short.panel.labs) .labeller <- label_both
  
  # Use custom labeller if provided
  if(!is.null(custom.labeller) && !is.character(custom.labeller)){
    .labeller <- custom.labeller
  }

  if(length(facet.by) == 1){
    facet.formula <- paste0("~", facet.by) %>% stats::as.formula()
    p <- p + facet_wrap(facet.formula, nrow = nrow, ncol = ncol, scales = scales, labeller = .labeller)
  }
  else if(length(facet.by) == 2){
    facet.formula <- paste(facet.by, collapse = " ~ ") %>% stats::as.formula()
    p <- p + facet_grid(facet.formula, scales = scales, labeller = .labeller, space = space)
  }

  if(!.is_empty(panel.labs.background))
    p <- p + theme(strip.background = do.call(element_rect, panel.labs.background))
  if(!.is_empty(panel.labs.font.x))
    p <- p + theme(strip.text.x = do.call(element_text, panel.labs.font.x))
  if(!.is_empty(panel.labs.font.y))
    p <- p + theme(strip.text.y = do.call(element_text, panel.labs.font.y))

  p
}


# Print method for ggsurvplot_facet with risk tables
#' @export
print.ggsurvplot_facet <- function(x, newpage = TRUE, ...) {
  if(newpage) grid::grid.newpage()
  grid::grid.draw(x)
  invisible(x)
}


