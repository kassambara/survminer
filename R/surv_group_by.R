#' @include utilities.R
NULL

#' Create a Grouped Dataset for Survival Analysis
#' @description Split a data frame into multiple new data frames based on one or
#'   two grouping variables. The \code{surv_group_by()} function takes an
#'   existing data frame and converts it into a grouped data frame where
#'   survival analysis are performed "by group".
#'
#' @param data a data frame
#' @param grouping.vars  a character vector containing the name of grouping
#'   variables. Should be of length <= 2
#' @return Returns an object of class \code{surv_group_by} which is a
#'   \link[tibble]{tibble} data frame with the following components: \itemize{
#'   \item one column for each grouping variables. Contains the levels. \item a
#'   coumn named "data", which is a named list of data subsets created by the
#'   grouping variables. The list names are created by concatening the levels of
#'   grouping variables. }
#' @examples
#' library("survival")
#' library("magrittr")
#'
#' # Grouping by one variables: treatment "rx"
#' #::::::::::::::::::::::::::::::::::::::::::
#' grouped.d <- colon %>%
#'   surv_group_by("rx")
#'
#' grouped.d # print
#'
#' grouped.d$data # Access to the data
#'
#' # Grouping by two variables
#' #::::::::::::::::::::::::::::::::::::::::::
#' grouped.d <- colon %>%
#'    surv_group_by(grouping.vars = c("rx", "adhere"))
#'    grouped.d
#'
#' @importFrom rlang syms
#' @export
#' @rdname surv_group_by
surv_group_by <- function(data, grouping.vars){

  . <- NULL # used in pipes
  if(length(grouping.vars) > 2)
    stop("grouping.vars should be of length 1 or 2.")

  # Grouping the data ==> list of data sets
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  grouped.d <- dplyr::group_by(.data = data, !!!syms(grouping.vars)) %>%
    tidyr::nest()

  # Ordering the grouped data by the original factor levels
  # We should do this because original level orders are altered by the nest() function
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  if(length(grouping.vars) == 1){
    grp.levels <- .levels(data[, grouping.vars])
    current.order <- grouped.d[, grouping.vars] %>% as.data.frame() %>% .[,1]
    grouped.d <- grouped.d[match(grp.levels, current.order), , drop = FALSE]
  }
  else if(length(grouping.vars) == 2){
    # Order by factor 1 levels and then by factor 2 levels
    grouping.vars.df <- grouped.d[, grouping.vars] %>% as.data.frame()
    grp1.levels.pos <- match(grouping.vars.df[, 1], .levels(grouping.vars.df[, 1]))
    grp2.levels.pos <- match(grouping.vars.df[, 2], .levels(grouping.vars.df[, 2]))
    grouped.d <- grouped.d %>%
      dplyr::arrange(grp1.levels.pos, grp2.levels.pos)
  }

  # Defining names for the list of data sets.
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # names = combination of the levels of the grouping variables
  .names.df <- grouped.d[, grouping.vars, drop = FALSE]
  if(ncol(.names.df) == 1)
    .names <- .paste_colnames(.names.df, sep = ":")[, 1] %>% as.character()
  else if(ncol(.names.df) == 2){
    .names <- .paste_colnames(.names.df, sep = ":")
    .names <- paste(as.character(.names[,1]), as.character(.names[, 2]), sep = ", ")
  }
  names(grouped.d$data) <- .names

  structure(grouped.d, class = c("surv_group_by", class(grouped.d)))
}
