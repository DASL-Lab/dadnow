#' check validity of DAD data
#'
#' @param dad
#'
#' @returns Logical value indicating whether the data is valid
#' @export
#'
check_dad_data <- function(dad) {

  required_cols = c("date", "virus", "geo", "count", "percapita")
  missing_cols = setdiff(required_cols, colnames(dad))
  if (length(missing_cols) > 0) {
    stop(paste("DAD data is missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  if (!inherits(dad$date, "Date")) {
    stop("The 'date' column in DAD data must be of Date type.")
  }

  if (!is.numeric(dad$count) || !is.numeric(dad$percapita)) {
    stop("The 'count' and 'percapita' columns in DAD data must be numeric.")
  }

  if (any(dad$count < 0, na.rm = TRUE)) {
    stop("The 'count' column in DAD data contains negative values.")
  }
  return(TRUE)
}


#' Check validity of explanatory data
#'
#' @param x a data frame containing explanatory data
#'
#' @returns Logical value indicating whether the data is valid
#' @export
#'
check_expl_data <- function(x) {
  required_cols = c("date", "virus", "geo")
  missing_cols = setdiff(required_cols, colnames(x))
  if (length(missing_cols) > 0) {
    stop(paste("Expl data is missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  if (!inherits(x$date, "Date")) {
    stop("The 'date' column in expl data must be of Date type.")
  }
  return(TRUE)
}


#' Check if the variable name for DAD data is valid
#'
#' @param varname.dad String. Must be either "count" or "percapita".
#'
#' @returns Logical value indicating whether the variable name is valid
#'
check_varname_dad <- function(varname.dad) {
  if (!varname.dad %in% c("count", "percapita")) {
    stop("varname.dad must be either 'count' or 'percapita'")
  }
  return(TRUE)
}

#' Check if the variable name for explanatory data is valid
#'
#' @param data.expl Dataframe of explanatory data
#' @param varname.expl String. Name of the variable in data.expl to use as an explanatory variable.
#'
#' @returns Logical value indicating whether the variable name is valid
#'
check_varname_expl <- function(data.expl, varname.expl) {
  if (!varname.expl %in% colnames(data.expl)) {
    stop(paste("varname.expl must be one of the columns in data.expl:", paste(colnames(data.expl), collapse = ", ")))
  }
  return(TRUE)
}

#' Check if the date range is valid
#'
#' @param date.fit.range Vector of two dates.
#' The first date must be earlier than the second date.
#'
check_date_range_fit <- function(d) {
  if (length(d) != 2) {
    stop("`date.range.fit` must be a vector of two dates.")
  }
  if (!inherits(d[1], "Date") || !inherits(d[2], "Date")) {
    stop("Both elements of `date.range.fit` must be of Date type.")
  }
  if (d[1] >= d[2]) {
    stop("The first date in `date.range.fit` must be earlier than the second date.")
  }
  return(TRUE)
}


