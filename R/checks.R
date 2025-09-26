#' check validity of DAD data
#'
#' @param dad
#'
#' @returns Logical value indicating whether the data is valid
#' @keywords internal
#'
check_dad_data <- function(dad) {

  required_cols = c("date", "virus", "geo")
  check.colnames = all(required_cols %in% names(dad))

  if (!check.colnames) {
    stop(paste("DAD data must have columns:", paste(required_cols, collapse = ", ")))
  }

  if (!inherits(dad$date, "Date")) {
    stop("The 'date' column in DAD data must be of Date type.")
  }

  return(TRUE)
}


#' Check validity of explanatory data
#'
#' @param x a data frame containing explanatory data
#'
#' @returns Logical value indicating whether the data is valid
#' @keywords internal
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
#' @keywords internal
#'
check_varname_dad <- function(varname.dad) {
  if (!varname.dad %in% c("count", "percapita")) {
    stop("varname.dad must be either 'count' or 'percapita'")
  }
  return(TRUE)
}

#'  Check if the variable name is present in the dataframe.
#'
#' @param data Dataframe of  data
#' @param varname String. Name of the variable in data to use as a variable.
#'
#' @returns Logical value indicating whether the variable name is valid
#' @keywords internal
#'
check_varname <- function(data, varname) {
  if (!varname %in% colnames(data)) {
    stop(paste("Variable name '",varname,
               "' not a variable of data; existing column names are:",
               paste(colnames(data), collapse = ", ")))
  }
  return(TRUE)
}

#' Check if the date range is valid
#'
#' @param date.fit.range Vector of two dates.
#' The first date must be earlier than the second date.
#' @keywords internal
#'
check_date_range_fit <- function(date.fit.range) {
  d = date.fit.range
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

#' Check the validity of a fitted model
#'
#' @param fitted.model A list containing the fitted model
#' and its associated data.
#'
#' @returns Logical value indicating whether the fitted model is valid.
#' @keywords internal
#'
check_fitted_model <- function(fitted.model) {
  if (!is.list(fitted.model)) {
    stop("fitted.model must be a list.")
  }
  check1 = all(names(fitted.model) %in% c(
    "model", "data", "family",
    "varname.dad", "varname.expl",
    "date.range.fit"))
  if (!check1) {
    stop("fitted.model must contain the following elements: 'model', 'data', 'family', 'varname.dad', 'varname.expl', and 'date.range.fit'.")
  }
  return(TRUE)
}

#' Check the compatibility of the new exploratory data
#' and the fitted model used for nowcast.
#'
#' @param newdata.expl Dataframe of new explanatory data
#' @param fitted.model List. Fitted model.
#'
#' @returns  Logical value indicating whether the new explanatory data is valid.
#' @keywords internal
#'
check_newdata_expl <- function(newdata.expl, fitted.model) {
  if (!is.data.frame(newdata.expl)) {
    stop("newdata.expl must be a data frame.")
  }
  check1 = all(c('date','virus','geo',fitted.model$varname.expl) %in%
                 colnames(newdata.expl) )
  if(!check1) {
    stop("newdata.expl does not have the expected format.")
  }

  check.dates = any(newdata.expl$date %in% fitted.model$data$date)
  if (check.dates) {
    stop("newdata.expl contains dates that are already present in the fitted model's data.")
  }

  virus = unique(fitted.model$data$virus)
  check.virus = virus %in% unique(newdata.expl$virus)
  if(!check.virus)
    stop('Fitted virus `',virus,'` not present in new explanatory data.')

  geo = unique(fitted.model$data$geo)
  check.geo = geo %in% unique(newdata.expl$geo)
  if(!check.geo)
    stop('Fitted geography `',geo,'` not present in new explanatory data.')

  return(TRUE)
}

#' @title Ensures prm has the correct arguments for lm
#' 
#' @keywords internal
check_prm_lm <- function(prm) {
  required_names <- c(
    "family", "data.dad", "data.expl", "varname.dad",
    "varname.expl", "date.range.fit")
  missing_names = setdiff(required_names, names(prm))
  if (length(missing_names) > 0) {
    stop(
      paste(
        "prm is missing required parameters:",
        paste(missing_names, collapse = ", ")))
  }

  return(TRUE)
}
