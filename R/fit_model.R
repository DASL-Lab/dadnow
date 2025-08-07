#' @title Fit a model to the data.
#'
#' @description The goal of this function is to fit a statistical model, typically
#' a regression, of the DAD data against another "explanatory" data set.
#' This fitted model will then be used downstream to nowcast DAD data
#' from new (i.e. more recent than the latest DAD observation) explanatory data.
#'
#' The data consists of a pair of data sets, one being DAD and the other
#' being another data source that plays the role of the
#' explanatory variable (e.g., CNISP, PTSOS, RVDSS, ...)
#'
#' Moreover, the variables used in each data set must be selected.
#' For example, the user can choose to work with counts of DAD hospital admissions
#' or per-capita rates. This choice determines the input parameter `varname.dad`.
#' Similarly, for the explanatory data set, the variable name used to fit the model
#' must be selected. Its name depends on the data set itself (the column name used)
#' and is set through the input parameter `varname.expl`.
#'
#' This function is a wrapper for several possible models. The type of the
#' model used is set with the input parameter `family`.
#'
#'
#' @param prm List of parameters. Must contain:
#'  \itemize{
#'   \item \code{family} String. Type of model to fit. "lm" for linear regression.
#' "lm-log" for linear regression with log transformation.
#'
#'   \item \code{data.dad} Dataframe of DAD data. Must contain the following columns:
#' date, virus, geo, count, percapita
#'
#'   \item  \code{data.expl} Dataframe of explanatory data. Must contain the following columns:
#' date, virus, geo, varname.expl
#'
#'   \item  \code{varname.dad} String. Name of the variable in data.dad to fit the model to.
#'
#'   \item  \code{varname.expl} String. Name of the variable in data.expl to use as an explanatory variable.
#'
#'   \item  \code{date.range.fit} Vector of two dates. The model will be fit to data within this date range.
#'  }
#' @returns A fitted model object.
#' @export
#'
#' @examples
#'
fit_model <- function(prm) {

  # Unpack prameters
  family         = prm$family
  data.dad       = prm$data.dad
  data.expl      = prm$data.expl
  varname.dad    = prm$varname.dad
  varname.expl   = prm$varname.expl
  date.range.fit = prm$date.range.fit

  # data.dad = fake_dad()
  # data.expl = fake_expl(data.dad)

  # Process inputs
  check_dad_data(data.dad)
  check_expl_data(data.expl)
  check_varname_dad(varname.dad)
  check_varname_expl(data.expl, varname.expl)

  datesrng = as.Date(date.range.fit)
  check_date_range_fit(datesrng)

  # varname.dad = 'count'
  # varname.expl = 'thecount'


  # prepare the data sets before model ingestion
  df = data.dad |>
    join_dad_expl(data.expl, varname.dad, varname.expl) |>
    filter_date_range_fit(datesrng)

  if(family == 'lm') {
    # Fit a linear regression model
    m = lm(
      as.formula(paste0(varname.dad, '.dad ~ ', varname.expl, '.expl')),
      data = df
    )
  } else if(family == 'lm-log') {
    # Fit a linear regression model with log transformation
    m = lm(
      as.formula(paste0('log(', varname.dad, '.dad) ~ ',
                        'log(', varname.expl, '.expl)')),
      data = df
    )
  } else {
    stop("Unsupported family type. Use 'lm' or 'lm-log'.")
  }

  res = list(
    model          = m,
    data           = df,
    varname.dad    = varname.dad,
    varname.expl   = varname.expl,
    family         = family,
    date.range.fit = datesrng
  )
  return(res)
}
