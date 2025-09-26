#' @title Prepare data for simple linear model
#' 
#' @description Internal. Ensures the variable names are as expected, joins the dad with the expl data, filters into the relevant date range, then removes NAs.

prep_lm <- function(prm) {
  # Process inputs
  check_dad_data(prm$data.dad)
  check_expl_data(prm$data.expl)
  check_varname(prm$data.dad, prm$varname.dad)
  check_varname(prm$data.expl, prm$varname.expl)

  datesrng = as.Date(prm$date.range.fit)
  check_date_range_fit(datesrng)

  # prepare the data sets before model ingestion
  df = data.dad |>
    join_dad_expl(prm$data.expl,
                  varname.dad = prm$varname.dad,
                  varname.expl = prm$varname.expl) |>
    filter_date_range_fit(datesrng) |>
    remove_NAs(varname.dad = prm$varname.dad,
               varname.expl = prm$varname.expl)

  df
}

#' @title Fit a simple linear regression model
#' 
#' @description Expects carefully constructed variable names. Fits a model of the form *.dad ~ *.expl
fit_simple_lm <- function(prm) {
  df <- prep_lm(prm)

  lm_formula <- as.formula(
    paste0(prm$varname.dad, '.dad ~ ', prm$varname.expl, '.expl')
  )

  m = lm(
    lm_formula,
    data = df
  )

  list(
    model          = m,
    data           = df,
    varname.dad    = varname.dad,
    varname.expl   = varname.expl,
    family         = family,
    date.range.fit = datesrng
  )
}


fit_simple_log_log <- function(prm) {
  df <- prep_lm(prm) |> 
    replace_zeros_by_tiny(df,
      varname.dad = varname.dad,
      varname.expl = varname.expl
    )

  ll_lm_formula <- as.formula(
    paste0('log(', varname.dad, '.dad) ~ ', 'log(', varname.expl, '.expl)')
  )

  m = lm(
    ll_lm_formula,
    data = df
  )

  list(
    model          = m,
    data           = df,
    varname.dad    = varname.dad,
    varname.expl   = varname.expl,
    family         = family,
    date.range.fit = datesrng
  )
}


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
#'set.seed(1234)
#'
#'data.dad  = fake_dad()
#'data.expl = fake_expl(dad = data.dad)
#'
#'prm = list(
#'  family = 'lm',
#'  data.dad = data.dad,
#'  data.expl = data.expl,
#'  varname.dad = 'count',
#'  varname.expl = 'thecount',
#'  date.range.fit = c('2020-01-10', '2020-12-15')
#')
#'
#'fitted.model = fit_model(prm)
#'
fit_model <- function(prm) {

  res <- switch(
    EXPR = prm$family,
    "lm" = fit_simple_lm(prm),
    "lm-log" = fit_simple_log_log(prm),
    stop("Unsupported family type. Currently supported families are 'lm' and 'lm-log'.")
  )
  return(res)
}
