

#' Process variable name for DAD data
#'
#' @param data.dad Dataframe of DAD data
#' @param varname.dad String. Name of the variable in data.dad to fit the model to.
#'
#' @returns Dataframe
#' @keywords internal
#'
process_varname_dad <- function(data.dad, varname.dad) {
  if(varname.dad == 'count'){
    res = data.dad |>
      dplyr::rename(count.dad = count)

    if('percapita' %in% names(res))
      res = dplyr::select(res, -percapita)
  }
  if(varname.dad == 'percapita'){
    res = data.dad |>
      dplyr::rename(percapita.dad = percapita) |>
      dplyr::select(-count)
  }
  return(res)
}


#' Process the variable name for explanatory data
#' by appending the suffix '.expl' to the variable name
#'
#' @param data.expl Dataframe of explanatory data
#' @param varname.expl String. Name of the variable in data.expl to use as an explanatory variable.
#'
#' @returns Dataframe
#' @keywords internal
#'
process_varname_expl <- function(data.expl, varname.expl) {
  res = data.expl |>
    dplyr::rename(!!paste0(varname.expl, '.expl') := !!varname.expl)
}


#' Join DAD and explanatory data
#'
#' @param data.dad Dataframe of DAD data
#' @param data.expl Dataframe of explanatory data
#' @param varname.dad String. Name of the variable in data.dad to fit the model to.
#' @param varname.expl String. Name of the variable in data.expl to use as an explanatory variable.
#'
#' @returns Dataframe
#' @keywords internal
#'
join_dad_expl <- function(data.dad,
                          data.expl,
                          varname.dad,
                          varname.expl) {

  tmp.expl = process_varname_expl(data.expl, varname.expl)

  res = data.dad |>
    process_varname_dad(varname.dad) |>
    dplyr::left_join(
      tmp.expl,
      by = c("date", "virus", "geo"))
  return(res)
}

filter_date_range_fit <- function(d,
                                  date.range.fit) {
  if(is.null(date.range.fit)) {
    return(d)
  }
  res = d |>
    dplyr::filter(dplyr::between(date,
                                 date.range.fit[1],
                                 date.range.fit[2]))

  if(nrow(res) == 0) {
    stop("No data in the specified date range.")
  }
  if(nrow(res) < 3) {
    stop("Not enough data in the specified date range to fit a model.")
  }

  return(res)
}



#' Remove all NA from the joint data set
#'
#' @param df Dataframe of joint data
#' @param varname.dad String. Variable used from the DAD data
#' @param varname.expl String. Variable used from the explanatory data
#'
#' @returns Dataframe stripped of NAs
#' @keywords internal
#'
remove_NAs <- function(df, varname.dad, varname.expl) {
  idx.expl = which(!is.na(df[[app_varname_expl(varname.expl)]]))
  idx.dad  = which(!is.na(df[[app_varname_dad(varname.dad)]]))
  idx = intersect(idx.dad, idx.expl)
  res = df[idx,]
  return(res)
}

#' Process new explanatory data before nowcasting.
#'
#' @param newdata.expl Dataframe of new explanatory data
#' @param fitted.model List. Fitted model.
#'
#' @returns Processed dataframe
#' @keywords internal
#'
process_newdata_expl <- function(newdata.expl,
                                 fitted.model) {

  virus.model = unique(fitted.model$data$virus)
  geo.model   = unique(fitted.model$data$geo)
  v.expl      = fitted.model$varname.expl

  res = newdata.expl |>
    # Filter any irrelevant information
    dplyr::filter(virus == virus.model,
                  geo == geo.model)|>
    # append suffix because this is how
    # it is identified in the fitted model
    dplyr::rename(!!paste0(v.expl, '.expl') := !!v.expl)

  if(nrow(res) < nrow(newdata.expl))
    warning('New explanatory data contains irrelevant imformation.')

  return(res)
}

#' Add a tiny value to the elements of a vector that are equal to zero.
#'
#' @param x Vector of values.
#'
#' @returns A vector with zeros replaced by a tiny value.
#' @keywords internal
#'
add_tiny <- function(x) {

  idx.zeros = which(x==0)

  if(length(idx.zeros)==0) {return(x)}

  m = median(x[-idx.zeros])

  # Case when x is _not_ a rate
  if(m > 1) tiny = 0.1
  # Case when x is likely a percentage
  if(m > 0.005 & m <=1) tiny = 1e-4
  # Case when x is likely a per capita rate
  if(m <= 0.005 ) tiny = 1e-7

  # Replace zeros with tiny value
  res = x
  res[idx.zeros] = tiny

  return(res)
}

#' Replace zeros by a tiny value (avoids log(0) downstream).
#'
#' @param df Dataframe of values.
#' @param varname.dad String. Name of the DAD variable whose zeros must be replaced by tiny value.
#' @param varname.expl String. Name of the explanatory variable whose zeros must be replaced by tiny value.
#'
#' @keywords internal
#'
replace_zeros_by_tiny <- function(df,varname.dad, varname.expl) {

  # identify the rows where there is 0 as data value
  # idx0.dad  = which(df[[app_varname_dad(varname.dad)]] == 0)
  # idx0.expl = which(df[[app_varname_expl(varname.expl)]] == 0)

  vd = df[[app_varname_dad(varname.dad)]]
  ve = df[[app_varname_expl(varname.expl)]]

  res = df
  res[[app_varname_dad(varname.dad)]] = add_tiny(vd)
  res[[app_varname_expl(varname.expl)]] = add_tiny(ve)

  return(res)
}
