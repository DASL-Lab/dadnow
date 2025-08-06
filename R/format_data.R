

#' Process variable name for DAD data
#'
#' @param data.dad Dataframe of DAD data
#' @param varname.dad String. Name of the variable in data.dad to fit the model to.
#'
#' @returns Dataframe
#'
process_varname_dad <- function(data.dad, varname.dad) {
  if(varname.dad == 'count'){
    res = data.dad |>
      dplyr::rename(count.dad = count) |>
      dplyr::select(-percapita)
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
    dplyr::filter(between(date, date.range.fit[1], date.range.fit[2]))

  if(nrow(res) == 0) {
    stop("No data in the specified date range.")
  }
  if(nrow(res) < 3) {
    stop("Not enough data in the specified date range to fit a model.")
  }

  return(res)
}




