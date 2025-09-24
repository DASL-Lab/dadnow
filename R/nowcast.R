
#' Nowcast DAD data using fresh explanatory data.
#'
#' @param fitted.model List. Fitted model as created by the function \code{fit_model()}.
#' @param newdata.expl Dataframe of new explanatory data. Must contain colums named
#' \code{date, virus, geo} and a column name matching \code{fitted.model$varname.expl}.
#' @param ci Numerical. Confidence interval of the prediction. Must be between 0 and 1.
#' @param floor.zero Logical. If `TRUE` nowcast will be floored at 0 when estimates are negatives.
#'
#' @returns A list:
#' \itemize{
#'     \item \code{nowcast}: Dataframe of nowcasted DAD values.
#'     \item \code{fitted.model}: Fitted model used to nowcast.
#'     \item \code{newdata.expl}: New explanatory data used to nowcast.
#' }
#'
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
#'g.fit = plot_fitted_model(fitted.model)
#'g.fit$xy
#'g.fit$timeseries
#'
#'
#'n = 15
#'newdata.expl = data.frame(
#'  date = data.expl$date[1:n] + 360,
#'  virus = 'VXX',
#'  geo = 'GZZ',
#'  thecount = rpois(n, lambda = 20*c(1:n))
#')
#'
#'a = nowcast(fitted.model = fitted.model,
#'            newdata.expl = newdata.expl)
#'g.nowc = plot_nowcast(nowc = a)
#'g.nowc

nowcast <- function(fitted.model, newdata.expl,
                    ci = 0.95,
                    floor.zero = TRUE) {

  check_fitted_model(fitted.model)
  check_newdata_expl(newdata.expl, fitted.model)

  m = fitted.model$model
  dates = newdata.expl$date

  newdata = process_newdata_expl(newdata.expl, fitted.model)

  # Prediction (nowcasting) using fitted model
  pr = predict.lm(object = m,
                  newdata = newdata,
                  interval = 'prediction')

  if(grepl('log', fitted.model$family)) pr = exp(pr)

  if(floor.zero) pr = pmax(pr,0)

  # Merge prediction to other data (dates, virus, ...)
  basedf = data.frame(
    virus = get_virus(fitted.model),
    geo   = get_geo(fitted.model),
    date  = dates
  )

  prdf = as.data.frame(pr) |>
    dplyr::rename(
      nowcast = fit, nowcast.lo = lwr, nowcast.hi = upr
    )
  df = cbind(basedf, prdf)

  res = list(
    nowcast = df,
    fitted.model = fitted.model,
    newdata.expl = newdata.expl
  )

  return(res)
}
