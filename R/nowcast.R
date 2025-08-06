
#' Nowcast DAD data using a fresh explanatory data set.
#'
#' @param fitted.model List. Fitted model.
#' @param newdata.expl Dataframe of new explanatory data.
#' @param ci Numerical. Confidence interval of the prediction. Must be between 0 and 1.
#' @param floor.zero Logical. If `TRUE` nowcast will be floored at 0 when estimates are negatives.
#'
#' @returns Dataframe of nowcasted DAD values.
#' @export
#'
#' @examples
#'
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
  res = cbind(basedf, prdf)
  return(res)
}
