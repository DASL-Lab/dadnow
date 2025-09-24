#' Plot the fitted model: regression line, data points used for the fit and fit metrics.
#'
#' @param fitted.model List. Fitted model as created by the function \code{fit_model()}.
#' @param ci Numerical. Confidence level.
#'
#' @returns A list of ggplot objects.
#'
#' @keywords internal
#'
#'
plot_fitted_model_lm <- function(fitted.model, ci) {

  family = fitted.model$family
  is.log = grepl('log', family)

  # Unpack regression results
  m  = fitted.model$model
  sm = summary(m)
  k  = sm$coefficients
  int   = k[1,1]
  slope = k[2,1]
  rsq   = sm$adj.r.squared

  # append identifying suffixes
  data = fitted.model$data |>
    dplyr::rename(DAD = dplyr::ends_with('dad'),
                  Explanatory = dplyr::ends_with('expl'))
  if(is.log)
    data = dplyr::mutate(data,
                         DAD = log(DAD),
                         Explanatory = log(Explanatory))

  # Reconstruct DAD time series inferred from fitted model
  v.expl = paste0(fitted.model$varname.expl,'.expl')
  v = fitted.model$data[[v.expl]]
  newdata = data.frame(v)
  names(newdata) = v.expl

  dad.estim.fit = predict.lm(m, newdata = newdata,
                             interval = 'confidence',
                             ci = ci)

  dts = cbind(data, dad.estim.fit) |>
    dplyr::rename(DAD.fit = fit, DAD.lo = lwr, DAD.hi = upr)


  dtslong = dts |>
    tidyr::pivot_longer(cols = -c(date, virus, geo)) |>
    dplyr::mutate(variable = dplyr::case_when(
      name == 'DAD' ~ 'Actual DAD',
      name == 'Explanatory' ~ 'Explanatory',
      name == 'DAD.fit' ~ 'Fitted DAD',
      TRUE ~ NA
    ))

  # Build plot

  subt = paste0(id_model(fitted.model),
    '\nslope = ', round(slope,3),
    ' ; intercept = ', round(int,3),
    ' ; R2adj = ', round(rsq,3)
  )

  xlab = ifelse(is.log, 'log(Explanatory)', 'Explanatory')
  ylab = ifelse(is.log, 'log(DAD)', 'DAD')

  g.xy = data |>
    ggplot2::ggplot(ggplot2::aes(x = Explanatory, y = DAD)) +
    ggplot2::theme_bw()+
    ggplot2::geom_point()+
    # Bissect for reference
    ggplot2::geom_abline(intercept = 0, slope = 1,
                         color = 'grey80', linetype = 'dashed')+
    # Fitted regression line
    ggplot2::geom_abline(intercept = int, slope = slope,
                         color = 'steelblue', linewidth = 2, alpha = 0.7)+
    # Cosmetics
    ggplot2::theme(panel.grid = ggplot2::element_line('grey97'),
                   plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.6)))+
    ggplot2::labs(title = 'Fitted Model',
                  subtitle = subt,
                  x=xlab, y = ylab)
  # g.xy

  col.fit    = 'indianred2'
  col.actual = 'black'
  col.expl   = 'steelblue2'

  g.ts = dtslong |>
    filter(!is.na(variable)) |>
    ggplot2::ggplot(ggplot2::aes(x=date)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.6)),
      panel.grid.minor = element_line(color = 'grey97')) +
    ggplot2::geom_ribbon(data = dts,
                         ggplot2::aes(ymin = DAD.lo, ymax = DAD.hi),
                         alpha = 0.2, fill = col.fit)+
    ggplot2::geom_line(ggplot2::aes(x=date, color = variable, y = value))+
    ggplot2::geom_point(data = dplyr::filter(dtslong,
                                             grepl('(Actual|Expl)', variable)),
                        ggplot2::aes(x=date, color = variable, y = value))+
    ggplot2::scale_color_manual(values = c(col.actual, col.expl, col.fit))+
    ggplot2::labs(title = 'Fitted Model Time Series',
                  subtitle = id_model(fitted.model),
                  y = ifelse(is.log,'log(value)', 'value'))
  # g.ts

  res = list(
    xy = g.xy,
    timeseries = g.ts
  )
  return(res)
}



#' @title Plot fitted model and data used.
#'
#' @param fitted.model List. Fitted model as created by the function \code{fit_model()}.
#' @param ci Numerical. Confidence interval of the fit displayed.
#'
#' @returns A list of ggplot objects:
#' \itemize{
#'   \item \code{xy}: scattered plot of data and regression line.
#'   \item \code{timeseries}: fitted DAD time series in comparison with the actual data.
#' }
#'
#' @export
#'
#' @examples
plot_fitted_model <- function(fitted.model, ci) {

  family = fitted.model$family

  if(grepl('lm', family)) {
    g = plot_fitted_model_lm(fitted.model, ci)
  }
  return(g)
}

#' @title Plot the time series of DAD and explanatory data used to fit the model.
#'
#' @param fitted.model List. Fitted model as created by the function \code{fit_model()}
#'
#' @returns  A ggplot object.
#' @export
#'
#' @examples
plot_timeseries_data <- function(fitted.model) {

  dplot = fitted.model$data |>
    tidyr::pivot_longer(cols = -c(date, virus, geo),
                        names_to = 'variable') |>
    # Keep only the variable used in the model
    dplyr::filter(grepl('(dad$|expl$)', variable))

  g = dplot |>
    ggplot2::ggplot(ggplot2::aes(x=date, y = value, color = variable)) +
    ggplot2::theme_bw()+
    ggplot2::geom_step(linewidth = 1) +
     ggplot2::theme(panel.grid = ggplot2::element_line('grey97'),
                   plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.6)))+
    ggplot2::labs(title = 'Time series',
                  subtitle = id_model(fitted.model))
  g
}



#' @title Plot the DAD nowcasting time series.
#'
#' @description
#' Plot the nowcast DAD estimates along the new ("fresh")
#' explanatory data that fed the nowcast and the past
#' DAD and explanatory data that were used to fit the model.
#'
#'
#' @param nowc List representing a nowcast object as returned
#' by the function \code{nowcast()}.
#'
#' @returns A ggplot object.
#' @export
#'
#' @examples
plot_nowcast <- function(nowc) {

  df.nowc = nowc$nowcast |>
    dplyr::mutate(variable = 'nowcast', value = 0)

  df.data = nowc$fitted.model$data

  df.newdata = nowc$newdata.expl|>
    dplyr::mutate(variable ='new expl. data')

  g.ts = plot_timeseries_data(fitted.model)

  # --- cosmetics

  col.dad     = 'black'
  col.expl    = 'steelblue1'
  col.explnew = 'steelblue3'
  col.nowcast = 'indianred'

  v.dad  = paste0(nowc$fitted.model$varname.dad,'.dad')
  v.expl = paste0(nowc$fitted.model$varname.expl,'.expl')

  cols  =  c(col.dad,
             col.expl,
             `new expl. data` = col.explnew,
             nowcast = col.nowcast)
  names(cols)[1] = v.dad
  names(cols)[2] = v.expl

  # Build final plot

  g.nc = g.ts +
    ggplot2::geom_point(data = df.newdata,
                        ggplot2::aes(y = .data[[fitted.model$varname.expl]])) +
    ggplot2::geom_ribbon(data = df.nowc,
                         ggplot2::aes(x=date, ymin=nowcast.lo, ymax=nowcast.hi),
                         alpha = 0.2, fill = col.nowcast, linewidth = 0.1)+
    ggplot2::geom_line(data = df.nowc,
                       ggplot2::aes(x=date, y=nowcast),linewidth = 0.5)+
     ggplot2::theme(
       panel.grid = ggplot2::element_line('grey97'),
       plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.6)))+
    ggplot2::scale_color_manual(values = cols)+
    ggplot2::labs(title = paste('Time series nowcast'))
  g.nc
}


