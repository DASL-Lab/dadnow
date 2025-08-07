#' Plot the fitted model: regression line, data points used for the fit and fit metrics.
#'
#' @param fitted.model List. Fitted model as created by the function \code{fit_model()}.
#'
#' @returns A ggplot object.
#'
#' @keywords internal
#'
#'
plot_fitted_model_lm <- function(fitted.model) {

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

  # Build plot

  tt = paste(
    fitted.model$family,
    fitted.model$varname.dad,
    fitted.model$varname.expl,
    get_virus(fitted.model),
    get_geo(fitted.model),
    sep = ' / '
  )
  subt = paste0(
    'slope = ', round(slope,3),
    ' ; intercept = ', round(int,3),
    ' ; R2adj = ', round(rsq,3)
  )

  xlab = ifelse(is.log, 'log(Explanatory)', 'Explanatory')
  ylab = ifelse(is.log, 'log(DAD)', 'DAD')

  g = data |>
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
    ggplot2::labs(title = paste('Fitted Model :', tt),
         subtitle = subt, x=xlab, y = ylab)
  g
}



#' @title Plot fitted model and data used.
#'
#' @param fitted.model List. Fitted model as created by the function \code{fit_model()}.
#'
#' @returns A ggplot object.
#' @export
#'
#' @examples
plot_fitted_model <- function(fitted.model) {

  family = fitted.model$family

  if(grepl('lm')) {
    g = plot_fitted_model_lm(fitted.model)
  }
  return(g)
}


