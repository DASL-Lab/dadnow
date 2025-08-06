#' Plot the fitted model: regression line, data points used for the fit and fit metrics.
#'
#' @param fitted.model List. Fitted model.
#'
#' @returns A ggplot object.
#' @export
#'
#' @examples
#'
#'
plot_fitted_model_lm <- function(fitted.model) {

  m = fitted.model$model
  family = fitted.model$family
  sm = summary(m)
  k = sm$coefficients

  int = k[1,1]
  slope= k[2,1]
  rsq = sm$adj.r.squared

  data = fitted.model$data |>
    dplyr::rename(DAD = dplyr::ends_with('dad'),
                  Explanatory = dplyr::ends_with('expl'))

  is.log = grepl('log', family)
  if(is.log)
    data = mutate(data, DAD = log(DAD),
                  Explanatory = log(Explanatory))

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

  g = ggplot(data, aes(x = Explanatory, y = DAD)) +
    theme_bw()+
    geom_point()+
    geom_abline(intercept = 0, slope = 1, color = 'grey80', linetype = 'dashed')+
    geom_abline(intercept = int, slope = slope,
                color = 'steelblue', linewidth = 2, alpha = 0.7)+
    theme(panel.grid = element_line('grey97'))+
    labs(title = paste('Fitted Model :', tt),
         subtitle = subt, x=xlab, y = ylab)
  g
}
