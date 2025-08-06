

#' Generate fake DAD data for testing purposes
#'
#' @returns A dataframe.
#' @export
#' @examples
#'
#' data.dad = fake_dad()
#'
fake_dad <- function() {
  d = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "week")
  n = length(d)
  x = 1:n
  xc = round( (x* (n-x) * 1) * rnorm(n, mean = 1, sd = 0.1) )

  res = data.frame(
    date = d,
    virus = rep("VXX", n),
    geo = rep("GZZ", n),
    count = xc,
    percapita = xc/1e6
  )
  return(res)
}

#' Create fake explanatory data from existing DAD for testing purposes
#'
#' @param dad Dataframe of DAD data.
#'
#' @returns A dataframe with explanatory data.
#' @export
#'
#' @examples
#' data.dad = fake_dad()
#' data.expl = fake_expl(data.dad)
#'
fake_expl <- function(dad) {
  n = nrow(dad)
  d = dad$date
  res = data.frame(
    date = d,
    virus = rep("VXX", n),
    geo = rep("GZZ", n),
    thecount = round( dad$count * 0.5 * rnorm(n, mean = 1, sd = 0.1))
  )
  return(res)
}
