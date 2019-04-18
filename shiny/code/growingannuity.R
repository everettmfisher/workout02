#' @title future value of growing annuity
#' @description calculates the future value of annuity with a growing rate
#' @param amount initial investment
#' @param rate annual rate of return
#' @param growth growth rate
#' @param years time (in years) elapsed since initial amount
#' @return computed future value

growing_annuity <- function(contrib, rate, growth, years) {
  if(is.numeric(contrib)==FALSE | is.numeric(rate)==FALSE || is.numeric(growth)==FALSE |   is.numeric(years)==FALSE) {
    stop("values must be numeric")
  }
  gann = contrib*(((1+rate)^years - (1+growth)^years)/(rate - growth))
  return(gann)
}