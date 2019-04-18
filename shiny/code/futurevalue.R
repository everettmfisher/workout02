#' @title future value
#' @description calculates the future value of an investment with interest
#' @param amount initial investment
#' @param rate interest rate
#' @param years time (in years) elapsed since initial amount
#' @return computed future value

future_value <- function(amount, rate, years) {
  if(is.numeric(amount)==FALSE | is.numeric(rate)==FALSE | is.numeric(years)==FALSE) {
    stop("values must be numeric")
  }
  fv = amount*(1 + rate)^years
  return(fv)
}