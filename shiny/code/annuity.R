#' @title future value of annuity
#' @description calculates the future value of annuity
#' @param contrib annual contribution
#' @param rate annual rate of return
#' @param years time (in years) elapsed since initial amount
#' @return computed future value

annuity <- function(contrib, rate, years) {
  if(is.numeric(contrib)==FALSE | is.numeric(rate)==FALSE | is.numeric(years)==FALSE) {
    stop("values must be numeric")
  }
  ann = contrib*(((1+rate)^years - 1)/rate)
  return(ann)
}