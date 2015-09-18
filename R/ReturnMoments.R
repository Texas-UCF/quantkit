#' Return Moments.
#'
#' @param ticker Yahoo Finance target ticker
#' @param start Start date
#' @param end End date
#' @return vector of mean, SD, skewness, kurtosis
#' @keywords moments
#' @importFrom quantmod getSymbols dailyReturn
#' @importFrom moments skewness kurtosis
#' @export
#' @examples
#' ReturnMoments("TSLA", '2015-01-01', '2015-05-25')

ReturnMoments <- function(ticker, start = Sys.Date() - 365, end = Sys.Date()) {
  
  # Use quantmod to get stock prices and then logarithmic daily returns
  # Calculate mean, sd, and use moments to calculate skewness, kurtosis
  
}