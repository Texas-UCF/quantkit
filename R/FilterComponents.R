#' Filter Components.
#'
#' @param ticker Yahoo Finance target ticker
#' @param components Vector of Yahoo Finance tickers to be filtered out
#' @param cutoff If p-value > cutoff, exclude the component
#' @param start Start date
#' @param end End date
#' @return List of filtered out time series and regression results
#' @keywords regression, components
#' @importFrom quantmod getSymbols, dailyReturn
#' @export
#' @examples
#' FilterComponents("XOM", c("^GSPC", "USO"), 0.05, '2015-01-01', '2015-05-25')

FilterComponents <- function(ticker, components = "^GSPC", cutoff = 0.05, start = Sys.Date() - 365, end = Sys.Date()) {
  
  # Use quantmod to get all the stock prices
  # Use quantmod to calculate returns (use arithmetic daily returns)
  # Run Regression
  # Remove variables where p value > cutoff
  # Re-Run Regression if variables were removed
  # Use regression results to filter out time series
  
}