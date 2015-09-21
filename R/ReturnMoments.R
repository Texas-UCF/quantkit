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
  tickerData <- new.env()
  getSymbols(ticker, from = start, to = end, env = tickerData, warnings = F)

  # Use quantmod to get a vector of logarithmic daily returns
  tickerReturns <- dailyReturn(get(ticker, tickerData), type='log')
  tickerReturns <- as.vector(tickerReturns)

  # Calculate mean, sd, and use moments to calculate skewness, kurtosis
  tickerMean <- mean(tickerReturns)
  tickerSd <- sd(tickerReturns)
  tickerSkew <- skewness(tickerReturns)
  tickerKurt <- kurtosis(tickerReturns)

  # Construct a list of the values to return
  my_list <- list("mean" = tickerMean, "sd" = tickerSd, "skewness" = tickerSkew, "kurtosis" = tickerKurt)

  # Display a histogram of the ticker's returns
  hist(as.vector(tickerReturns), breaks = "FD", col = "red", xlab = "Logarithmic daily returns", 
        ylab = "Occurrences", main = paste(ticker, "return distribution with normal curve"))
  
  # Overlay a normal distribution on top of the histogram
  xfit <- seq(min(tickerReturns), max(tickerReturns), length = 40)
  yfit <- dnorm(xfit, mean = tickerMean, sd=tickerSd)
  lines(xfit, yfit, col="blue", lwd = 2)

  return(my_list)
}
