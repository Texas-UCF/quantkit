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
  
  # Use quantmod to obtain price and returns 
  prices <- getSymbols(ticker, from = start, to = end, auto.assign = FALSE)
  returns <- dailyReturn(prices, type = 'log')

  # Construct a list of the values to return
  ticker.moments <- list("mean" = mean(returns), "sd" = sd(returns), "skewness" = skewness(returns), "kurtosis" = kurtosis(returns))

  # Display a histogram of the ticker's returns
  hist(as.vector(returns), breaks = "FD", col = "red", xlab = "Logarithmic daily returns", 
        ylab = "Occurrences", main = paste(ticker, " return distribution with normal curve"))
  
  # Overlay a normal distribution on top of the histogram
  xfit <- seq(min(returns), max(returns), length = 40)
  yfit <- dnorm(xfit, mean = mean(returns), sd = sd(returns))
  lines(xfit, yfit, col="blue", lwd = 2)

  return(ticker.moments)
}
