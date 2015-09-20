#' Earnings Chart
#'
#' @param ticker Yahoo Finance target ticker
#' @return 
#' @keywords earnings
#' @importFrom quantmod getFinancials viewFinancials
#' @export
#' @examples
#' EarningsChart("TSLA")

EarningsChart <- function(ticker) {
  # Get earnings data from quantmod (they retrieve exclusively from Google Finance)
  earningsEnv <- new.env()
  getFinancials(ticker, env = earningsEnv, src = 'google')
  earningsData <- get(paste(ticker, '.f', sep = ''), earningsEnv)
  qFinancials <- viewFinancials(earningsData, type = 'IS', period = 'Q')

  # Create a vector composed of all the available earnings data
  numQuarters = dim(qFinancials)[2]
  earnings <- numeric(numQuarters)
  date <- numeric(numQuarters)
  for(i in 1:numQuarters) {
    earnings[i] <- qFinancials[25, i]
    date[i] <- -i
  }

  date

  # Plot an earnings chart for all the quarterly data from Yahoo finance
  plot(date, earnings, col = 'blue', xlab = '# Quarters from now', ylab = 'Earnings (in millions)', main = 'Earnings chart')
  lines(date, earnings, col = 'blue', lwd = 2)
  
}
