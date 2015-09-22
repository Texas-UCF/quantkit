#' Earnings Performance.
#'
#' @param ticker Yahoo Finance target ticker
#' @param quarterNumber Integer from 1-4 representing Q1, Q2, etc.
#' @param year Year in which to search
#' @return data frame with earnings results
#' @keywords earnings performance
#' @importFrom quantmod getSymbols dailyReturn
#' @export
#' @examples
#' EarningsPerformance("TSLA", 2, 2015)

EarningsPerformance <- function(ticker, quarterNumber, year) {
  # We want to search in the quarter ahead of the quarter reported by the earnings announcment
  # for the date.
#   if(quarterNumber == 3) {
#      searchStart <- as.Date(paste(year - 1, "-10-01", sep = ""))
#      searchEnd <- as.Date(paste(year - 1, "-12-31", sep = ""))
#   }
#   else if(quarterNumber == 4) {
#      searchStart <- as.Date(paste(year + 1, "-01-01", sep = ""))
#      searchEnd <- as.Date(paste(year + 1, "-03-31", sep = ""))
#   } 
#   else if(quarterNumber == 1) {
#     searchStart <- as.Date(paste(year, "-04-01", sep = ""))
#     searchEnd <- as.Date(paste(year, "-06-30", sep = ""))
#   }
#   else if(quarterNumber == 2) {
#     searchStart <- as.Date(paste(year, "-07-01", sep = ""))
#     searchStart <- as.Date(paste(year, "-09-30", sep = ""))
#   }
# 
#   # Search the earnings calendar using qmao during that period 
#   ec <- getEarningsCalendar(from = searchStart, to = searchEnd)
#   indexFound <- match(ticker, ec[,"Symbol"], nomatch = -1)
#   if(indexFound == -1) {
#     stop("Earnings report not found for that quarter")
#   }
#   earningsAnnouncement <- as.Date(ec[indexFound, "Date"])
# 
#   # Look at a 1 week window before and after the earnings announcement
#   start <- earningsAnnouncement - 7
#   end <- earningsAnnouncement + 7
# 
#   # Use quantmod to get stock prices for a certain period
#   tickerData <- new.env()
#   getSymbols(ticker, from = start, to = end, env = tickerData, warnings = F)
#   returns <- dailyReturn(get(ticker, tickerData), type='log')

  # Display a time series plot of the stock's performance
  # plot(returns, y = NULL, xlab = "Time", ylab = "Log daily returns", main = paste(ticker, earningsAnnouncement, "earnings report performance"))
}
