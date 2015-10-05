#' Earnings Performance.
#'
#' @param ticker Yahoo Finance target ticker
#' @param searchStart Beginning of date range to search in
#' @param searchEnd End of date range to search in
#' @param timePeriod Period of time before and after earnings report
#' @return data frame with earnings results
#' @keywords earnings performance
#' @importFrom quantmod getSymbols dailyReturn
#' @importFrom qmao getEarningsCalendar
#' @export
#' @examples
#' EarningsPerformance("TSLA", 2, 2015)

EarningsPerformance <- function(ticker, searchStart, searchEnd, timePeriod = 7) {
# We want to search in the quarter ahead of the quarter reported by the earnings announcment
# for the date.

   # Search the earnings calendar using qmao during that period
   ec <- getEarningsCalendar(from = searchStart, to = searchEnd)
   indexFound <- match(ticker, ec[,"Symbol"], nomatch = -1)
   if(indexFound == -1) {
     stop("Earnings report not found for that quarter")
   }
   earningsAnnouncement <- as.Date(ec[indexFound, "Date"])

   returns <- EventMovement(ticker, earningsAnnouncement, timePeriod)

 # Display a time series plot of the stock's performance
 plot(returns[[1]], y = NULL, xlab = "Time", ylab = "Log daily returns", main = paste(ticker, earningsAnnouncement, "earnings report performance"))
}
