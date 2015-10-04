#' Earnings Performance.
#'
#' @param ticker Yahoo Finance target ticker
#' @param quarterNumber Integer from 1-4 representing Q1, Q2, etc.
#' @param year Year in which to search
#' @return data frame with earnings results
#' @keywords earnings performance
#' @importFrom quantmod getSymbols dailyReturn
#' @importFrom qmao getEarningsCalendar
#' @export
#' @examples
#' EarningsPerformance("TSLA", 2, 2015)

EarningsPerformance <- function(ticker, quarterNumber, year) {
# # We want to search in the quarter ahead of the quarter reported by the earnings announcment
# # for the date.
#    searchStart<-as.Date(Sys.Date())
#    searchEnd<-as.Date(Sys.Date())
#    if(quarterNumber == 3) {
#       searchStart <- as.Date(paste(year - 1, "-10-01", sep = ""))
#       searchEnd <- as.Date(paste(year - 1, "-12-31", sep = ""))
#    }
#    else if(quarterNumber == 4) {
#       searchStart <- as.Date(paste(year + 1, "-01-01", sep = ""))
#       searchEnd <- as.Date(paste(year + 1, "-03-31", sep = ""))
#    }
#    else if(quarterNumber == 1) {
#      searchStart <- as.Date(paste(year, "-04-01", sep = ""))
#      searchEnd <- as.Date(paste(year, "-06-30", sep = ""))
#    }
#    else if(quarterNumber == 2) {
#      searchStart <- as.Date(paste(year, "-07-01", sep = ""))
#      searchStart <- as.Date(paste(year, "-09-30", sep = ""))
#    }
#
#    # Search the earnings calendar using qmao during that period
#    ec <- getEarningsCalendar(from = searchStart, to = searchEnd)
#    indexFound <- match(ticker, ec[,"Symbol"], nomatch = -1)
#    if(indexFound == -1) {
#      stop("Earnings report not found for that quarter")
#    }
#    earningsAnnouncement <- as.Date(ec[indexFound, "Date"])
#
#    returns <- EventMovement(ticker, earningsAnnouncement)
#
#  # Display a time series plot of the stock's performance
#  plot(returns, y = NULL, xlab = "Time", ylab = "Log daily returns", main = paste(ticker, earningsAnnouncement, "earnings report performance"))
}
