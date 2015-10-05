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

EarningsPerformance <- function(ticker, searchStart = Sys.Date() - 365, searchEnd = Sys.Date(), timePeriod = 7) {
# We want to search in the quarter ahead of the quarter reported by the earnings announcment
# for the date.

   # Search the earnings calendar using qmao during that period
   ec <- getEarningsCalendar(from = searchStart, to = searchEnd)
   indexFound <- match(ticker, ec[,"Symbol"], nomatch = -1)
   if(indexFound == -1) {
     stop("Earnings report not found for that quarter")
   }
   earningsAnnouncement <- as.Date(ec[indexFound, "Date"])

   move <- EventMovement(ticker, earningsAnnouncement, timePeriod)
   ret <- list()
   ret$earningsDate <- earningsAnnouncement
   ret$data <- data.frame(date = index(move[[1]]), coredata(move), row.names = NULL )
   colnames(ret$data) <- c("date", "return")
   return(ret)

}
