#' Event Performance
#'
#' @param ticker Yahoo Finance target ticker
#' @param eventDates list dates of events for which you want performance information
#' @param timePeriod period of time (in days) before and after events for which you want data
#' @return vector of timeseries data showing performance in specified period before and after event dates
#' @keywords earnings performance
#' @importFrom quantmod getSymbols dailyReturn
#' @export
#' @examples
#' EventMovement("TSLA", 2, 2015)

EventMovement <- function(ticker, eventDates, timePeriod=7) {

  # Set up environment and return list
  tickerData <- new.env()
  returns<-list()

  # Return list index
  i<-1
  for(date in eventDates) {

    # Calculate start and end of time period
    start <- date - timePeriod
    end <- date + timePeriod

    # Use quantmod to retrieve daily returns in period
    getSymbols(ticker, from = start, to = end, env=tickerData)
    dR<-dailyReturn(get(ticker, tickerData), type="log")

    # Add dailyReturn timseries to vetor
    returns[[i]]<-dR

    i<-i+1
  }
  returns
}
