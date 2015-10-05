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
  return(lapply(eventDates, function(x) 
    dailyReturn(getSymbols(ticker, from = x - timePeriod, to = x + timePeriod, 
                           auto.assign = FALSE), type="log")))
}
