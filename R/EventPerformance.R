#' Event Performance
#'
#' @param ticker Yahoo Finance target ticker
#' @param event Event date
#' @return 
#' @keywords event performance
#' @importFrom quantmod getSymbols dailyReturn
#' @export
#' @examples
#' EventPerformance("TSLA", '2015-01-01')

EventPerformance <- function(ticker, start = Sys.Date() - 365, end = Sys.Date()) {
  
}
