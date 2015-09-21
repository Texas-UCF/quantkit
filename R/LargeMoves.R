#' Large Moves.
#'
#' @param ticker Yahoo Finance target ticker
#' @param k number of standard deviations to be considered a large move
#' @param start Start date
#' @param end End date
#' @return Data frame of days that had large moves and their arithmetic returns
#' @keywords movement, events
#' @importFrom quantmod getSymbols dailyReturn
#' @export
#' @examples
#' LargeMoves("XOM", 2, '2015-01-01', '2015-05-25')

LargeMoves <- function(ticker, k, start = Sys.Date() - 365, end = Sys.Date()) {
  # Use quantmod to obtain price and returns 
  prices <- getSymbols(ticker, from = start, to = end, auto.assign = FALSE)
  returns <- data.frame(dailyReturn(prices))
  
  # Fix returns data frame
  returns$date <- row.names(returns)
  colnames(returns) <- c('return', 'date')
  returns <- returns[, c('date', 'return')]
  row.names(returns) <- NULL
  
  # Get daysReturn price series of days that have |movement| >= k * SD
  cutoff <- k * sd(returns$return)
  large.moves <- returns[which(abs(returns$return) >= cutoff), ]
  return(large.moves)
}