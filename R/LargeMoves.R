#' Large Moves.
#'
#' @param ticker Yahoo Finance target ticker
#' @param k number of standard deviations to be considered a large move
#' @param start Start date
#' @param end End date
#' @return price series of days that had large moves
#' @keywords large price movement
#' @importFrom quantmod getSymbols 
#' @export
#' @examples
#' LargeMoves("XOM", 2, '2015-01-01', '2015-05-25')

LargeMoves <- function(ticker, k, start=Sys.Date() - 365, end=Sys.Date()){
    tickerData <- new.env()
    getSymbols(ticker,from=start,to=end,env=tickerData)
    
    tickerDaily <- get(ticker, tickerData)
    
    dailyMoves <- tickerDaily[,2] - tickerDaily[,3]
    dailySD <- sd(dailyMoves)
    largeMoves <- dailyMoves[dailyMoves > k*dailySD,]
    
    return(tickerDaily[which(index(tickerDaily) %in% index(largeMoves)),])
}