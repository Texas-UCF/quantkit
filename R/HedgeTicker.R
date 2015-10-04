#' Hedge Portfolio
#'
#' @param ticker Yahoo Finance target ticker
#' @param numShares number of shares in ticker 
#' @param components Components that you want to have 0 exposure to when you hedge
#' @param cutoff If p-value > cutoff, exclude the portfolio component
#' @param start Start date
#' @param end End date
#' @return number of shares of each component necessary to hedge out exposure
#' @keywords regression, components
#' @importFrom quantmod getSymbols
#' @export
#' @examples
#' HedgeTicker('SDRL', 100, c('SPY', 'USO'))

HedgeTicker <- function(ticker, numshares=100, components='^GSPC', cutoff=0.05, start=Sys.Date()-365, end=Sys.Date()){
  filter <- FilterComponents(ticker, components=components, 
                               cutoff = cutoff, start=start, end=end)
  filter <- filter$regression$coefficients[,1] * -1 * numshares
  return(filter[2:length(filter)])
}