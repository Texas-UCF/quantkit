#' Similar Stocks.
#'
#' @param ticker Stock ticker
#' @param market.cap Percentage difference in market cap for similar stocks (or FALSE if not used)
#' @param sector Boolean to represent if stocks should be restricted to same sector
#' @param industry Boolean to represent if stocks should be restricted to same industry
#' @return Data frame of similar stocks with their market caps, sectors, and industries
#' @keywords similar, market cap, sector, industry
#' @importFrom TTR stockSymbols
#' @export
#' @examples
#' SimilarStocks("AAPL", market.cap = 0.15)

SimilarStocks <- function(ticker, market.cap = 0.1, sector = TRUE, industry = FALSE) {
  
  # Get data.frame of all stocks
  df <- stockSymbols(quiet = TRUE)
  
  # Fix market cap column
  df$Market.Cap <- as.numeric(gsub("[\\$MB]", "", df$MarketCap))
  df[grep("B", df$MarketCap), 'Market.Cap'] <- df[grep("B", df$MarketCap), 'Market.Cap'] * 1000
  df <- df[, c('Symbol', 'Name', 'Market.Cap', 'Sector', 'Industry')]
  
  # Get targets
  row <- df[which(df$Symbol == ticker), ]
  
  if (market.cap != FALSE) {
    min.market.cap <- row$Market.Cap * (1 - market.cap)
    max.market.cap <- row$Market.Cap * (1 + market.cap)
    df <- df[which(df$Market.Cap >= min.market.cap & df$Market.Cap <= max.market.cap), ]
  }
  
  if (sector) {
    df <- df[which(df$Sector == row$Sector), ]
  }
  
  if (industry) {
    df <- df[which(df$Industry == row$Industry), ]
  }
  
  return(df)
}