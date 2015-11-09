options("getSymbols.warning4.0"=FALSE, warn=-1)

#' CorrelationMatrix.
#'
#' @param ticker Yahoo Finance target ticker
#' @param correlation cutoff - used to determine if the other stock should be added to a basket of correlated stocks
#' @param start Start date
#' @param end End date
#' @param market.cap Percentage difference in market cap for similar stocks (or FALSE if not used)
#' @param sector Boolean to represent if stocks should be restricted to same sector
#' @param industry Boolean to represent if stocks should be restricted to same industry
#' @return List of covariance matrix, correlation matrix, and basket of highly correlated stocks
#' @keywords regression, components
#' @importFrom quantmod getSymbols dailyReturn
#' @export
#' @examples
#' CorrelationMatrix("IBM", 0.5, as.Date('2015-01-01'), as.Date('2015-05-25'))
CorrelationMatrix <- function(ticker, cutoff = .8, start = Sys.Date() - 365, end = Sys.Date(), market.cap = 0.1, sector = TRUE, industry = FALSE)
{
  #pick similar stocks to ticker
  similar_stocks <- SimilarStocks(ticker, market.cap, sector, industry)$Symbol

  #Get daily returns each stock
  stock_prices <- data.frame() 
  stock_returns <- data.frame() 
  
  for(stock in similar_stocks)
  {
    prices <- data.frame(getSymbols(stock, from = start, to = end, auto.assign = F, warnings = FALSE))
    prices$Date <- rownames(prices)
    stock_prices <-  if(nrow(stock_prices) == 0) prices[,c(4,7)] else merge(stock_prices, prices[,c(4,7)], by = "Date")
  }
  closes <- stock_prices[,colnames(stock_prices) != 'Date']
  if(class(closes) == "data.frame")
    stock_returns <- data.frame(t(matrix(unlist(lapply(closes, function(x) diff(x) / head(x, -1))), nrow=ncol(closes), byrow=F)))
  else
    stock_returns <- data.frame(diff(closes) / head(closes, -1))
  
  colnames(stock_returns) <- similar_stocks
  correlations <- cor(stock_returns)
  basket <- colnames(correlations)[correlations[,ticker] >= abs(cutoff)]
  basket <- basket[basket != ticker]

  
  #Create the returning list
  result <- list()
  result$prices <- stock_prices
  result$cov_matrix <- cov(stock_returns)
  result$cor_matrix <- correlations
  result$highly_corr_stocks <- basket
  return(result)
}
