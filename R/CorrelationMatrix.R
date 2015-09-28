options("getSymbols.warning4.0"=FALSE, warn=-1)

CorrelationMatrix <- function(ticker, correlation_cutoff = .5, start = Sys.Date() - 365, end = Sys.Date())
{
  ticker <- 'IBM'
  start <- Sys.Date() - 365
  end <- Sys.Date()

  similar_stocks <- SimilarStocks(ticker)$Symbol

  stock_returns <- data.frame(dailyReturn(getSymbols(similar_stocks[1], from = start, to = end, auto.assign = F, warnings = FALSE)))
  for(stock in similar_stocks[-1])
  {
    prices <- getSymbols(stock, from = start, to = end, auto.assign = F, warnings = FALSE)
    rets <- dailyReturn(prices)
    stock_returns <- merge(stock_returns, data.frame(rets), by=0)
    rownames(stock_returns) <- stock_returns[,1]
    stock_returns <- stock_returns[,-1]
  }

  colnames(stock_returns) <- similar_stocks
  dim <- length(colnames(stock_returns))

  covariance_matrix <- matrix(rep(0, dim **2), ncol = dim, nrow = dim)
  correlation_matrix <- matrix(rep(0, dim ** 2), ncol = dim, nrow = dim)

  dimnames(covariance_matrix) <- list(similar_stocks, similar_stocks)
  dimnames(correlation_matrix) <- list(similar_stocks, similar_stocks)
  for(i in 1:nrow(covariance_matrix))
  {
    for(j in i:nrow(covariance_matrix))
    {
      cov <- cor(stock_returns[,i], stock_returns[,j])
      corr <- cor(stock_returns[,i], stock_returns[,j])
      covariance_matrix[i, j] <- cov
      covariance_matrix[j, i] <- cov
      correlation_matrix[i,j] <- corr
      correlation_matrix[j,i] <- corr
    }
  }


 return(list(covariance_matrix, correlation_matrix))
}
