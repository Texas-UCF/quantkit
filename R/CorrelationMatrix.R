options("getSymbols.warning4.0"=FALSE, warn=-1)

#' CorrelationMatrix.
#'
#' @param ticker Yahoo Finance target ticker
#' @param correlation cutoff - used to determine if the other stock should be added to a basket of correlated stocks
#' @param start Start date
#' @param end End date
#' @return List of covariance matrix, correlation matrix, and basket of highly correlated stocks
#' @keywords regression, components
#' @importFrom quantmod getSymbols dailyReturn
#' @export
#' @examples
#' CorrelationMatrix("IBM", 0.5, as.Date('2015-01-01'), as.Date('2015-05-25'))
CorrelationMatrix <- function(ticker, cutoff = .5, start = Sys.Date() - 365, end = Sys.Date())
{
  #pick similar stocks to ticker
  similar_stocks <- SimilarStocks(ticker)$Symbol

  #Get daily returns each stock
  stock_returns <- data.frame(dailyReturn(getSymbols(similar_stocks[1], from = start, to = end, auto.assign = F, warnings = FALSE)))
  for(stock in similar_stocks[-1])
  {
    prices <- getSymbols(stock, from = start, to = end, auto.assign = F, warnings = FALSE)
    rets <- dailyReturn(prices)
    stock_returns <- merge(stock_returns, data.frame(rets), by=0)
    rownames(stock_returns) <- stock_returns[,1]
    stock_returns <- stock_returns[,-1]
  }

  #Create an empty correlation and covariance matrices
  colnames(stock_returns) <- similar_stocks
  dim <- length(colnames(stock_returns))

  covariance_matrix <- matrix(rep(0, dim **2), ncol = dim, nrow = dim)
  correlation_matrix <- matrix(rep(0, dim ** 2), ncol = dim, nrow = dim)

  dimnames(covariance_matrix) <- list(similar_stocks, similar_stocks)
  dimnames(correlation_matrix) <- list(similar_stocks, similar_stocks)
  ncol_ticker <- match(ticker, similar_stocks)

  #Populate the correlation and covariance matrices with the appropriate values and select
  #a basket of highly correlated stocks
  highly_corr_stocks <- vector()
  for(i in 1:nrow(covariance_matrix))
  {
    for(j in i:nrow(covariance_matrix))
    {
      cov <- cov(stock_returns[,i], stock_returns[,j])
      corr <- cor(stock_returns[,i], stock_returns[,j])
      covariance_matrix[i, j] <- cov
      covariance_matrix[j, i] <- cov
      correlation_matrix[i,j] <- corr
      correlation_matrix[j,i] <- corr
      if((i == ncol_ticker | j == ncol_ticker) & i != j & corr > cutoff)
        highly_corr_stocks <- c(highly_corr_stocks, ifelse(i==ncol_ticker, similar_stocks[j], similar_stocks[i]))
    }
  }

  #Create the returning list
  result <- list()
  result$cov_matrix <- covariance_matrix
  result$cor_matrix <- correlation_matrix
  result$highly_corr_stocks <- highly_corr_stocks
  return(result)
}
