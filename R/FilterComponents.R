#' Filter Components.
#'
#' @param ticker Yahoo Finance target ticker
#' @param components Vector of Yahoo Finance tickers to be filtered out
#' @param cutoff If p-value > cutoff, exclude the component
#' @param start Start date
#' @param end End date
#' @return List of filtered out time series, returns, and regression results
#' @keywords regression, components
#' @importFrom quantmod getSymbols dailyReturn
#' @export
#' @examples
#' FilterComponents("XOM", c("^GSPC", "USO"), 0.05, '2015-01-01', '2015-05-25')

FilterComponents <- function(ticker, components = "^GSPC", cutoff = 0.05, start = Sys.Date() - 365, end = Sys.Date()) {
  
  all.data <- NA
  
  # Use quantmod to get all the stock prices
  for (stock in c(ticker, components)) {
    
    prices <- getSymbols(stock, from = start, to = end, auto.assign = F, warnings = F)
    rets <- dailyReturn(prices)
    colnames(rets) <- stock
    
    if (is.na(all.data)) {
      all.data <- rets
    } else {
      all.data <- merge(all.data, rets)
    }
  }
  
  # Run Regression
  regression <- summary(lm(all.data[, 1] ~ ., data = all.data[, -1]))
  
  # Remove variables where p value > cutoff
  initCoeff <- regression$coefficients
  sigComponents <- intersect(rownames(initCoeff)[initCoeff[,'Pr(>|t|)'] < cutoff], names(returnData))
  
  # Re-Run Regression if variables were removed
  if(length(sigComponents) > 0){
    sigReturns <- returnData[,sigComponents]
    sigRegression <- summary(lm(tickerReturns ~ ., data=sigReturns))
    
    # Use regression results to filter out time series
    finCoeff <- sigRegression$coefficients[,"Estimate"]
    y_hat <- rowSums(t(t(sigReturns) * finCoeff[2:length(finCoeff)]))
    resid <- tickerReturns - y_hat
    result <- list()
    result$returns <- resid
    
    price <- 1
    resid[1] <- price
    for(i in 2:nrow(resid)){
      resid[i] <- price * as.numeric(resid[i]) + price
      price <- resid[i]
    }
    result$filtered.price <- resid
    result$regression <- sigRegression
    
    return(result)   
  }
}