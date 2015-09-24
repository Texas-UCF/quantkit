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
  
  all.data <- c()
  
  # Use quantmod to get all the stock prices
  for (stock in c(ticker, components)) {
    prices <- getSymbols(stock, from = start, to = end, auto.assign = F, warnings = F)
    rets <- dailyReturn(prices)
    colnames(rets) <- stock
    
    if (length(all.data) == 0) {
      all.data <- rets
    } else {
      all.data <- merge(all.data, rets)
    }
  }
  
  all.data <- all.data[complete.cases(all.data), ]
  
  # Run Regression
  regression <- summary(lm(all.data[, 1] ~ ., data = all.data[, -1]))
  
  # Remove variables where p value > cutoff
  init.coeff <- regression$coefficients
  sig.comp <- c(colnames(all.data)[1], 
                intersect(rownames(init.coeff)[init.coeff[,'Pr(>|t|)'] < cutoff], colnames(all.data)))
  
  # Re-Run Regression if variables were removed
  if(length(sig.comp) > 0) {
    sig.returns <- all.data[, sig.comp]
    sig.regression <- summary(lm(sig.returns[, 1] ~ ., data = sig.returns[, -1]))
    
    # Use regression results to filter out time series
    fin.coeff <- sig.regression$coefficients[, "Estimate"]
    y.hat <- rowSums(t(t(sig.returns[, -1]) * fin.coeff[2:length(fin.coeff)]))
    resid <- sig.returns[, 1] - y.hat
    result <- list()
    result$returns <- resid
    
    price <- 1
    resid[1] <- price
    for(i in 2:nrow(resid)){
      resid[i] <- price * as.numeric(resid[i]) + price
      price <- resid[i]
    }
    result$filtered.price <- resid
    result$regression <- sig.regression
    
    return(result)   
  }
}